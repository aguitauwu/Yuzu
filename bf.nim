# bf.nim - Brainfuck interpreter + transpiler to Nim
# Usage:
#   nim c -d:release bf.nim
#   ./bf [--compile=out.nim] [--tape-size=N] [--cell-bits=8|16|32] [--grow] [--debug] program.bf
#
import os, strutils, sequtils, tables

const VERSION = "1.0.0"

type
  Pos = tuple[line: int, col: int]

proc usage() =
  stdout.write """bf - Brainfuck interpreter + Nim transpiler
Usage:
  bf [options] program.bf
Options:
  --help                Show this message
  --version             Show version
  --compile=FILE        Emit Nim source that implements the BF program
  --tape-size=N         Tape length (default 30000)
  --cell-bits=B         Cell size in bits: 8,16,32 (default 8)
  --grow                Allow tape to auto-grow instead of erroring on overflow
  --debug               Print debug info (pc/ptr) while running
"""

# --- simple argv parsing ---
proc getArg(prefix: string): string =
  for a in commandLineParams():
    if a.startsWith(prefix & "="):
      return a.split("=")[1]
  return ""

proc hasFlag(name: string): bool =
  for a in commandLineParams():
    if a == name: return true
  return false

if hasFlag("--help"):
  usage()
  quit(0)

if hasFlag("--version"):
  echo "bf nim interpreter/transpiler ", VERSION
  quit(0)

let compileTo = getArg("--compile")
let tapeSizeArg = getArg("--tape-size")
let cellBitsArg = getArg("--cell-bits")
let growFlag = hasFlag("--grow")
let debugFlag = hasFlag("--debug")

var tapeSize = 30000
if tapeSizeArg.len > 0:
  try:
    tapeSize = parseInt(tapeSizeArg)
    if tapeSize <= 0:
      stderr.write "Invalid --tape-size: must be > 0\n"; quit(2)
  except:
    stderr.write "Invalid --tape-size value\n"; quit(2)

var cellBits = 8
if cellBitsArg.len > 0:
  try:
    cellBits = parseInt(cellBitsArg)
    if cellBits notin @[8,16,32]:
      stderr.write "Invalid --cell-bits: only 8,16,32 supported\n"; quit(2)
  except:
    stderr.write "Invalid --cell-bits value\n"; quit(2)

let modBase = 1 shl cellBits
let cellMask = modBase - 1

# --- read file / stdin ---
if paramCount() == 0:
  stderr.write "No program specified. Use --help for usage.\n"; quit(2)

let programPath = paramStr(1)
var raw = ""
try:
  raw = readFile(programPath)
except OSError:
  stderr.write &"Cannot open program file: {programPath}\n"
  quit(2)

# --- filter Brainfuck commands and keep position mapping for errors ---
var codeChars = newSeq[char]()
var posMap = newSeq[Pos]()     # index -> (line,col)
var line = 1
var col = 1
for ch in raw:
  if ch == '\n':
    line.inc; col = 1
    continue
  if ch in {'>', '<', '+', '-', '.', ',', '[', ']'}:
    codeChars.add(ch)
    posMap.add((line: line, col: col))
  col.inc

let codeLen = codeChars.len
if codeLen == 0:
  stderr.write "Program contains no Brainfuck instructions.\n"; quit(2)

# --- precompute bracket pairs with error reporting ---
var jump = initTable[int,int]()   # maps index -> matching index
var st: seq[int] = @[]
for i in 0..<codeLen:
  let c = codeChars[i]
  if c == '[':
    st.add(i)
  elif c == ']':
    if st.len == 0:
      let p = posMap[i]
      stderr.write &"Unmatched ']' at {programPath}:{p.line}:{p.col}\n"
      quit(3)
    let j = st.pop()
    jump[i] = j
    jump[j] = i

if st.len > 0:
  let i = st.pop()
  let p = posMap[i]
  stderr.write &"Unmatched '[' at {programPath}:{p.line}:{p.col}\n"
  quit(3)

# --- optional simple optimizer: run-length encode consecutive ops ---
type OpKind = enum
  okIncPtr, okDecPtr, okIncCell, okDecCell, okOut, okIn, okLoopStart, okLoopEnd, okOther

type Instr = object
  kind: OpKind
  count: int
  idx: int  # original index (for error reporting)

proc optimizeRunLength(chars: seq[char]): seq[Instr] =
  var res: seq[Instr] = @[]
  var i = 0
  while i < chars.len:
    let c = chars[i]
    if c in {'>', '<', '+', '-'}:
      var cnt = 1
      var j = i+1
      while j < chars.len and chars[j] == c:
        cnt.inc; j.inc
      var k: OpKind
      case c
      of '>': k = okIncPtr
      of '<': k = okDecPtr
      of '+': k = okIncCell
      of '-': k = okDecCell
      else: k = okOther
      res.add(Instr(kind: k, count: cnt, idx: i))
      i = j
    else:
      var k: OpKind
      case c
      of '.': k = okOut
      of ',': k = okIn
      of '[': k = okLoopStart
      of ']': k = okLoopEnd
      else: k = okOther
      res.add(Instr(kind: k, count: 1, idx: i))
      i.inc
  result = res

let instrs = optimizeRunLength(codeChars)

# --- Interpreter ---
proc interpret() =
  var tape = newSeq[int](tapeSize)
  for i in 0..<tape.len: tape[i] = 0
  var ptr = 0
  var pc = 0
  var instCount = 0

  while pc < instrs.len:
    let ins = instrs[pc]
    case ins.kind
    of okIncPtr:
      ptr += ins.count
      if ptr >= tape.len:
        if growFlag:
          let need = ptr - tape.len + 1
          for _ in 0..<need: tape.add(0)
        else:
          let p = posMap[ins.idx]
          stderr.write &"Pointer overflow at {programPath}:{p.line}:{p.col} (ptr={ptr}, tape-size={tape.len})\n"
          quit(4)
    of okDecPtr:
      ptr -= ins.count
      if ptr < 0:
        let p = posMap[ins.idx]
        stderr.write &"Pointer underflow at {programPath}:{p.line}:{p.col} (ptr={ptr})\n"
        quit(5)
    of okIncCell:
      tape[ptr] = ((tape[ptr] + ins.count) mod modBase + modBase) mod modBase
    of okDecCell:
      tape[ptr] = ((tape[ptr] - ins.count) mod modBase + modBase) mod modBase
    of okOut:
      # output raw byte(s)
      stdout.write($chr(tape[ptr]))
    of okIn:
      # read one char, if EOF -> 0
      var c: char = '\0'
      try:
        if stdin.atEnd():
          c = '\0'
        else:
          c = stdin.readChar()
      except:
        c = '\0'
      tape[ptr] = ord(c) mod modBase
    of okLoopStart:
      # find matching bracket index in terms of instrs[] indexes.
      # We have jump map in terms of original code indices - need to map.
      # We'll find the corresponding code index and jump by pc index scanning.
      if tape[ptr] == 0:
        # jump forward: find the matching ']' original index and move pc to that instruction
        let origIdx = instrs[pc].idx
        var targetOrig = jump.getOrDefault(origIdx, -1)
        if targetOrig == -1:
          let p = posMap[origIdx]
          stderr.write &"Internal error: missing jump mapping at {programPath}:{p.line}:{p.col}\n"; quit(6)
        # find pc' such that instrs[pc'].idx == targetOrig
        var found = -1
        var s = pc + 1
        while s < instrs.len:
          if instrs[s].idx == targetOrig:
            found = s; break
          s.inc
        if found == -1:
          # fallback: linear scan on posMap to find code index -> compute corresponding instruction index
          var targetIdx = -1
          for k in 0..<instrs.len:
            if instrs[k].idx == targetOrig:
              targetIdx = k; break
          if targetIdx == -1:
            stderr.write "Internal error: cannot map jump target\n"; quit(7)
          pc = targetIdx
        else:
          pc = found
      # else continue
    of okLoopEnd:
      if tape[ptr] != 0:
        # jump back to matching '[' original index
        let origIdx = instrs[pc].idx
        var targetOrig = jump.getOrDefault(origIdx, -1)
        if targetOrig == -1:
          let p = posMap[origIdx]
          stderr.write &"Internal error: missing jump mapping at {programPath}:{p.line}:{p.col}\n"; quit(6)
        var found = -1
        var s = pc - 1
        while s >= 0:
          if instrs[s].idx == targetOrig:
            found = s; break
          s.dec
        if found == -1:
          var targetIdx = -1
          for k in 0..<instrs.len:
            if instrs[k].idx == targetOrig:
              targetIdx = k; break
          if targetIdx == -1:
            stderr.write "Internal error: cannot map jump target\n"; quit(7)
          pc = targetIdx
        else:
          pc = found
      # else continue
    else:
      discard

    instCount.inc
    if debugFlag and (instCount mod 1000 == 0):
      stdout.write &"\n[debug] pc={pc} ptr={ptr}\n"
    pc.inc

# --- Transpiler: produce Nim source that directly implements BF with nested while loops ---
proc compileToNimSource(code: seq[char], filePath: string, tsz: int, bits: int): string =
  var out = newStringBuilder()
  out.add("""# Generated by bf.nim transpiler
# Source: """ & filePath & """
import os\n
const
  TAPE_SIZE = """ & $tsz & """
  MOD_BASE = """ & $((1 shl bits)) & """

proc main() =
  var tape: array[TAPE_SIZE, int]
  for i in 0..TAPE_SIZE-1: tape[i] = 0
  var ptr = 0
""")
  var indent = 2
  proc addLine(s: string) =
    out.add(repeat(' ', indent*2) & s & "\n")

  # simple optimizer: compress repeated ops into expressions
  var i = 0
  while i < code.len:
    let c = code[i]
    if c in {'>', '<', '+', '-'}:
      var cnt = 1
      var j = i+1
      while j < code.len and code[j] == c:
        cnt.inc; j.inc
      case c
      of '>': addLine("ptr += " & $cnt)
      of '<': addLine("ptr -= " & $cnt)
      of '+': addLine("tape[ptr] = (tape[ptr] + " & $cnt & ") mod MOD_BASE")
      of '-': addLine("tape[ptr] = (tape[ptr] - " & $cnt & " + MOD_BASE) mod MOD_BASE")
      i = j; continue
    elif c == '.':
      addLine("stdout.write($chr(tape[ptr]))")
    elif c == ',':
      addLine("var c: char")
      addLine("if stdin.atEnd(): c = '\\0' else: c = stdin.readChar()")
      addLine("tape[ptr] = ord(c) mod MOD_BASE")
    elif c == '[':
      addLine("while tape[ptr] != 0:")
      indent.inc
    elif c == ']':
      indent.dec
      if indent < 0: indent = 0
    else:
      discard
    i.inc

  out.add("\nwhen isMainModule:\n  main()\n")
  return out.toString()

# --- Main flow ---
if compileTo.len > 0:
  # generate Nim source
  let nimSource = compileToNimSource(codeChars, programPath, tapeSize, cellBits)
  try:
    writeFile(compileTo, nimSource)
    echo "Wrote generated Nim to ", compileTo
    quit(0)
  except OSError:
    stderr.write "Cannot write output file: " & compileTo & "\n"; quit(8)
else:
  interpret()