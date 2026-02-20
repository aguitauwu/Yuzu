<div align="center">

<br>

<img src="https://img.shields.io/badge/%E2%9C%A6-YUZU-000000?style=for-the-badge&labelColor=000000" alt="Yuzu" height="50">

<br><br>

# Multi-Language API Monitoring Framework

**Seven languages. Each chosen for a reason. No compromises.**<br>
**C core, COBOL metrics, Assembly TUI, Rust CLI, Swift HTTPS, C++ dashboard, Brainfuck output.**

<br>

<a href="#architecture"><img src="https://img.shields.io/badge/ARCHITECTURE-000000?style=for-the-badge" alt="Architecture"></a>
&nbsp;&nbsp;
<a href="#quick-start"><img src="https://img.shields.io/badge/QUICK_START-000000?style=for-the-badge" alt="Quick Start"></a>
&nbsp;&nbsp;
<a href="https://github.com/aguitauwu/Yuzu"><img src="https://img.shields.io/badge/REPOSITORY-000000?style=for-the-badge" alt="Repository"></a>

<br><br>

[![License](https://img.shields.io/badge/MIT-222222?style=flat-square&logo=opensourceinitiative&logoColor=white)](LICENSE)
&nbsp;
[![C](https://img.shields.io/badge/C-222222?style=flat-square&logo=c&logoColor=white)](c/server.c)
&nbsp;
[![C++](https://img.shields.io/badge/C++-222222?style=flat-square&logo=cplusplus&logoColor=white)](cpp/dashboard.cpp)
&nbsp;
[![Rust](https://img.shields.io/badge/Rust-222222?style=flat-square&logo=rust&logoColor=white)](rust/src/main.rs)
&nbsp;
[![Swift](https://img.shields.io/badge/Swift-222222?style=flat-square&logo=swift&logoColor=white)](swift/Sources/yuzu/main.swift)
&nbsp;
[![COBOL](https://img.shields.io/badge/COBOL-222222?style=flat-square&logoColor=white)](cobol/metrics.cob)

<br>

---

<br>

<table>
<tr>
<td width="50%" valign="top">

**Full monitoring stack.**<br><br>
HTTP API with routing and threading.<br>
Real-time web dashboard with auto-refresh.<br>
Metrics processing with business rules.<br>
HTTPS connection management layer.<br>
CLI with watch, ping, get, and post.<br>
TUI for six hardware architectures.<br>
Visual byte-level output display.

</td>
<td width="50%" valign="top">

**Zero compromises on language choice.**<br><br>
C for raw syscall performance.<br>
COBOL for structured data processing.<br>
Assembly for direct hardware control.<br>
Rust for memory-safe tooling.<br>
Swift for best-in-class ARM networking.<br>
<br>
Runs on Linux, macOS, and Android (Termux).

</td>
</tr>
</table>

<br>

</div>

---

<br>

<div align="center">

## What is Yuzu?

</div>

<br>

**Yuzu** is a multi-language API monitoring framework where each component is implemented in the language best suited for its technical role. The project is not an exercise in language diversity for its own sake -- each choice reflects a specific technical justification: C for zero-overhead networking and threading, COBOL for its native design around structured business-rule data processing, Assembly for direct syscall access with no runtime dependencies across six hardware targets, Rust for the expressive and memory-safe CLI tooling, Swift for first-class ARM networking, C++ for STL-driven HTML dashboard generation, and Brainfuck as a byte-level visual output layer interpreted by a Nim compiler.

The stack runs in full on an Android device under Termux, including the COBOL metrics processor and all six Assembly TUI targets.

<br>

---

<br>

<div align="center">

## Architecture

</div>

<br>

```
+-------------------------------------------------------------+
|                        Yuzu Stack                           |
+------------------+------------------------------------------+
|  Swift           |  HTTPS layer, connection management      |
|  C               |  HTTP API core, routing, threading       |
|  COBOL           |  Metrics processing, business rules      |
|  C++             |  Web dashboard (auto-refresh)            |
|  Rust            |  CLI -- watch, ping, get, post           |
|  Assembly        |  TUI manager (6 architectures)           |
|  Brainfuck + Nim |  Visual byte-level display               |
+------------------+------------------------------------------+
```

<br>

**Data flow:**

```
  C API (/metrics JSON)
       |
       v
  COBOL processor
       |
       v
  Nim interpreter
       |
       v
  Brainfuck visualizer --> stdout
```

<br>

---

<br>

<div align="center">

## Language Roles

</div>

<br>

| Language | Component | Justification |
|:---------|:----------|:--------------|
| **C** | API core | Zero-overhead, pthreads, raw syscalls |
| **C++** | Web dashboard | STL, threads, HTML generation |
| **COBOL** | Metrics processor | Designed for structured data and business rules |
| **Swift** | HTTPS layer | Best-in-class networking on Apple/ARM platforms |
| **Rust** | CLI | Memory safe, expressive, great for tooling |
| **Assembly** | TUI | Direct syscalls, zero dependencies, full hardware control |
| **Brainfuck** | Visual output | Byte-level display |
| **Nim** | BF interpreter | Compiles to C, interprets the Brainfuck visualizer |

<br>

---

<br>

<div align="center">

## Endpoints

</div>

<br>

| Method | Path | Description |
|:-------|:-----|:------------|
| GET | `/` | Health check |
| GET | `/health` | Health status, version, uptime |
| GET | `/ping` | Latency probe |
| GET | `/metrics` | Full metrics JSON |
| GET | `/routes` | List of registered routes |
| GET / POST | `/echo` | Request mirror |

<br>

---

<br>

<div align="center">

## Assembly TUI Targets

</div>

<br>

| File | Architecture | Linker | ABI |
|:-----|:-------------|:-------|:----|
| `yuzu-tui-x86_64.s` | x86_64 Linux | `/lib64/ld-linux-x86-64.so.2` | System V AMD64 |
| `yuzu-tui-x86.s` | i686 Linux | `/lib/ld-linux.so.2` | cdecl |
| `yuzu-tui-arm64-linux.s` | ARM64 Linux | `/lib/ld-linux-aarch64.so.1` | AAPCS64/glibc |
| `yuzu-tui-arm64-android.s` | ARM64 Android | `/system/bin/linker64` | AAPCS64/Bionic |
| `yuzu-tui-arm32-linux.s` | ARM32 Linux | `/lib/ld-linux-armhf.so.3` | AAPCS/glibc |
| `yuzu-tui-arm32-android.s` | ARM32 Android | `/system/bin/linker` | AAPCS/Bionic |

<br>

**Bionic vs glibc -- critical `ai_addr` offset difference:**

| libc | Architecture | `ai_addr` offset |
|:-----|:-------------|:-----------------|
| glibc | ARM64 | 24 |
| Bionic | ARM64 | **32** |
| glibc | ARM32 | 20 |
| Bionic | ARM32 | 20 |

<br>

---

<br>

<div align="center">

## COBOL Metrics

</div>

<br>

The COBOL processor receives the JSON response from `/metrics` and computes derived metrics and alert conditions. Output is formatted as key-value pairs for consumption by the C++ dashboard and Assembly TUI.

<br>

<table>
<tr>
<td width="50%" valign="top">

**Computed metrics**

- Success rate -- percentage of successful requests
- Error rate -- percentage of failed requests
- Requests/min -- throughput normalized to 60 seconds
- Uptime -- expressed in seconds, minutes, hours, and days

</td>
<td width="50%" valign="top">

**Alert conditions**

- `HIGH_LAT` -- average latency above 100ms
- `HIGH_ERR` -- error rate above 5%
- `LOW_REQ` -- zero requests with uptime above 60 seconds

</td>
</tr>
</table>

<br>

**Sample output:**

```
STATUS:OK
TOTAL_REQ:1042
OK_REQ:1040
ERR_REQ:2
SUCCESS_RATE:99.81
ERROR_RATE:0.19
AVG_LAT:0.31
REQ_PER_MIN:124.50
UPTIME_HOURS:2
ALERT_HIGH_LAT:N
ALERT_HIGH_ERR:N
ALERT_LOW_REQ:N
```

<br>

---

<br>

<div align="center">

## Quick Start

</div>

<br>

### Local

```bash
# Build everything
make all

# Start API and dashboard
make run

# In a second terminal -- TUI (select your architecture)
make asm-x86_64
./asm/yuzu-tui-x86_64

# Rust CLI
./rust/target/release/yuzu health
./rust/target/release/yuzu metrics
./rust/target/release/yuzu watch
```

<br>

### Docker Compose

```bash
make docker
# API:       http://localhost:8080
# Dashboard: http://localhost:3000
# Proxy:     http://localhost:80

make docker-logs
make docker-down
```

<br>

### Termux (ARM64 Android)

```bash
# Install dependencies
pkg install clang gnucobol

# Build
make c cpp cobol
make asm-arm64-android

# Start
./c/yuzu-api &
./cpp/yuzu-dashboard &
./asm/yuzu-tui-arm64-android
```

<br>

---

<br>

<div align="center">

## Rust CLI

</div>

<br>

```bash
yuzu ping
yuzu health
yuzu metrics
yuzu routes
yuzu watch            # live monitor, refreshes every 2s
yuzu watch 5          # refresh every 5s
yuzu get /metrics
yuzu post /echo '{"test":true}'
yuzu --host api.example.com --port 443 health
```

<br>

---

<br>

<div align="center">

## Brainfuck

</div>

<br>

`yuzu-viz.bf` reads COBOL output via stdin and renders a visual border around it. The interpreter is written in Nim and compiles to a native binary.

```bash
./cobol/yuzu-metrics "$DATA" | ./bf/bf bf/yuzu-viz.bf
```

<br>

---

<br>

<div align="center">

## Project Structure

</div>

<br>

```
yuzu/
├── c/
│   ├── server.c                C API core
│   └── Dockerfile
├── cpp/
│   ├── dashboard.cpp           Web dashboard
│   └── Dockerfile
├── cobol/
│   ├── metrics.cob             Metrics processor
│   ├── entrypoint.sh           Docker poller
│   └── Dockerfile
├── swift/
│   ├── Package.swift
│   └── Sources/yuzu/
│       └── main.swift          HTTPS layer
├── rust/
│   ├── Cargo.toml
│   └── src/main.rs             CLI
├── asm/
│   ├── yuzu-tui-x86_64.s
│   ├── yuzu-tui-x86.s
│   ├── yuzu-tui-arm64-linux.s
│   ├── yuzu-tui-arm64-android.s
│   ├── yuzu-tui-arm32-linux.s
│   └── yuzu-tui-arm32-android.s
├── bf/
│   └── yuzu-viz.bf             Brainfuck visualizer
├── bf.nim                      Brainfuck interpreter
├── docker/
│   └── nginx.conf
├── docker-compose.yml
├── Makefile
└── README.md
```

<br>

---

<br>

<div align="center">

## License

</div>

<br>

```
MIT License

Copyright (c) 2026 Yuzu Project

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

<br>

---

<br>

<div align="center">

**Seven languages. One monitor. Zero compromises.**

<br>

[![Yuzu](https://img.shields.io/badge/Yuzu-2026-000000?style=for-the-badge)](https://github.com/aguitauwu/Yuzu)

<br>

</div>
