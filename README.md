# 🍋 Yuzu

**Multi-language API monitoring framework.** Each component is written in the language best suited for its role — no compromises.

[![C](https://img.shields.io/badge/C-API_Core-00599C?style=flat&logo=c)](c/server.c)
[![C++](https://img.shields.io/badge/C++-Dashboard-00599C?style=flat&logo=cplusplus)](cpp/dashboard.cpp)
[![COBOL](https://img.shields.io/badge/COBOL-Metrics-005CA5?style=flat)](cobol/metrics.cob)
[![Swift](https://img.shields.io/badge/Swift-HTTPS-F05138?style=flat&logo=swift)](swift/Sources/yuzu/main.swift)
[![Rust](https://img.shields.io/badge/Rust-CLI-000000?style=flat&logo=rust)](rust/src/main.rs)
[![Assembly](https://img.shields.io/badge/Assembly-TUI_×6-4B0082?style=flat)](asm/)
[![License: MIT](https://img.shields.io/badge/License-MIT-pink.svg)](LICENSE)

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Yuzu Stack                              │
├─────────────┬──────────────────────────────────────────────┤
│  Swift      │  HTTPS layer, connection management           │
│  C          │  HTTP API core, routing, threading            │
│  COBOL      │  Metrics processing, business rules           │
│  C++        │  Web dashboard (auto-refresh)                 │
│  Rust       │  CLI — watch, ping, get, post                 │
│  Assembly   │  TUI manager (6 architectures)                │
│  Brainfuck  │  Visual byte-level display                    │
└─────────────┴──────────────────────────────────────────────┘
```

## Why each language

| Language | Role | Why |
|----------|------|-----|
| **C** | API core | Zero-overhead, pthreads, raw syscalls |
| **C++** | Web dashboard | STL, threads, HTML generation |
| **COBOL** | Metrics | Designed for structured data processing and business rules |
| **Swift** | HTTPS layer | Best-in-class networking on Apple/ARM platforms |
| **Rust** | CLI | Memory safe, expressive, great for tooling |
| **Assembly** | TUI | Direct syscalls, zero dependencies, full control |
| **Brainfuck** | Visual output | Byte-level display, absurd but functional |

---

## Endpoints (C API)

| Method | Path | Description |
|--------|------|-------------|
| GET | `/` | Health check |
| GET | `/health` | Health + version + uptime |
| GET | `/ping` | Latency probe |
| GET | `/metrics` | Full metrics JSON |
| GET | `/routes` | List registered routes |
| GET/POST | `/echo` | Request mirror |

---

## Assembly TUI targets

| File | Architecture | Linker | ABI |
|------|-------------|--------|-----|
| `yuzu-tui-x86_64.s` | x86_64 Linux | `/lib64/ld-linux-x86-64.so.2` | System V AMD64 |
| `yuzu-tui-x86.s` | i686 Linux | `/lib/ld-linux.so.2` | cdecl |
| `yuzu-tui-arm64-linux.s` | ARM64 Linux | `/lib/ld-linux-aarch64.so.1` | AAPCS64/glibc |
| `yuzu-tui-arm64-android.s` | ARM64 Android | `/system/bin/linker64` | AAPCS64/Bionic |
| `yuzu-tui-arm32-linux.s` | ARM32 Linux | `/lib/ld-linux-armhf.so.3` | AAPCS/glibc |
| `yuzu-tui-arm32-android.s` | ARM32 Android | `/system/bin/linker` | AAPCS/Bionic |

**Critical difference — Bionic vs glibc `ai_addr` offset:**

| libc | Architecture | `ai_addr` offset |
|------|-------------|-----------------|
| glibc | ARM64 | 24 |
| Bionic | ARM64 | **32** ← |
| glibc | ARM32 | 20 |
| Bionic | ARM32 | 20 |

---

## Quick start

### Local (no Docker)

```bash
# Compilar todo
make all

# Arrancar API + Dashboard
make run

# En otra terminal — TUI Assembly (tu arquitectura)
make asm-x86_64        # o asm-arm64-linux, etc.
./asm/yuzu-tui-x86_64

# CLI Rust
./rust/target/release/yuzu health
./rust/target/release/yuzu metrics
./rust/target/release/yuzu watch
```

### Docker Compose

```bash
make docker
# API:       http://localhost:8080
# Dashboard: http://localhost:3000
# Proxy:     http://localhost:80

make docker-logs
make docker-down
```

### Termux (ARM64 Android)

```bash
# Instalar dependencias
pkg install clang gnucobol

# Compilar
make c cpp cobol
make asm-arm64-android

# Arrancar
./c/yuzu-api &
./cpp/yuzu-dashboard &
./asm/yuzu-tui-arm64-android
```

---

## Rust CLI

```bash
yuzu ping
yuzu health
yuzu metrics
yuzu routes
yuzu watch          # live monitor, actualiza cada 2s
yuzu watch 5        # cada 5s
yuzu get /metrics
yuzu post /echo '{"test":true}'
yuzu --host api.example.com --port 443 health
```

---

## COBOL metrics

COBOL recibe el JSON de `/metrics` y calcula:

- **Success rate** — porcentaje de requests exitosos
- **Error rate** — porcentaje de errores
- **Requests/min** — throughput normalizado
- **Uptime** — en segundos, minutos, horas y días
- **Alertas** — HIGH_LAT, HIGH_ERR, LOW_REQ

Output para integración con C++/Assembly:

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
```

---

## Brainfuck

`yuzu-viz.bf` recibe el output de COBOL via stdin e imprime un borde visual alrededor:

```bash
./cobol/yuzu-metrics "$DATA" | ./bf/bf bf/yuzu-viz.bf
```

---

## Project structure

```
yuzu/
├── c/
│   ├── server.c          C API core
│   └── Dockerfile
├── cpp/
│   ├── dashboard.cpp     Web dashboard
│   └── Dockerfile
├── cobol/
│   ├── metrics.cob       Metrics processor
│   ├── entrypoint.sh     Docker poller
│   └── Dockerfile
├── swift/
│   ├── Package.swift
│   └── Sources/yuzu/
│       └── main.swift    HTTPS layer
├── rust/
│   ├── Cargo.toml
│   └── src/main.rs       CLI
├── asm/
│   ├── yuzu-tui-x86_64.s
│   ├── yuzu-tui-x86.s
│   ├── yuzu-tui-arm64-linux.s
│   ├── yuzu-tui-arm64-android.s
│   ├── yuzu-tui-arm32-linux.s
│   └── yuzu-tui-arm32-android.s
├── bf/
│   └── yuzu-viz.bf       Brainfuck visualizer
├── docker/
│   └── nginx.conf
├── docker-compose.yml
├── Makefile
└── README.md
```

---

## License

MIT — 2025 Yuzu Project
