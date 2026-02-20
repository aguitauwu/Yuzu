# ============================================================
# Yuzu — Master Makefile
# make all          → compilar todo para el sistema actual
# make asm-all      → compilar todos los targets de assembly
# make docker       → levantar con docker compose
# make clean        → limpiar binarios
# ============================================================

.PHONY: all c cpp cobol rust asm-all asm-x86_64 asm-x86 \
        asm-arm64-linux asm-arm64-android \
        asm-arm32-linux asm-arm32-android \
        docker docker-down clean help

CC      = gcc
CXX     = g++
COBC    = cobc
CARGO   = cargo
AS      = as
LD      = ld

# ============================================================
# Detección de arquitectura
# ============================================================
ARCH := $(shell uname -m)

all: c cpp cobol rust
	@echo ""
	@echo "  🍋 Yuzu compilado para $(ARCH)"
	@echo "  ✦ C API:       ./c/yuzu-api"
	@echo "  ✦ C++ Dashboard: ./cpp/yuzu-dashboard"
	@echo "  ✦ COBOL:       ./cobol/yuzu-metrics"
	@echo "  ✦ Rust CLI:    ./rust/target/release/yuzu"
	@echo ""
	@echo "  Para TUI assembly: make asm-$(ARCH)"
	@echo "  Para todo:         make asm-all"

# ============================================================
# C API
# ============================================================
c: c/yuzu-api

c/yuzu-api: c/server.c
	$(CC) -O2 -o c/yuzu-api c/server.c -lpthread
	@echo "✦ C API compilado → c/yuzu-api"

# ============================================================
# C++ Dashboard
# ============================================================
cpp: cpp/yuzu-dashboard

cpp/yuzu-dashboard: cpp/dashboard.cpp
	$(CXX) -O2 -std=c++17 -o cpp/yuzu-dashboard cpp/dashboard.cpp -lpthread
	@echo "✦ C++ Dashboard compilado → cpp/yuzu-dashboard"

# ============================================================
# COBOL
# ============================================================
cobol: cobol/yuzu-metrics

cobol/yuzu-metrics: cobol/metrics.cob
	$(COBC) -x -o cobol/yuzu-metrics cobol/metrics.cob
	@echo "✦ COBOL compilado → cobol/yuzu-metrics"

# ============================================================
# Rust CLI
# ============================================================
rust:
	cd rust && $(CARGO) build --release
	@echo "✦ Rust CLI compilado → rust/target/release/yuzu"

# ============================================================
# Assembly — x86_64
# ============================================================
asm-x86_64: asm/yuzu-tui-x86_64
asm/yuzu-tui-x86_64: asm/yuzu-tui-x86_64.s
	$(AS) -o asm/yuzu-tui-x86_64.o asm/yuzu-tui-x86_64.s
	$(LD) -dynamic-linker /lib64/ld-linux-x86-64.so.2 \
	      -o asm/yuzu-tui-x86_64 asm/yuzu-tui-x86_64.o -lc
	@echo "✦ Assembly x86_64 compilado → asm/yuzu-tui-x86_64"

# ============================================================
# Assembly — x86 (i686)
# ============================================================
asm-x86: asm/yuzu-tui-x86
asm/yuzu-tui-x86: asm/yuzu-tui-x86.s
	$(AS) --32 -o asm/yuzu-tui-x86.o asm/yuzu-tui-x86.s
	$(LD) -m elf_i386 -dynamic-linker /lib/ld-linux.so.2 \
	      -o asm/yuzu-tui-x86 asm/yuzu-tui-x86.o -lc
	@echo "✦ Assembly x86 (i686) compilado → asm/yuzu-tui-x86"

# ============================================================
# Assembly — ARM64 Linux
# ============================================================
asm-arm64-linux: asm/yuzu-tui-arm64-linux
asm/yuzu-tui-arm64-linux: asm/yuzu-tui-arm64-linux.s
	$(AS) -o asm/yuzu-tui-arm64-linux.o asm/yuzu-tui-arm64-linux.s
	$(LD) -dynamic-linker /lib/ld-linux-aarch64.so.1 \
	      -o asm/yuzu-tui-arm64-linux asm/yuzu-tui-arm64-linux.o -lc
	@echo "✦ Assembly ARM64 Linux compilado → asm/yuzu-tui-arm64-linux"

# ============================================================
# Assembly — ARM64 Android/Termux
# ============================================================
asm-arm64-android: asm/yuzu-tui-arm64-android
asm/yuzu-tui-arm64-android: asm/yuzu-tui-arm64-android.s
	$(AS) -o asm/yuzu-tui-arm64-android.o asm/yuzu-tui-arm64-android.s
	$(LD) --pie \
	      -dynamic-linker /system/bin/linker64 \
	      -rpath /data/data/com.termux/files/usr/lib \
	      -L /data/data/com.termux/files/usr/lib \
	      -o asm/yuzu-tui-arm64-android asm/yuzu-tui-arm64-android.o -lc
	@echo "✦ Assembly ARM64 Android compilado → asm/yuzu-tui-arm64-android"

# ============================================================
# Assembly — ARM32 Linux
# ============================================================
asm-arm32-linux: asm/yuzu-tui-arm32-linux
asm/yuzu-tui-arm32-linux: asm/yuzu-tui-arm32-linux.s
	$(AS) -march=armv7-a -o asm/yuzu-tui-arm32-linux.o asm/yuzu-tui-arm32-linux.s
	$(LD) -dynamic-linker /lib/ld-linux-armhf.so.3 \
	      -o asm/yuzu-tui-arm32-linux asm/yuzu-tui-arm32-linux.o -lc
	@echo "✦ Assembly ARM32 Linux compilado → asm/yuzu-tui-arm32-linux"

# ============================================================
# Assembly — ARM32 Android/Termux
# ============================================================
asm-arm32-android: asm/yuzu-tui-arm32-android
asm/yuzu-tui-arm32-android: asm/yuzu-tui-arm32-android.s
	$(AS) -march=armv7-a -o asm/yuzu-tui-arm32-android.o asm/yuzu-tui-arm32-android.s
	$(LD) --pie \
	      -dynamic-linker /system/bin/linker \
	      -rpath /data/data/com.termux/files/usr/lib \
	      -L /data/data/com.termux/files/usr/lib \
	      -o asm/yuzu-tui-arm32-android asm/yuzu-tui-arm32-android.o -lc
	@echo "✦ Assembly ARM32 Android compilado → asm/yuzu-tui-arm32-android"

# ============================================================
# Todos los assembly
# ============================================================
asm-all: asm-x86_64 asm-arm64-linux asm-arm64-android \
         asm-arm32-linux asm-arm32-android
	@echo "✦ Todos los targets assembly compilados"
	@echo "  Nota: x86 (i686) requiere toolchain 32bit: make asm-x86"

# ============================================================
# Docker
# ============================================================
docker:
	docker compose up -d --build
	@echo ""
	@echo "✦ Yuzu corriendo:"
	@echo "  API:       http://localhost:8080"
	@echo "  Dashboard: http://localhost:3000"
	@echo "  Proxy:     http://localhost:80"

docker-down:
	docker compose down

docker-logs:
	docker compose logs -f

# ============================================================
# Run local (sin Docker)
# ============================================================
run: c cpp cobol
	@echo "✦ Iniciando Yuzu API..."
	./c/yuzu-api &
	sleep 1
	@echo "✦ Iniciando Dashboard..."
	./cpp/yuzu-dashboard &
	@echo ""
	@echo "✦ Yuzu corriendo local:"
	@echo "  API:       http://localhost:8080"
	@echo "  Dashboard: http://localhost:3000"
	@echo ""
	@echo "  Ctrl+C para detener"
	@trap 'kill %1 %2' INT; wait

# ============================================================
# Brainfuck test
# ============================================================
bf-build: c/bf.c
	$(CC) -O2 -o bf/bf bf/../bf.c 2>/dev/null || $(CC) -O2 -o bf/bf /dev/stdin < /dev/null

bf-test: bf/yuzu-viz.bf
	@echo "STATUS:OK\nTOTAL_REQ:42\nOK_REQ:40\nAVG_LAT:0.5" | ./bf/bf bf/yuzu-viz.bf

# ============================================================
# Clean
# ============================================================
clean:
	rm -f c/yuzu-api
	rm -f cpp/yuzu-dashboard
	rm -f cobol/yuzu-metrics
	rm -f asm/yuzu-tui-*.o asm/yuzu-tui-x86_64 asm/yuzu-tui-x86
	rm -f asm/yuzu-tui-arm64-linux asm/yuzu-tui-arm64-android
	rm -f asm/yuzu-tui-arm32-linux asm/yuzu-tui-arm32-android
	rm -f bf/bf
	cd rust && cargo clean 2>/dev/null || true
	@echo "✦ Limpio"

# ============================================================
# Help
# ============================================================
help:
	@echo ""
	@echo "  🍋 Yuzu — Comandos disponibles"
	@echo ""
	@echo "  make all              Compilar C + C++ + COBOL + Rust"
	@echo "  make c                Solo C API"
	@echo "  make cpp              Solo C++ Dashboard"
	@echo "  make cobol            Solo COBOL Metrics"
	@echo "  make rust             Solo Rust CLI"
	@echo ""
	@echo "  make asm-x86_64       TUI Assembly x86_64"
	@echo "  make asm-x86          TUI Assembly i686"
	@echo "  make asm-arm64-linux  TUI Assembly ARM64 Linux"
	@echo "  make asm-arm64-android TUI Assembly ARM64 Android"
	@echo "  make asm-arm32-linux  TUI Assembly ARM32 Linux"
	@echo "  make asm-arm32-android TUI Assembly ARM32 Android"
	@echo "  make asm-all          Todos los assembly"
	@echo ""
	@echo "  make docker           Docker Compose up"
	@echo "  make docker-down      Docker Compose down"
	@echo "  make docker-logs      Ver logs"
	@echo ""
	@echo "  make run              Correr local sin Docker"
	@echo "  make clean            Limpiar binarios"
