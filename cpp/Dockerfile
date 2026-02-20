FROM gcc:12-bullseye AS builder
WORKDIR /build
COPY dashboard.cpp .
RUN g++ -O2 -std=c++17 -o yuzu-dashboard dashboard.cpp -lpthread -static

FROM debian:bullseye-slim
WORKDIR /app
COPY --from=builder /build/yuzu-dashboard .
EXPOSE 3000
HEALTHCHECK --interval=15s --timeout=3s \
    CMD wget -qO- http://localhost:3000/api/health || exit 1
CMD ["./yuzu-dashboard"]
