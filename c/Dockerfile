FROM gcc:12-bullseye AS builder
WORKDIR /build
COPY server.c .
RUN gcc -O2 -o yuzu-api server.c -lpthread -static

FROM debian:bullseye-slim
WORKDIR /app
COPY --from=builder /build/yuzu-api .
EXPOSE 8080
HEALTHCHECK --interval=10s --timeout=3s \
    CMD wget -qO- http://localhost:8080/health || exit 1
CMD ["./yuzu-api"]
