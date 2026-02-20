FROM debian:bullseye-slim AS builder
RUN apt-get update && apt-get install -y gnucobol wget && rm -rf /var/lib/apt/lists/*
WORKDIR /build
COPY metrics.cob .
RUN cobc -x -o yuzu-metrics metrics.cob

FROM debian:bullseye-slim
RUN apt-get update && apt-get install -y libcob4 wget && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY --from=builder /build/yuzu-metrics .
COPY entrypoint.sh .
RUN chmod +x entrypoint.sh
CMD ["./entrypoint.sh"]
