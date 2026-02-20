#!/bin/sh
# Yuzu COBOL metrics poller
# Hace fetch de /metrics cada 5 segundos y procesa via COBOL

HOST="${YUZU_API_HOST:-localhost}"
PORT="${YUZU_API_PORT:-8080}"
URL="http://${HOST}:${PORT}/metrics"

echo "✦ Yuzu COBOL Metrics Processor iniciando..."
echo "✦ Polling ${URL} cada 5s"

while true; do
    DATA=$(wget -qO- "${URL}" 2>/dev/null)
    if [ -n "$DATA" ]; then
        echo "--- $(date '+%H:%M:%S') ---"
        ./yuzu-metrics "$DATA"
    else
        echo "$(date '+%H:%M:%S') API no disponible"
    fi
    sleep 5
done
