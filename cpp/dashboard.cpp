// ============================================================
// Yuzu — C++ Web Dashboard
// Compilar: g++ -O2 -std=c++17 -o yuzu-dashboard dashboard.cpp -lpthread
// ============================================================
#include <iostream>
#include <sstream>
#include <string>
#include <map>
#include <vector>
#include <thread>
#include <mutex>
#include <atomic>
#include <cstring>
#include <ctime>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>

#define DASHBOARD_PORT 3000
#define API_PORT       8080
#define VERSION        "1.0.0"

std::mutex  metrics_mutex;
std::string cached_metrics = "{}";
std::atomic<bool> running(true);

// ============================================================
// HTTP client para polling a C API
// ============================================================
std::string http_get(const std::string& host, int port, const std::string& path) {
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return "";

    struct timeval tv = {2, 0};
    setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));

    struct sockaddr_in addr{};
    addr.sin_family = AF_INET;
    addr.sin_port   = htons(port);
    inet_pton(AF_INET, host.c_str(), &addr.sin_addr);

    if (connect(fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        close(fd); return "";
    }

    std::string req = "GET " + path + " HTTP/1.1\r\nHost: " + host + "\r\nConnection: close\r\n\r\n";
    write(fd, req.c_str(), req.size());

    std::string response;
    char buf[4096];
    ssize_t n;
    while ((n = read(fd, buf, sizeof(buf))) > 0)
        response.append(buf, n);
    close(fd);

    auto pos = response.find("\r\n\r\n");
    return pos != std::string::npos ? response.substr(pos + 4) : "";
}

// ============================================================
// Poller de métricas en background
// ============================================================
void metrics_poller() {
    while (running) {
        std::string m = http_get("127.0.0.1", API_PORT, "/metrics");
        if (!m.empty()) {
            std::lock_guard<std::mutex> lock(metrics_mutex);
            cached_metrics = m;
        }
        std::this_thread::sleep_for(std::chrono::seconds(2));
    }
}

// ============================================================
// HTML Dashboard
// ============================================================
std::string build_dashboard() {
    std::string metrics;
    {
        std::lock_guard<std::mutex> lock(metrics_mutex);
        metrics = cached_metrics;
    }

    auto escape_js = [](const std::string& s) {
        std::string out;
        for (char c : s) {
            if (c == '\'') out += "\\'";
            else if (c == '\\') out += "\\\\";
            else out += c;
        }
        return out;
    };

    return R"(<!DOCTYPE html>
<html lang="es">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>🍋 Yuzu Dashboard</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body {
            background: #09090b;
            color: #fafafa;
            font-family: 'JetBrains Mono', 'Fira Code', monospace;
            min-height: 100vh;
            padding: 2rem;
        }
        .header {
            display: flex;
            align-items: center;
            gap: 1rem;
            margin-bottom: 2rem;
            padding-bottom: 1rem;
            border-bottom: 1px solid #27272a;
        }
        .header h1 {
            font-size: 1.5rem;
            color: #f472b6;
        }
        .header .version {
            font-size: 0.75rem;
            color: #71717a;
            background: #18181b;
            padding: 0.25rem 0.5rem;
            border-radius: 4px;
        }
        .status-badge {
            padding: 0.25rem 0.75rem;
            border-radius: 9999px;
            font-size: 0.75rem;
            font-weight: bold;
        }
        .status-ok   { background: #14532d; color: #86efac; }
        .status-warn { background: #451a03; color: #fbbf24; }
        .status-crit { background: #450a0a; color: #f87171; }
        .grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 1rem;
            margin-bottom: 2rem;
        }
        .card {
            background: #111113;
            border: 1px solid #27272a;
            border-radius: 8px;
            padding: 1.25rem;
        }
        .card .label {
            font-size: 0.7rem;
            color: #71717a;
            text-transform: uppercase;
            letter-spacing: 0.05em;
            margin-bottom: 0.5rem;
        }
        .card .value {
            font-size: 1.75rem;
            font-weight: bold;
            color: #f472b6;
        }
        .card .unit {
            font-size: 0.75rem;
            color: #71717a;
        }
        .chart-container {
            background: #111113;
            border: 1px solid #27272a;
            border-radius: 8px;
            padding: 1.25rem;
            margin-bottom: 1rem;
        }
        .chart-container h3 {
            font-size: 0.8rem;
            color: #71717a;
            text-transform: uppercase;
            letter-spacing: 0.05em;
            margin-bottom: 1rem;
        }
        .bar-chart { display: flex; flex-direction: column; gap: 0.5rem; }
        .bar-row { display: flex; align-items: center; gap: 0.75rem; }
        .bar-label { width: 120px; font-size: 0.75rem; color: #a1a1aa; }
        .bar-track {
            flex: 1;
            height: 8px;
            background: #27272a;
            border-radius: 4px;
            overflow: hidden;
        }
        .bar-fill {
            height: 100%;
            border-radius: 4px;
            transition: width 0.5s ease;
        }
        .bar-fill.ok   { background: #86efac; }
        .bar-fill.err  { background: #f87171; }
        .bar-fill.lat  { background: #f472b6; }
        .bar-value { font-size: 0.75rem; color: #a1a1aa; min-width: 60px; text-align: right; }
        .alerts {
            background: #111113;
            border: 1px solid #27272a;
            border-radius: 8px;
            padding: 1.25rem;
            margin-bottom: 1rem;
        }
        .alerts h3 {
            font-size: 0.8rem;
            color: #71717a;
            text-transform: uppercase;
            letter-spacing: 0.05em;
            margin-bottom: 0.75rem;
        }
        .alert-item {
            padding: 0.5rem 0.75rem;
            border-radius: 4px;
            font-size: 0.8rem;
            margin-bottom: 0.5rem;
        }
        .alert-ok   { background: #14532d22; color: #86efac; border: 1px solid #14532d; }
        .alert-warn { background: #451a0322; color: #fbbf24; border: 1px solid #451a03; }
        .alert-crit { background: #450a0a22; color: #f87171; border: 1px solid #450a0a; }
        .refresh-info {
            font-size: 0.7rem;
            color: #3f3f46;
            text-align: right;
            margin-top: 1rem;
        }
        .separator { border: none; border-top: 1px solid #27272a; margin: 1.5rem 0; }
        canvas { width: 100% !important; }
    </style>
</head>
<body>
    <div class="header">
        <span style="font-size:2rem">🍋</span>
        <h1>Yuzu Dashboard</h1>
        <span class="version">v)" + std::string(VERSION) + R"(</span>
        <span id="status-badge" class="status-badge status-ok">OK</span>
        <span style="margin-left:auto;font-size:0.7rem;color:#3f3f46" id="last-update">—</span>
    </div>

    <div class="grid" id="cards-grid">
        <div class="card">
            <div class="label">Total Requests</div>
            <div class="value" id="total-req">—</div>
        </div>
        <div class="card">
            <div class="label">Requests OK</div>
            <div class="value" id="ok-req" style="color:#86efac">—</div>
        </div>
        <div class="card">
            <div class="label">Errors</div>
            <div class="value" id="err-req" style="color:#f87171">—</div>
        </div>
        <div class="card">
            <div class="label">Avg Latency</div>
            <div class="value" id="avg-lat">—</div>
            <div class="unit">ms</div>
        </div>
        <div class="card">
            <div class="label">Last Latency</div>
            <div class="value" id="last-lat">—</div>
            <div class="unit">ms</div>
        </div>
        <div class="card">
            <div class="label">Uptime</div>
            <div class="value" id="uptime">—</div>
        </div>
    </div>

    <div class="chart-container">
        <h3>Distribución de Requests</h3>
        <div class="bar-chart">
            <div class="bar-row">
                <div class="bar-label">Exitosos</div>
                <div class="bar-track">
                    <div class="bar-fill ok" id="bar-ok" style="width:0%"></div>
                </div>
                <div class="bar-value" id="bar-ok-val">0%</div>
            </div>
            <div class="bar-row">
                <div class="bar-label">Errores</div>
                <div class="bar-track">
                    <div class="bar-fill err" id="bar-err" style="width:0%"></div>
                </div>
                <div class="bar-value" id="bar-err-val">0%</div>
            </div>
        </div>
    </div>

    <div class="alerts" id="alerts-section">
        <h3>Estado del Sistema</h3>
        <div id="alerts-list">
            <div class="alert-item alert-ok">✦ Sistema iniciando...</div>
        </div>
    </div>

    <div class="refresh-info">Actualización automática cada 2s • Yuzu C++ Dashboard</div>

<script>
const METRICS_URL = '/api/metrics';
let history_lat = [];
let history_req = [];

function formatUptime(s) {
    if (s < 60)   return s + 's';
    if (s < 3600) return Math.floor(s/60) + 'm ' + (s%60) + 's';
    const h = Math.floor(s/3600);
    const m = Math.floor((s%3600)/60);
    return h + 'h ' + m + 'm';
}

function parseJSON(text) {
    try { return JSON.parse(text); } catch { return null; }
}

async function fetchMetrics() {
    try {
        const res = await fetch(METRICS_URL);
        const data = await res.json();

        const total = data.total_requests || 0;
        const ok    = data.ok_requests    || 0;
        const err   = data.error_requests || 0;
        const avg   = (data.avg_latency_ms  || 0).toFixed(2);
        const last  = (data.last_latency_ms || 0).toFixed(2);
        const up    = data.uptime_seconds  || 0;

        document.getElementById('total-req').textContent = total;
        document.getElementById('ok-req').textContent    = ok;
        document.getElementById('err-req').textContent   = err;
        document.getElementById('avg-lat').textContent   = avg;
        document.getElementById('last-lat').textContent  = last;
        document.getElementById('uptime').textContent    = formatUptime(up);

        const okPct  = total > 0 ? (ok  / total * 100).toFixed(1) : 0;
        const errPct = total > 0 ? (err / total * 100).toFixed(1) : 0;
        document.getElementById('bar-ok').style.width    = okPct + '%';
        document.getElementById('bar-err').style.width   = errPct + '%';
        document.getElementById('bar-ok-val').textContent  = okPct + '%';
        document.getElementById('bar-err-val').textContent = errPct + '%';

        // Alertas
        const alerts = [];
        if (parseFloat(avg) > 100)
            alerts.push({cls:'alert-warn', msg:'⚠ Latencia promedio alta: ' + avg + 'ms'});
        else
            alerts.push({cls:'alert-ok', msg:'✦ Latencia normal: ' + avg + 'ms'});

        if (parseFloat(errPct) > 5)
            alerts.push({cls:'alert-crit', msg:'✖ Tasa de error crítica: ' + errPct + '%'});
        else if (parseFloat(errPct) > 0)
            alerts.push({cls:'alert-warn', msg:'⚠ Errores presentes: ' + errPct + '%'});
        else
            alerts.push({cls:'alert-ok', msg:'✦ Sin errores'});

        if (total === 0 && up > 60)
            alerts.push({cls:'alert-warn', msg:'⚠ Sin requests en ' + formatUptime(up)});

        const badge = document.getElementById('status-badge');
        const hasCrit = alerts.some(a => a.cls === 'alert-crit');
        const hasWarn = alerts.some(a => a.cls === 'alert-warn');
        badge.textContent = hasCrit ? 'CRITICAL' : hasWarn ? 'WARNING' : 'OK';
        badge.className   = 'status-badge ' + (hasCrit ? 'status-crit' : hasWarn ? 'status-warn' : 'status-ok');

        document.getElementById('alerts-list').innerHTML =
            alerts.map(a => `<div class="alert-item ${a.cls}">${a.msg}</div>`).join('');

        document.getElementById('last-update').textContent =
            'Última actualización: ' + new Date().toLocaleTimeString();

    } catch(e) {
        document.getElementById('status-badge').textContent = 'OFFLINE';
        document.getElementById('status-badge').className = 'status-badge status-crit';
    }
}

fetchMetrics();
setInterval(fetchMetrics, 2000);
</script>
</body>
</html>)";
}

// ============================================================
// HTTP server del dashboard
// ============================================================
void handle_dashboard_client(int fd) {
    char buf[4096] = {};
    ssize_t n = read(fd, buf, sizeof(buf) - 1);
    if (n <= 0) { close(fd); return; }

    std::string raw(buf);
    std::string method, path;
    std::istringstream ss(raw);
    ss >> method >> path;

    std::string body;
    std::string content_type;
    int status = 200;

    if (path == "/" || path == "/dashboard") {
        body         = build_dashboard();
        content_type = "text/html; charset=utf-8";
    } else if (path == "/api/metrics") {
        std::lock_guard<std::mutex> lock(metrics_mutex);
        body         = cached_metrics;
        content_type = "application/json";
    } else if (path == "/api/health") {
        body         = R"({"status":"ok","service":"yuzu-dashboard","version":")" + std::string(VERSION) + "\"}";
        content_type = "application/json";
    } else {
        status       = 404;
        body         = R"({"error":"not found"})";
        content_type = "application/json";
    }

    std::ostringstream resp;
    resp << "HTTP/1.1 " << status << (status == 200 ? " OK" : " Not Found") << "\r\n"
         << "Content-Type: " << content_type << "\r\n"
         << "Content-Length: " << body.size() << "\r\n"
         << "Access-Control-Allow-Origin: *\r\n"
         << "Cache-Control: no-cache\r\n"
         << "Connection: close\r\n"
         << "\r\n"
         << body;

    std::string r = resp.str();
    write(fd, r.c_str(), r.size());
    close(fd);
}

int main() {
    // Arrancar poller de métricas
    std::thread poller(metrics_poller);
    poller.detach();

    int server_fd = socket(AF_INET, SOCK_STREAM, 0);
    int opt = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    struct sockaddr_in addr{};
    addr.sin_family      = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port        = htons(DASHBOARD_PORT);

    bind(server_fd, (struct sockaddr*)&addr, sizeof(addr));
    listen(server_fd, 32);

    std::cout << "\033[38;2;244;114;182m✦\033[0m Yuzu Dashboard corriendo en "
              << "http://0.0.0.0:" << DASHBOARD_PORT << "\n";
    std::cout << "\033[38;2;244;114;182m✦\033[0m Poller de métricas activo → "
              << "http://localhost:" << API_PORT << "/metrics\n\n";

    while (running) {
        struct sockaddr_in client{};
        socklen_t len = sizeof(client);
        int fd = accept(server_fd, (struct sockaddr*)&client, &len);
        if (fd < 0) continue;

        std::thread([fd]() { handle_dashboard_client(fd); }).detach();
    }

    close(server_fd);
    return 0;
}
