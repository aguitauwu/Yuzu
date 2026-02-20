// ============================================================
// Yuzu API Server — C Core
// Compilar: gcc -O2 -o yuzu-api server.c -lpthread
// ============================================================
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <pthread.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <time.h>

#define PORT         8080
#define BUFFER_SIZE  8192
#define MAX_ROUTES   64
#define MAX_CLIENTS  128
#define VERSION      "1.0.0"
#define NAME         "yuzu"

// ============================================================
// Estructuras
// ============================================================

typedef struct {
    char method[8];
    char path[512];
    char body[4096];
    char headers[2048];
    char query[512];
    char content_type[128];
    int  content_length;
} Request;

typedef struct {
    int  status;
    char content_type[64];
    char body[8192];
    char extra_headers[512];
} Response;

typedef struct {
    char   method[8];
    char   path[512];
    void (*handler)(Request*, Response*);
} Route;

typedef struct {
    pthread_mutex_t lock;
    long   total_requests;
    long   ok_requests;
    long   error_requests;
    long   active_connections;
    double avg_latency_ms;
    double last_latency_ms;
    double min_latency_ms;
    double max_latency_ms;
    time_t start_time;
    time_t last_request_time;
    char   last_path[256];
    char   last_method[8];
} Metrics;

typedef struct {
    int client_fd;
    struct sockaddr_in client_addr;
} ClientArgs;

// ============================================================
// Globales
// ============================================================

static Route   routes[MAX_ROUTES];
static int     route_count = 0;
static Metrics metrics     = {.lock = PTHREAD_MUTEX_INITIALIZER,
                               .min_latency_ms = 999999.0};
static int     server_fd   = -1;
static volatile int running = 1;

// ============================================================
// Logging
// ============================================================

void log_info(const char *fmt, ...) {
    time_t now = time(NULL);
    struct tm *t = localtime(&now);
    char ts[32];
    strftime(ts, sizeof(ts), "%Y-%m-%d %H:%M:%S", t);
    printf("\033[38;2;115;115;115m[%s]\033[0m \033[38;2;244;114;182m✦\033[0m ", ts);
    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);
    printf("\n");
    fflush(stdout);
}

void log_request(const char *method, const char *path, int status, double ms) {
    const char *color = status < 400 ? "\033[38;2;134;239;172m" : "\033[38;2;244;114;182m";
    time_t now = time(NULL);
    struct tm *t = localtime(&now);
    char ts[32];
    strftime(ts, sizeof(ts), "%H:%M:%S", t);
    printf("\033[38;2;115;115;115m[%s]\033[0m %s%d\033[0m \033[38;2;250;250;250m%-6s\033[0m \033[38;2;147;197;253m%s\033[0m \033[38;2;115;115;115m%.2fms\033[0m\n",
           ts, color, status, method, path, ms);
    fflush(stdout);
}

// ============================================================
// Utilidades
// ============================================================

double now_ms() {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (tv.tv_sec * 1000.0) + (tv.tv_usec / 1000.0);
}

char *url_decode(const char *src, char *dst, size_t dst_len) {
    size_t i = 0, j = 0;
    while (src[i] && j < dst_len - 1) {
        if (src[i] == '%' && src[i+1] && src[i+2]) {
            char hex[3] = {src[i+1], src[i+2], 0};
            dst[j++] = (char)strtol(hex, NULL, 16);
            i += 3;
        } else if (src[i] == '+') {
            dst[j++] = ' ';
            i++;
        } else {
            dst[j++] = src[i++];
        }
    }
    dst[j] = '\0';
    return dst;
}

// ============================================================
// HTTP
// ============================================================

void parse_request(const char *raw, Request *req) {
    memset(req, 0, sizeof(Request));

    // method y path
    sscanf(raw, "%7s %511s", req->method, req->path);

    // separar query string
    char *q = strchr(req->path, '?');
    if (q) {
        strncpy(req->query, q + 1, sizeof(req->query) - 1);
        *q = '\0';
    }

    // headers
    const char *h = strchr(raw, '\n');
    if (h) strncpy(req->headers, h + 1, sizeof(req->headers) - 1);

    // Content-Type
    const char *ct = strcasestr(raw, "Content-Type:");
    if (ct) sscanf(ct, "Content-Type: %127[^\r\n]", req->content_type);

    // Content-Length
    const char *cl = strcasestr(raw, "Content-Length:");
    if (cl) sscanf(cl, "Content-Length: %d", &req->content_length);

    // body
    const char *body = strstr(raw, "\r\n\r\n");
    if (body) strncpy(req->body, body + 4, sizeof(req->body) - 1);
}

void send_response(int fd, Response *res) {
    char header[2048];
    const char *status_text =
        res->status == 200 ? "OK" :
        res->status == 201 ? "Created" :
        res->status == 204 ? "No Content" :
        res->status == 400 ? "Bad Request" :
        res->status == 404 ? "Not Found" :
        res->status == 405 ? "Method Not Allowed" :
        res->status == 500 ? "Internal Server Error" : "Unknown";

    snprintf(header, sizeof(header),
        "HTTP/1.1 %d %s\r\n"
        "Content-Type: %s\r\n"
        "Content-Length: %zu\r\n"
        "Access-Control-Allow-Origin: *\r\n"
        "Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS\r\n"
        "Access-Control-Allow-Headers: Content-Type, Authorization\r\n"
        "X-Powered-By: %s/%s\r\n"
        "Connection: close\r\n"
        "%s"
        "\r\n",
        res->status, status_text,
        res->content_type,
        strlen(res->body),
        NAME, VERSION,
        res->extra_headers
    );

    write(fd, header, strlen(header));
    if (strlen(res->body) > 0)
        write(fd, res->body, strlen(res->body));
}

void respond_json(Response *res, int status, const char *json) {
    res->status = status;
    strncpy(res->content_type, "application/json", 63);
    strncpy(res->body, json, sizeof(res->body) - 1);
}

void respond_text(Response *res, int status, const char *text) {
    res->status = status;
    strncpy(res->content_type, "text/plain", 63);
    strncpy(res->body, text, sizeof(res->body) - 1);
}

// ============================================================
// Router
// ============================================================

void add_route(const char *method, const char *path, void (*handler)(Request*, Response*)) {
    if (route_count >= MAX_ROUTES) return;
    strncpy(routes[route_count].method, method, 7);
    strncpy(routes[route_count].path,   path,   511);
    routes[route_count].handler = handler;
    route_count++;
}

void dispatch(Request *req, Response *res) {
    // OPTIONS preflight
    if (strcmp(req->method, "OPTIONS") == 0) {
        res->status = 204;
        strncpy(res->content_type, "text/plain", 63);
        return;
    }

    for (int i = 0; i < route_count; i++) {
        if (strcmp(routes[i].method, req->method) == 0 &&
            strcmp(routes[i].path,   req->path)   == 0) {
            routes[i].handler(req, res);
            return;
        }
    }
    respond_json(res, 404, "{\"error\":\"route not found\",\"status\":404}");
}

// ============================================================
// Handlers
// ============================================================

void handle_health(Request *req, Response *res) {
    time_t uptime = time(NULL) - metrics.start_time;
    char buf[256];
    snprintf(buf, sizeof(buf),
        "{\"status\":\"ok\",\"name\":\"%s\",\"version\":\"%s\",\"uptime\":%ld}",
        NAME, VERSION, uptime);
    respond_json(res, 200, buf);
}

void handle_ping(Request *req, Response *res) {
    respond_json(res, 200, "{\"pong\":true}");
}

void handle_metrics(Request *req, Response *res) {
    pthread_mutex_lock(&metrics.lock);
    time_t uptime = time(NULL) - metrics.start_time;
    char buf[1024];
    snprintf(buf, sizeof(buf),
        "{"
        "\"total_requests\":%ld,"
        "\"ok_requests\":%ld,"
        "\"error_requests\":%ld,"
        "\"active_connections\":%ld,"
        "\"avg_latency_ms\":%.2f,"
        "\"last_latency_ms\":%.2f,"
        "\"min_latency_ms\":%.2f,"
        "\"max_latency_ms\":%.2f,"
        "\"uptime_seconds\":%ld,"
        "\"last_path\":\"%s\","
        "\"last_method\":\"%s\","
        "\"version\":\"%s\","
        "\"name\":\"%s\""
        "}",
        metrics.total_requests,
        metrics.ok_requests,
        metrics.error_requests,
        metrics.active_connections,
        metrics.avg_latency_ms,
        metrics.last_latency_ms,
        metrics.min_latency_ms == 999999.0 ? 0.0 : metrics.min_latency_ms,
        metrics.max_latency_ms,
        uptime,
        metrics.last_path,
        metrics.last_method,
        VERSION,
        NAME
    );
    pthread_mutex_unlock(&metrics.lock);
    respond_json(res, 200, buf);
}

void handle_routes(Request *req, Response *res) {
    char buf[2048] = "{\"routes\":[";
    for (int i = 0; i < route_count; i++) {
        char route[128];
        snprintf(route, sizeof(route),
            "{\"method\":\"%s\",\"path\":\"%s\"}%s",
            routes[i].method, routes[i].path,
            i < route_count - 1 ? "," : "");
        strncat(buf, route, sizeof(buf) - strlen(buf) - 1);
    }
    strncat(buf, "]}", sizeof(buf) - strlen(buf) - 1);
    respond_json(res, 200, buf);
}

void handle_echo(Request *req, Response *res) {
    char buf[4096];
    snprintf(buf, sizeof(buf),
        "{\"method\":\"%s\",\"path\":\"%s\",\"query\":\"%s\",\"body\":%s}",
        req->method, req->path, req->query,
        strlen(req->body) > 0 ? req->body : "null");
    respond_json(res, 200, buf);
}

void handle_not_found(Request *req, Response *res) {
    respond_json(res, 404, "{\"error\":\"not found\",\"status\":404}");
}

// ============================================================
// Worker thread
// ============================================================

void *handle_client(void *arg) {
    ClientArgs *ca = (ClientArgs*)arg;
    int fd = ca->client_fd;
    free(ca);

    char buf[BUFFER_SIZE];
    double t_start = now_ms();

    pthread_mutex_lock(&metrics.lock);
    metrics.active_connections++;
    pthread_mutex_unlock(&metrics.lock);

    ssize_t n = read(fd, buf, sizeof(buf) - 1);
    if (n <= 0) {
        close(fd);
        pthread_mutex_lock(&metrics.lock);
        metrics.active_connections--;
        pthread_mutex_unlock(&metrics.lock);
        return NULL;
    }
    buf[n] = '\0';

    Request req;
    Response res;
    memset(&res, 0, sizeof(Response));
    strncpy(res.content_type, "application/json", 63);

    parse_request(buf, &req);
    dispatch(&req, &res);
    send_response(fd, &res);

    double latency = now_ms() - t_start;

    pthread_mutex_lock(&metrics.lock);
    metrics.total_requests++;
    if (res.status < 400) metrics.ok_requests++;
    else                  metrics.error_requests++;
    metrics.active_connections--;
    metrics.last_latency_ms = latency;
    metrics.avg_latency_ms =
        (metrics.avg_latency_ms * (metrics.total_requests - 1) + latency)
        / metrics.total_requests;
    if (latency < metrics.min_latency_ms) metrics.min_latency_ms = latency;
    if (latency > metrics.max_latency_ms) metrics.max_latency_ms = latency;
    metrics.last_request_time = time(NULL);
    strncpy(metrics.last_path,   req.path,   255);
    strncpy(metrics.last_method, req.method, 7);
    pthread_mutex_unlock(&metrics.lock);

    log_request(req.method, req.path, res.status, latency);

    close(fd);
    return NULL;
}

// ============================================================
// Signal handler
// ============================================================

void handle_signal(int sig) {
    log_info("señal %d recibida — cerrando Yuzu...", sig);
    running = 0;
    if (server_fd >= 0) close(server_fd);
    exit(0);
}

// ============================================================
// Main
// ============================================================

int main(int argc, char **argv) {
    signal(SIGINT,  handle_signal);
    signal(SIGTERM, handle_signal);
    signal(SIGPIPE, SIG_IGN);

    metrics.start_time = time(NULL);

    // registrar rutas
    add_route("GET",  "/",        handle_health);
    add_route("GET",  "/health",  handle_health);
    add_route("GET",  "/ping",    handle_ping);
    add_route("GET",  "/metrics", handle_metrics);
    add_route("GET",  "/routes",  handle_routes);
    add_route("GET",  "/echo",    handle_echo);
    add_route("POST", "/echo",    handle_echo);

    server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0) { perror("socket"); exit(1); }

    int opt = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    struct sockaddr_in addr = {
        .sin_family      = AF_INET,
        .sin_addr.s_addr = INADDR_ANY,
        .sin_port        = htons(PORT)
    };

    if (bind(server_fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        perror("bind"); exit(1);
    }
    if (listen(server_fd, MAX_CLIENTS) < 0) {
        perror("listen"); exit(1);
    }

    printf("\033[2J\033[H");
    printf("\033[38;2;244;114;182m\033[1m");
    printf("  ╭─────────────────────────────────────────╮\n");
    printf("  │                                         │\n");
    printf("  │   🍋  Y U Z U  A P I                    │\n");
    printf("  │   C Core — v%s                       │\n", VERSION);
    printf("  │                                         │\n");
    printf("  ╰─────────────────────────────────────────╯\n");
    printf("\033[0m\n");

    log_info("escuchando en http://0.0.0.0:%d", PORT);
    log_info("rutas registradas: %d", route_count);
    printf("\n");

    while (running) {
        struct sockaddr_in client_addr;
        socklen_t client_len = sizeof(client_addr);
        int client_fd = accept(server_fd, (struct sockaddr*)&client_addr, &client_len);
        if (client_fd < 0) {
            if (running) perror("accept");
            continue;
        }

        ClientArgs *ca = malloc(sizeof(ClientArgs));
        ca->client_fd   = client_fd;
        ca->client_addr = client_addr;

        pthread_t tid;
        pthread_attr_t attr;
        pthread_attr_init(&attr);
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
        pthread_create(&tid, &attr, handle_client, ca);
        pthread_attr_destroy(&attr);
    }

    close(server_fd);
    return 0;
}
