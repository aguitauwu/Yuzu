// ============================================================
// Yuzu — Rust CLI
// Compilar: cargo build --release
// ============================================================
use std::io::{self, Read, Write};
use std::net::TcpStream;
use std::time::{Duration, Instant};
use std::env;
use std::process;

const VERSION: &str = "1.0.0";
const DEFAULT_HOST: &str = "localhost";
const DEFAULT_PORT: u16 = 8080;

// ============================================================
// Colores ANSI
// ============================================================
const PINK:  &str = "\x1b[38;2;244;114;182m";
const WHITE: &str = "\x1b[38;2;250;250;250m";
const GRAY:  &str = "\x1b[38;2;115;115;115m";
const GREEN: &str = "\x1b[38;2;134;239;172m";
const RED:   &str = "\x1b[38;2;248;113;113m";
const YELLOW:&str = "\x1b[38;2;251;191;36m";
const RESET: &str = "\x1b[0m";
const BOLD:  &str = "\x1b[1m";

// ============================================================
// HTTP client simple
// ============================================================
fn http_get(host: &str, port: u16, path: &str) -> Result<(u16, String), String> {
    let addr = format!("{}:{}", host, port);
    let mut stream = TcpStream::connect(&addr)
        .map_err(|e| format!("conexión fallida: {}", e))?;

    stream.set_read_timeout(Some(Duration::from_secs(5)))
          .map_err(|e| e.to_string())?;

    let req = format!(
        "GET {} HTTP/1.1\r\nHost: {}\r\nUser-Agent: yuzu-cli/{}\r\nConnection: close\r\n\r\n",
        path, host, VERSION
    );
    stream.write_all(req.as_bytes())
          .map_err(|e| e.to_string())?;

    let mut response = String::new();
    stream.read_to_string(&mut response)
          .map_err(|e| e.to_string())?;

    let status: u16 = response
        .lines()
        .next()
        .and_then(|l| l.split_whitespace().nth(1))
        .and_then(|s| s.parse().ok())
        .unwrap_or(500);

    let body = response
        .find("\r\n\r\n")
        .map(|i| response[i + 4..].to_string())
        .unwrap_or_default();

    Ok((status, body))
}

fn http_post(host: &str, port: u16, path: &str, body: &str) -> Result<(u16, String), String> {
    let addr = format!("{}:{}", host, port);
    let mut stream = TcpStream::connect(&addr)
        .map_err(|e| format!("conexión fallida: {}", e))?;

    stream.set_read_timeout(Some(Duration::from_secs(5)))
          .map_err(|e| e.to_string())?;

    let req = format!(
        "POST {} HTTP/1.1\r\nHost: {}\r\nUser-Agent: yuzu-cli/{}\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
        path, host, VERSION, body.len(), body
    );
    stream.write_all(req.as_bytes())
          .map_err(|e| e.to_string())?;

    let mut response = String::new();
    stream.read_to_string(&mut response)
          .map_err(|e| e.to_string())?;

    let status: u16 = response
        .lines()
        .next()
        .and_then(|l| l.split_whitespace().nth(1))
        .and_then(|s| s.parse().ok())
        .unwrap_or(500);

    let resp_body = response
        .find("\r\n\r\n")
        .map(|i| response[i + 4..].to_string())
        .unwrap_or_default();

    Ok((status, resp_body))
}

// ============================================================
// Parser JSON simple
// ============================================================
fn json_get(json: &str, key: &str) -> Option<String> {
    let pattern = format!("\"{}\":", key);
    let start = json.find(&pattern)? + pattern.len();
    let rest = json[start..].trim_start();

    if rest.starts_with('"') {
        let inner = &rest[1..];
        let end = inner.find('"')?;
        Some(inner[..end].to_string())
    } else {
        let end = rest.find(|c| c == ',' || c == '}').unwrap_or(rest.len());
        Some(rest[..end].trim().to_string())
    }
}

// ============================================================
// Comandos
// ============================================================
fn cmd_ping(host: &str, port: u16) {
    let start = Instant::now();
    match http_get(host, port, "/ping") {
        Ok((status, body)) => {
            let ms = start.elapsed().as_millis();
            let color = if status == 200 { GREEN } else { RED };
            println!("{}✦{} ping → {}{}ms{} {} {}{}{}",
                PINK, RESET, color, ms, RESET,
                if status == 200 { "✓" } else { "✗" },
                GRAY, body.trim(), RESET);
        }
        Err(e) => println!("{}✖{} ping fallido: {}", RED, RESET, e),
    }
}

fn cmd_health(host: &str, port: u16) {
    match http_get(host, port, "/health") {
        Ok((status, body)) => {
            let color = if status == 200 { GREEN } else { RED };
            println!("\n{}{}✦ Health Check{}", BOLD, PINK, RESET);
            println!("{}──────────────────────{}", GRAY, RESET);
            println!("  Status:  {}{}{}", color, status, RESET);

            if let Some(s) = json_get(&body, "status") {
                println!("  API:     {}{}{}", color, s.to_uppercase(), RESET);
            }
            if let Some(v) = json_get(&body, "version") {
                println!("  Version: {}{}{}", WHITE, v, RESET);
            }
            if let Some(u) = json_get(&body, "uptime") {
                println!("  Uptime:  {}{}s{}", WHITE, u, RESET);
            }
            println!();
        }
        Err(e) => println!("{}✖{} health check fallido: {}", RED, RESET, e),
    }
}

fn cmd_metrics(host: &str, port: u16) {
    match http_get(host, port, "/metrics") {
        Ok((_, body)) => {
            println!("\n{}{}✦ Métricas en tiempo real{}", BOLD, PINK, RESET);
            println!("{}──────────────────────────────{}", GRAY, RESET);

            let fields = [
                ("total_requests",   "Total Requests", WHITE,  ""),
                ("ok_requests",      "OK Requests",    GREEN,  ""),
                ("error_requests",   "Errores",         RED,   ""),
                ("avg_latency_ms",   "Latencia Avg",   YELLOW, "ms"),
                ("last_latency_ms",  "Latencia Última",YELLOW, "ms"),
                ("min_latency_ms",   "Latencia Mín",   GREEN,  "ms"),
                ("max_latency_ms",   "Latencia Máx",   RED,    "ms"),
                ("uptime_seconds",   "Uptime",         WHITE,  "s"),
                ("active_connections","Conexiones",     PINK,  ""),
            ];

            for (key, label, color, unit) in &fields {
                if let Some(val) = json_get(&body, key) {
                    println!("  {:<20} {}{}{}{}", label, color, val, unit, RESET);
                }
            }

            if let Some(path) = json_get(&body, "last_path") {
                if let Some(method) = json_get(&body, "last_method") {
                    println!("  {:<20} {}{} {}{}", "Último Request", GRAY, method, path, RESET);
                }
            }
            println!();
        }
        Err(e) => println!("{}✖{} métricas fallidas: {}", RED, RESET, e),
    }
}

fn cmd_watch(host: &str, port: u16, interval: u64) {
    println!("{}✦{} watch mode — actualizando cada {}{}s{} {}(Ctrl+C para salir){}",
        PINK, RESET, WHITE, interval, RESET, GRAY, RESET);

    loop {
        print!("\x1b[2J\x1b[H");
        print!("{}{}🍋 Yuzu CLI — Watch Mode{} {}{}{}\n",
            BOLD, PINK, RESET, GRAY, chrono_now(), RESET);
        cmd_metrics(host, port);
        std::thread::sleep(Duration::from_secs(interval));
    }
}

fn cmd_get(host: &str, port: u16, path: &str) {
    let start = Instant::now();
    match http_get(host, port, path) {
        Ok((status, body)) => {
            let ms = start.elapsed().as_millis();
            let color = if status < 400 { GREEN } else { RED };
            println!("{}{} {}{}ms{}", color, status, RESET, YELLOW, ms);
            println!("{}", RESET);
            println!("{}", body);
        }
        Err(e) => println!("{}✖{} GET fallido: {}", RED, RESET, e),
    }
}

fn cmd_post(host: &str, port: u16, path: &str, body: &str) {
    let start = Instant::now();
    match http_post(host, port, path, body) {
        Ok((status, resp)) => {
            let ms = start.elapsed().as_millis();
            let color = if status < 400 { GREEN } else { RED };
            println!("{}{} {}{}ms{}", color, status, RESET, YELLOW, ms);
            println!("{}", RESET);
            println!("{}", resp);
        }
        Err(e) => println!("{}✖{} POST fallido: {}", RED, RESET, e),
    }
}

fn cmd_routes(host: &str, port: u16) {
    match http_get(host, port, "/routes") {
        Ok((_, body)) => {
            println!("\n{}{}✦ Rutas disponibles{}", BOLD, PINK, RESET);
            println!("{}──────────────────{}", GRAY, RESET);
            // parsear array de rutas simple
            let mut rest = body.as_str();
            while let Some(m_start) = rest.find("\"method\":\"") {
                let m_rest = &rest[m_start + 10..];
                if let Some(m_end) = m_rest.find('"') {
                    let method = &m_rest[..m_end];
                    if let Some(p_start) = m_rest.find("\"path\":\"") {
                        let p_rest = &m_rest[p_start + 8..];
                        if let Some(p_end) = p_rest.find('"') {
                            let path = &p_rest[..p_end];
                            let color = match method {
                                "GET"    => GREEN,
                                "POST"   => YELLOW,
                                "DELETE" => RED,
                                _        => WHITE,
                            };
                            println!("  {}{:<8}{} {}{}{}",
                                color, method, RESET, WHITE, path, RESET);
                            rest = &p_rest[p_end..];
                            continue;
                        }
                    }
                }
                break;
            }
            println!();
        }
        Err(e) => println!("{}✖{} rutas fallidas: {}", RED, RESET, e),
    }
}

fn chrono_now() -> String {
    // simple timestamp sin chrono
    "ahora".to_string()
}

fn print_banner() {
    println!("{}{}",
        "\x1b[38;2;244;114;182m\x1b[1m",
        r#"  ╭─────────────────────────────────────────╮
  │                                         │
  │   🍋  Y U Z U  C L I                    │
  │   Tu API, bajo control                  │
  │                                         │
  ╰─────────────────────────────────────────╯"#
    );
    println!("\x1b[0m");
}

fn print_help() {
    print_banner();
    println!("{}Uso:{} yuzu [opciones] <comando>\n", BOLD, RESET);
    println!("{}Opciones:{}", BOLD, RESET);
    println!("  {}--host{} <host>    Host de la API {}(default: localhost){}", WHITE, RESET, GRAY, RESET);
    println!("  {}--port{} <port>    Puerto de la API {}(default: 8080){}", WHITE, RESET, GRAY, RESET);
    println!();
    println!("{}Comandos:{}", BOLD, RESET);
    println!("  {}ping{}             Verificar conectividad", GREEN, RESET);
    println!("  {}health{}           Estado de la API", GREEN, RESET);
    println!("  {}metrics{}          Métricas en tiempo real", GREEN, RESET);
    println!("  {}routes{}           Listar rutas disponibles", GREEN, RESET);
    println!("  {}watch{} [s]        Monitoreo continuo {}(default: 2s){}", GREEN, RESET, GRAY, RESET);
    println!("  {}get{} <path>       GET a un endpoint", YELLOW, RESET);
    println!("  {}post{} <path> <j>  POST con JSON body", YELLOW, RESET);
    println!();
    println!("{}Ejemplos:{}", BOLD, RESET);
    println!("  {}yuzu ping{}", GRAY, RESET);
    println!("  {}yuzu health{}", GRAY, RESET);
    println!("  {}yuzu metrics{}", GRAY, RESET);
    println!("  {}yuzu watch 5{}", GRAY, RESET);
    println!("  {}yuzu get /ping{}", GRAY, RESET);
    println!("  {}yuzu --host api.example.com --port 443 health{}", GRAY, RESET);
    println!();
}

// ============================================================
// Main
// ============================================================
fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_help();
        process::exit(0);
    }

    let mut host = DEFAULT_HOST.to_string();
    let mut port = DEFAULT_PORT;
    let mut cmd_args: Vec<String> = Vec::new();

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--host" => {
                i += 1;
                if i < args.len() { host = args[i].clone(); }
            }
            "--port" => {
                i += 1;
                if i < args.len() { port = args[i].parse().unwrap_or(DEFAULT_PORT); }
            }
            "--help" | "-h" => { print_help(); process::exit(0); }
            "--version" | "-v" => {
                println!("yuzu-cli v{}", VERSION);
                process::exit(0);
            }
            _ => cmd_args.push(args[i].clone()),
        }
        i += 1;
    }

    if cmd_args.is_empty() {
        print_help();
        process::exit(0);
    }

    match cmd_args[0].as_str() {
        "ping"    => cmd_ping(&host, port),
        "health"  => cmd_health(&host, port),
        "metrics" => cmd_metrics(&host, port),
        "routes"  => cmd_routes(&host, port),
        "watch"   => {
            let interval = cmd_args.get(1)
                .and_then(|s| s.parse().ok())
                .unwrap_or(2u64);
            cmd_watch(&host, port, interval);
        }
        "get" => {
            if cmd_args.len() < 2 {
                eprintln!("{}✖{} uso: yuzu get <path>", RED, RESET);
                process::exit(1);
            }
            cmd_get(&host, port, &cmd_args[1]);
        }
        "post" => {
            if cmd_args.len() < 3 {
                eprintln!("{}✖{} uso: yuzu post <path> <json>", RED, RESET);
                process::exit(1);
            }
            cmd_post(&host, port, &cmd_args[1], &cmd_args[2]);
        }
        unknown => {
            eprintln!("{}✖{} comando desconocido: {}", RED, RESET, unknown);
            print_help();
            process::exit(1);
        }
    }
}
