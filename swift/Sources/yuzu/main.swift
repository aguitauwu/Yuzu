// ============================================================
// Yuzu — Swift HTTPS Layer
// Compilar: swift build -c release
// ============================================================
import Foundation
import FoundationNetworking

let YUZU_API  = "http://localhost:8080"
let HTTPS_PORT = 8443
let VERSION   = "1.0.0"

// ============================================================
// Logger
// ============================================================
func log(_ msg: String) {
    let df = DateFormatter()
    df.dateFormat = "HH:mm:ss"
    let ts = df.string(from: Date())
    fputs("\u{001B}[38;2;115;115;115m[\(ts)]\u{001B}[0m \u{001B}[38;2;244;114;182m✦\u{001B}[0m \(msg)\n", stderr)
}

// ============================================================
// HTTP proxy hacia C
// ============================================================
func proxyRequest(method: String, path: String, body: String?, headers: [String: String]) -> (Int, String) {
    let urlStr = "\(YUZU_API)\(path)"
    guard let url = URL(string: urlStr) else { return (500, "{\"error\":\"invalid url\"}") }

    var req = URLRequest(url: url)
    req.httpMethod = method
    req.timeoutInterval = 10
    req.addValue("application/json", forHTTPHeaderField: "Content-Type")
    req.addValue("yuzu-swift/\(VERSION)", forHTTPHeaderField: "X-Forwarded-By")

    for (k, v) in headers { req.addValue(v, forHTTPHeaderField: k) }
    if let b = body { req.httpBody = b.data(using: .utf8) }

    var result = (500, "{\"error\":\"timeout\"}")
    let sem = DispatchSemaphore(value: 0)

    URLSession.shared.dataTask(with: req) { data, response, error in
        if let error = error {
            result = (500, "{\"error\":\"\(error.localizedDescription)\"}")
            sem.signal()
            return
        }
        let status = (response as? HTTPURLResponse)?.statusCode ?? 500
        let body   = data.flatMap { String(data: $0, encoding: .utf8) } ?? "{}"
        result = (status, body)
        sem.signal()
    }.resume()

    sem.wait()
    return result
}

// ============================================================
// Parser HTTP simple
// ============================================================
struct HTTPRequest {
    var method:  String = "GET"
    var path:    String = "/"
    var headers: [String: String] = [:]
    var body:    String = ""
}

func parseHTTP(_ raw: String) -> HTTPRequest {
    var req = HTTPRequest()
    let lines = raw.components(separatedBy: "\r\n")
    guard let first = lines.first else { return req }

    let parts = first.components(separatedBy: " ")
    if parts.count >= 2 {
        req.method = parts[0]
        req.path   = parts[1].components(separatedBy: "?").first ?? parts[1]
    }

    var i = 1
    while i < lines.count && !lines[i].isEmpty {
        let h = lines[i].components(separatedBy: ": ")
        if h.count >= 2 { req.headers[h[0]] = h[1...].joined(separator: ": ") }
        i += 1
    }

    if i + 1 < lines.count { req.body = lines[(i+1)...].joined(separator: "\r\n") }
    return req
}

// ============================================================
// Servidor TCP simple con TLS simulado (proxy a C)
// En producción usar NIO o Vapor para TLS real
// ============================================================
func buildResponse(status: Int, body: String, extraHeaders: String = "") -> String {
    let statusText = status == 200 ? "OK" : status == 404 ? "Not Found" : "Internal Server Error"
    return "HTTP/1.1 \(status) \(statusText)\r\n"
         + "Content-Type: application/json\r\n"
         + "Content-Length: \(body.utf8.count)\r\n"
         + "Access-Control-Allow-Origin: *\r\n"
         + "X-Forwarded-By: yuzu-swift/\(VERSION)\r\n"
         + "Connection: close\r\n"
         + extraHeaders
         + "\r\n"
         + body
}

// ============================================================
// Health check propio de Swift
// ============================================================
func handleSwiftHealth() -> String {
    let body = "{\"status\":\"ok\",\"layer\":\"swift\",\"version\":\"\(VERSION)\",\"upstream\":\"\(YUZU_API)\"}"
    return buildResponse(status: 200, body: body)
}

// ============================================================
// Main loop
// ============================================================
func main() {
    // Verificar que C esté corriendo
    let (healthStatus, _) = proxyRequest(method: "GET", path: "/health", body: nil, headers: [:])
    if healthStatus != 200 {
        log("⚠️  Yuzu C API no responde en \(YUZU_API)")
        log("   Asegúrate de que yuzu-api esté corriendo")
    } else {
        log("✅ Conectado a Yuzu C API en \(YUZU_API)")
    }

    log("Swift HTTPS layer v\(VERSION) listo")
    log("Proxy: Swift:\(HTTPS_PORT) → C:\(8080)")
    log("Usa el C server directamente en desarrollo")
    log("En producción: configurar TLS con certificados reales")

    // Loop de proxy para requests entrantes via stdin (modo pipe con C)
    // En producción: usar Network.framework o SwiftNIO
    while let line = readLine() {
        // Recibir path desde C via pipe y hacer proxy
        let trimmed = line.trimmingCharacters(in: .whitespaces)
        if trimmed.hasPrefix("PROXY:") {
            let path = String(trimmed.dropFirst(6))
            let (status, body) = proxyRequest(method: "GET", path: path, body: nil, headers: [:])
            log("PROXY \(path) → \(status)")
            print("STATUS:\(status):BODY:\(body)")
            fflush(stdout)
        } else if trimmed == "HEALTH" {
            let (status, body) = proxyRequest(method: "GET", path: "/health", body: nil, headers: [:])
            print("STATUS:\(status):BODY:\(body)")
            fflush(stdout)
        } else if trimmed == "METRICS" {
            let (status, body) = proxyRequest(method: "GET", path: "/metrics", body: nil, headers: [:])
            print("STATUS:\(status):BODY:\(body)")
            fflush(stdout)
        } else if trimmed == "EXIT" {
            break
        }
    }
}

main()
