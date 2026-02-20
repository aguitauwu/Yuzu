// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "yuzu",
    targets: [
        .executableTarget(
            name: "yuzu",
            path: "Sources/yuzu"
        )
    ]
)
