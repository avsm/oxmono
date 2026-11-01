// swift-tools-version: 6.2
import PackageDescription

let package = Package(
    name: "AppleMLXEmbeddingsBridge",
    platforms: [.macOS("26.0")],
    products: [
        .library(name: "AppleMLXEmbeddingsBridge", type: .static,
                 targets: ["AppleMLXEmbeddingsBridge"])
    ],
    dependencies: [
        .package(url: "https://github.com/ml-explore/mlx-swift", exact: "0.32.3"),
        .package(url: "https://github.com/ml-explore/mlx-swift-lm", exact: "3.32.3"),
        .package(url: "https://github.com/huggingface/swift-transformers", exact: "1.3.0")
    ],
    targets: [
        .target(
            name: "AppleMLXEmbeddingsBridge",
            dependencies: [
                .product(name: "MLX", package: "mlx-swift"),
                .product(name: "MLXNN", package: "mlx-swift"),
                .product(name: "MLXEmbedders", package: "mlx-swift-lm"),
                .product(name: "MLXVLM", package: "mlx-swift-lm"),
                .product(name: "MLXLMCommon", package: "mlx-swift-lm"),
                .product(name: "Hub", package: "swift-transformers"),
                .product(name: "Tokenizers", package: "swift-transformers")
            ])
    ]
)
