import CryptoKit
import Foundation
import Hub
import MLX
import MLXEmbedders
import MLXLMCommon
import Metal
import Tokenizers

@_silgen_name("ame_metallib_data")
private func metallibData() -> UnsafeRawPointer
@_silgen_name("ame_metallib_size")
private func metallibSize() -> Int

struct BridgeError: LocalizedError {
  let message: String
  var errorDescription: String? { message }
}

private struct LocalTokenizer: MLXLMCommon.Tokenizer {
  let tokenizer: any Tokenizers.Tokenizer
  var bosToken: String? { tokenizer.bosToken }
  var eosToken: String? { tokenizer.eosToken }
  var unknownToken: String? { tokenizer.unknownToken }
  func encode(text: String, addSpecialTokens: Bool) -> [Int] {
    tokenizer.encode(text: text, addSpecialTokens: addSpecialTokens)
  }
  func decode(tokenIds: [Int], skipSpecialTokens: Bool) -> String {
    tokenizer.decode(tokens: tokenIds, skipSpecialTokens: skipSpecialTokens)
  }
  func convertTokenToId(_ token: String) -> Int? {
    tokenizer.convertTokenToId(token)
  }
  func convertIdToToken(_ id: Int) -> String? {
    tokenizer.convertIdToToken(id)
  }
  func applyChatTemplate(
    messages: [[String: any Sendable]], tools: [[String: any Sendable]]?,
    additionalContext: [String: any Sendable]?
  ) throws -> [Int] {
    throw BridgeError(message: "Chat templates are not used for embeddings")
  }
}

private struct LocalTokenizerLoader: TokenizerLoader {
  func load(from directory: URL) async throws -> any MLXLMCommon.Tokenizer {
    // Reading the JSON directly also avoids Hub's bundled fallback configs.
    let config = try HubApi.shared.configuration(
      fileURL: directory.appendingPathComponent("tokenizer_config.json"))
    let data = try HubApi.shared.configuration(
      fileURL: directory.appendingPathComponent("tokenizer.json"))
    return LocalTokenizer(
      tokenizer: try AutoTokenizer.from(
        tokenizerConfig: config, tokenizerData: data))
  }
}

final class Shaders: @unchecked Sendable {
  static let shared = Shaders()
  private let lock = NSLock()
  private var ready = false

  // The asset is embedded in the archive so installed executables can move.
  // MLX accepts a filesystem path, so materialize it once per user/version.
  func prepare() throws {
    lock.lock()
    defer { lock.unlock() }
    if ready { return }
    let bytes = Data(
      bytesNoCopy: UnsafeMutableRawPointer(mutating: metallibData()),
      count: metallibSize(), deallocator: .none)
    let digest = SHA256.hash(data: bytes).map { String(format: "%02x", $0) }.joined()
    let manager = FileManager.default
    let base: URL
    if let path = ProcessInfo.processInfo.environment["XDG_CACHE_HOME"], !path.isEmpty {
      base = URL(fileURLWithPath: path, isDirectory: true)
    } else {
      base = try manager.url(
        for: .cachesDirectory, in: .userDomainMask,
        appropriateFor: nil, create: true)
    }
    let directory = base.appendingPathComponent("apple-mlx-embeddings", isDirectory: true)
    try manager.createDirectory(
      at: directory, withIntermediateDirectories: true,
      attributes: [.posixPermissions: 0o700])
    let file = directory.appendingPathComponent("\(digest).metallib")
    let existing = try? Data(contentsOf: file, options: .mappedIfSafe)
    if existing.map({ SHA256.hash(data: $0) }) != SHA256.hash(data: bytes) {
      try bytes.write(to: file, options: .atomic)
    }
    GPU.metallib = file
    // Configure after the shader path, before allocator/model initialization.
    // Variable token widths would otherwise retain free buffers up to RAM size.
    Memory.cacheLimit = min(Memory.cacheLimit, 256 * 1024 * 1024)
    ready = true
  }
}

private final class Engine: Sendable {
  let container: EmbedderModelContainer
  let maxTokens: Int
  let dimensions: Int

  init(directory: URL, maxTokens: Int) async throws {
    let data = try Data(contentsOf: directory.appendingPathComponent("config.json"))
    guard let config = try JSONSerialization.jsonObject(with: data) as? [String: Any],
      config["model_type"] as? String == "qwen3",
      let dimensions = config["hidden_size"] as? Int,
      (32...4096).contains(dimensions)
    else {
      throw BridgeError(message: "Expected a Qwen3 embedding model directory")
    }
    guard (32...32768).contains(maxTokens) else {
      throw BridgeError(message: "max_tokens must be between 32 and 32768")
    }
    let contextLength = config["max_position_embeddings"] as? Int ?? maxTokens
    guard contextLength >= 32 else {
      throw BridgeError(message: "Invalid model context length")
    }
    let index = directory.appendingPathComponent("model.safetensors.index.json")
    if FileManager.default.fileExists(atPath: index.path) {
      let object = try JSONSerialization.jsonObject(with: Data(contentsOf: index))
      guard let object = object as? [String: Any],
        let weights = object["weight_map"] as? [String: String]
      else {
        throw BridgeError(message: "Invalid safetensors weight index")
      }
      // The retrieval identity hashes visible files beneath this directory.
      // An index must not introduce weights outside that fingerprint.
      for name in weights.values {
        let components = name.split(separator: "/", omittingEmptySubsequences: false)
        guard !name.hasPrefix("/"), name.hasSuffix(".safetensors"),
          components.allSatisfy({ !$0.isEmpty && !$0.hasPrefix(".") })
        else {
          throw BridgeError(message: "Weight shards must stay inside the model directory")
        }
      }
    }
    guard MTLCreateSystemDefaultDevice() != nil else {
      throw BridgeError(message: "MLX embeddings require an Apple GPU")
    }
    try Shaders.shared.prepare()
    self.dimensions = dimensions
    self.maxTokens = min(maxTokens, contextLength)
    self.container = try await EmbedderModelFactory.shared.loadContainer(
      from: directory, using: LocalTokenizerLoader())
  }

  func embed(_ texts: [String], dimensions: Int) async throws -> [Float] {
    guard (32...self.dimensions).contains(dimensions) else {
      throw BridgeError(message: "Embedding dimensions exceed the model's output width")
    }
    let maxTokens = self.maxTokens
    return try await container.perform { context in
      let rows = try texts.map { text -> [Int32] in
        let tokens = context.tokenizer.encode(text: text, addSpecialTokens: true)
        guard !tokens.isEmpty else {
          throw BridgeError(message: "Embedding input contains no tokens")
        }
        return tokens.prefix(maxTokens).map { Int32($0) }
      }
      let width = rows.map(\.count).max()!
      let pad = Int32(context.tokenizer.eosTokenId ?? 0)
      var ids = [Int32]()
      var masks = [Int32]()
      ids.reserveCapacity(rows.count * width)
      masks.reserveCapacity(rows.count * width)
      for row in rows {
        ids.append(contentsOf: row)
        ids.append(contentsOf: repeatElement(pad, count: width - row.count))
        masks.append(contentsOf: repeatElement(1, count: row.count))
        masks.append(contentsOf: repeatElement(0, count: width - row.count))
      }
      // Right padding matches MLXEmbedders' final-token pooling indices.
      let input = MLXArray(ids, [rows.count, width])
      let mask = MLXArray(masks, [rows.count, width])
      let output = context.model(
        input, positionIds: nil, tokenTypeIds: nil,
        attentionMask: mask)
      let pooled = context.pooling(output, mask: mask, normalize: false)
      guard pooled.ndim == 2, pooled.dim(0) == rows.count,
        pooled.dim(1) >= dimensions
      else {
        throw BridgeError(message: "Model returned an invalid embedding shape")
      }
      let vectors = pooled[0..., 0..<dimensions].asType(.float32)
      let norm = MLX.sqrt(MLX.sum(vectors * vectors, axis: 1, keepDims: true))
      let normalized = vectors / norm
      normalized.eval()
      let result = normalized.asArray(Float.self)
      guard result.allSatisfy(\.isFinite) else {
        throw BridgeError(message: "Model returned a zero or nonfinite embedding")
      }
      return result
    }
  }
}

private final class ResultBox<T: Sendable>: @unchecked Sendable {
  var result: Result<T, Error>?
}

func wait<T: Sendable>(
  _ operation: @escaping @Sendable () async throws -> T
) throws -> T {
  let semaphore = DispatchSemaphore(value: 0)
  let box = ResultBox<T>()
  Task {
    do { box.result = .success(try await operation()) } catch { box.result = .failure(error) }
    semaphore.signal()
  }
  semaphore.wait()
  return try box.result!.get()
}

func failure(_ error: Error, _ output: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?)
  -> Int32
{
  output?.pointee = strdup(error.localizedDescription)
  return 1
}

@_cdecl("ame_available")
public func available() -> Int32 {
  #if arch(arm64)
    return MTLCreateSystemDefaultDevice() == nil ? 0 : 1
  #else
    return 0
  #endif
}

@_cdecl("ame_memory_stats")
public func memoryStats(
  _ active: UnsafeMutablePointer<Int64>?, _ cache: UnsafeMutablePointer<Int64>?,
  _ peak: UnsafeMutablePointer<Int64>?, _ limit: UnsafeMutablePointer<Int64>?
) -> Int32 {
  guard MTLCreateSystemDefaultDevice() != nil,
    let active, let cache, let peak, let limit else { return 1 }
  do { try Shaders.shared.prepare() } catch { return 1 }
  active.pointee = Int64(Memory.activeMemory)
  cache.pointee = Int64(Memory.cacheMemory)
  peak.pointee = Int64(Memory.peakMemory)
  limit.pointee = Int64(Memory.cacheLimit)
  return 0
}

@_cdecl("ame_load")
public func load(
  _ path: UnsafePointer<CChar>?, _ maxTokens: Int32,
  _ output: UnsafeMutablePointer<UnsafeMutableRawPointer?>?,
  _ error: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  do {
    guard let path, let path = String(validatingCString: path), let output else {
      throw BridgeError(message: "Invalid model directory")
    }
    let directory = URL(fileURLWithPath: path, isDirectory: true)
    let engine = try wait { try await Engine(directory: directory, maxTokens: Int(maxTokens)) }
    output.pointee = Unmanaged.passRetained(engine).toOpaque()
    return 0
  } catch let problem { return failure(problem, error) }
}

@_cdecl("ame_release")
public func release(_ handle: UnsafeMutableRawPointer?) {
  if let handle { Unmanaged<Engine>.fromOpaque(handle).release() }
}

@_cdecl("ame_embed")
public func embed(
  _ handle: UnsafeMutableRawPointer?, _ inputs: UnsafePointer<UnsafePointer<CChar>?>?,
  _ count: Int32, _ dimensions: Int32, _ output: UnsafeMutablePointer<Float>?,
  _ error: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  do {
    guard let handle, let inputs, let output, (1...16).contains(count) else {
      throw BridgeError(message: "Invalid embedding batch")
    }
    let texts = try (0..<Int(count)).map { index -> String in
      guard let pointer = inputs[index], let text = String(validatingCString: pointer) else {
        throw BridgeError(message: "Embedding input is not valid UTF-8")
      }
      return text
    }
    let engine = Unmanaged<Engine>.fromOpaque(handle).takeUnretainedValue()
    let values = try wait { try await engine.embed(texts, dimensions: Int(dimensions)) }
    guard values.count == Int(count) * Int(dimensions) else {
      throw BridgeError(message: "Embedding result has an invalid size")
    }
    values.withUnsafeBufferPointer { source in
      output.update(from: source.baseAddress!, count: source.count)
    }
    return 0
  } catch let problem { return failure(problem, error) }
}
