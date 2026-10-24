import Foundation
import Hub
import ImageIO
import MLX
import MLXLMCommon
import MLXNN
import MLXVLM
import Metal
import Tokenizers

private struct VisionTokenizer: MLXLMCommon.Tokenizer {
  let upstream: any Tokenizers.Tokenizer
  let embeddingEndToken: Int
  var bosToken: String? { upstream.bosToken }
  var eosToken: String? { upstream.eosToken }
  var unknownToken: String? { upstream.unknownToken }
  func encode(text: String, addSpecialTokens: Bool) -> [Int] {
    // Qwen's image-placeholder expansion encodes fragments. The embedding
    // checkpoint's EOS postprocessor applies to the complete prompt only.
    upstream.encode(text: text, addSpecialTokens: false)
  }
  func decode(tokenIds: [Int], skipSpecialTokens: Bool) -> String {
    upstream.decode(tokens: tokenIds, skipSpecialTokens: skipSpecialTokens)
  }
  func convertTokenToId(_ token: String) -> Int? { upstream.convertTokenToId(token) }
  func convertIdToToken(_ id: Int) -> String? { upstream.convertIdToToken(id) }
  func applyChatTemplate(
    messages: [[String: any Sendable]], tools: [[String: any Sendable]]?,
    additionalContext: [String: any Sendable]?
  ) throws -> [Int] {
    let prompt = try upstream.applyChatTemplate(
      messages: messages, tools: tools, additionalContext: additionalContext)
    // Swift chat templating skips tokenizer postprocessors. Qwen3-VL-Embedding
    // pools the endoftext token appended after the assistant generation prompt.
    return prompt + [embeddingEndToken]
  }

  init(directory: URL) throws {
    let config = try HubApi.shared.configuration(
      fileURL: directory.appendingPathComponent("tokenizer_config.json"))
    let data = try HubApi.shared.configuration(
      fileURL: directory.appendingPathComponent("tokenizer.json"))
    var values = config.dictionary(or: [:])
    values["chat_template"] = Config(try String(
      contentsOf: directory.appendingPathComponent("chat_template.jinja"), encoding: .utf8))
    upstream = try AutoTokenizer.from(tokenizerConfig: Config(values), tokenizerData: data)
    guard let end = upstream.convertTokenToId("<|endoftext|>") else {
      throw BridgeError(message: "Vision embedding tokenizer is missing endoftext")
    }
    embeddingEndToken = end
  }
}

private final class VisionEngine: Sendable {
  let container: ModelContainer
  let maxTokens: Int
  let dimensions: Int

  init(directory: URL, maxTokens: Int) async throws {
    guard (32...32768).contains(maxTokens) else {
      throw BridgeError(message: "max_tokens must be between 32 and 32768")
    }
    let data = try Data(contentsOf: directory.appendingPathComponent("config.json"))
    let base = try JSONDecoder().decode(BaseConfiguration.self, from: data)
    guard base.modelType == "qwen3_vl" else {
      throw BridgeError(message: "Expected a Qwen3-VL embedding model directory")
    }
    let config = try JSONDecoder().decode(Qwen3VLConfiguration.self, from: data)
    let text = config.textConfiguration
    let vision = config.visionConfiguration
    guard (32...4096).contains(text.hiddenSize), text.vocabSize > 0,
      text.maxPositionEmbeddings >= 32, (1...128).contains(text.numHiddenLayers),
      (1...128).contains(text.numAttentionHeads), text.numKeyValueHeads > 0,
      text.numAttentionHeads % text.numKeyValueHeads == 0,
      text.headDim > 0, vision.patchSize == 16, vision.spatialMergeSize == 2,
      vision.temporalPatchSize == 2
    else { throw BridgeError(message: "Invalid Qwen3-VL model configuration") }
    let processorConfig = try JSONDecoder().decode(
      Qwen3VLProcessorConfiguration.self,
      from: Data(contentsOf: directory.appendingPathComponent("preprocessor_config.json")))
    guard processorConfig.patchSize == 16, processorConfig.mergeSize == 2,
      processorConfig.temporalPatchSize == 2,
      processorConfig.imageMean.count == 3, processorConfig.imageStd.count == 3,
      processorConfig.imageMean.allSatisfy(\.isFinite),
      processorConfig.imageStd.allSatisfy({ $0.isFinite && $0 > 0 })
    else { throw BridgeError(message: "Invalid Qwen3-VL image processor configuration") }
    let index = directory.appendingPathComponent("model.safetensors.index.json")
    if FileManager.default.fileExists(atPath: index.path) {
      guard let object = try JSONSerialization.jsonObject(with: Data(contentsOf: index))
        as? [String: Any], let weights = object["weight_map"] as? [String: String]
      else { throw BridgeError(message: "Invalid safetensors weight index") }
      for name in weights.values {
        let parts = name.split(separator: "/", omittingEmptySubsequences: false)
        guard !name.hasPrefix("/"), name.hasSuffix(".safetensors"),
          parts.allSatisfy({ !$0.isEmpty && !$0.hasPrefix(".") })
        else { throw BridgeError(message: "Weight shards must stay inside the model directory") }
      }
    }
    guard MTLCreateSystemDefaultDevice() != nil else {
      throw BridgeError(message: "MLX embeddings require an Apple GPU")
    }
    try Shaders.shared.prepare()
    let tokenizer = try VisionTokenizer(directory: directory)
    let model = Qwen3VL(config)
    model.prepareForEmbeddingInference()
    try await loadWeights(
      modelDirectory: directory, model: model,
      perLayerQuantization: base.perLayerQuantization)
    dimensions = text.hiddenSize
    self.maxTokens = min(maxTokens, text.maxPositionEmbeddings)
    container = ModelContainer(context: ModelContext(
      configuration: ModelConfiguration(directory: directory), model: model,
      processor: Qwen3VLProcessor(processorConfig, tokenizer: tokenizer), tokenizer: tokenizer))
  }

  private static func instruction(_ text: String) -> String {
    var value = text.trimmingCharacters(in: .whitespacesAndNewlines)
    if value.isEmpty { return "Represent the user's input." }
    if let last = value.unicodeScalars.last, !CharacterSet.punctuationCharacters.contains(last) {
      value += "."
    }
    return value
  }

  private static func image(_ path: String) throws -> UserInput.Image {
    let url = URL(fileURLWithPath: path)
    let values = try url.resourceValues(forKeys: [.isRegularFileKey, .fileSizeKey])
    guard values.isRegularFile == true, let bytes = values.fileSize,
      bytes > 0, bytes <= 64 * 1024 * 1024,
      let source = CGImageSourceCreateWithURL(url as CFURL, nil),
      let properties = CGImageSourceCopyPropertiesAtIndex(source, 0, nil) as? [CFString: Any],
      let width = properties[kCGImagePropertyPixelWidth] as? Int,
      let height = properties[kCGImagePropertyPixelHeight] as? Int,
      width > 0, height > 0, width <= 16384, height <= 16384,
      width <= 100_000_000 / height
    else { throw BridgeError(message: "Expected a local image within the supported size limits") }
    return .url(url)
  }

  func embed(_ inputs: [String], images: Bool, instruction: String, dimensions: Int)
    async throws -> [Float]
  {
    guard (64...self.dimensions).contains(dimensions) else {
      throw BridgeError(message: "Embedding dimensions exceed the model's output width")
    }
    let instruction = Self.instruction(instruction)
    let maxTokens = self.maxTokens
    return try await container.perform { context in
      guard let model = context.model as? Qwen3VL else {
        throw BridgeError(message: "Invalid vision embedding model")
      }
      var result = [Float]()
      // Each row has its own image grid and M-RoPE positions. Keeping rows
      // separate bounds vision scratch and avoids cross-row padding effects.
      for value in inputs {
        let user: Chat.Message = images
          ? .user("", images: [try Self.image(value)]) : .user(value)
        let userInput = UserInput(
          chat: [.system(instruction), user],
          processing: .init(minPixels: 4096, maxPixels: 1_843_200))
        var input = try await context.processor.prepare(input: userInput)
        if input.text.tokens.dim(-1) > maxTokens {
          guard !images else {
            throw BridgeError(message: "Image tokens exceed max_tokens; increase the token limit")
          }
          // Hugging Face truncates the rendered prompt before its tokenizer
          // postprocessor appends <|endoftext|>. Preserve that pooling token.
          input = LMInput(tokens: concatenated([
            input.text.tokens[0..., 0..<(maxTokens - 1)],
            input.text.tokens[0..., (-1)...],
          ], axis: -1))
        }
        let ids = input.text.tokens.asArray(Int32.self)
        guard !ids.isEmpty else { throw BridgeError(message: "Empty vision embedding input") }
        if !images && ids.contains(where: { $0 == 151655 || $0 == 151656 || $0 == 151652 }) {
          throw BridgeError(message: "Text contains reserved image or video tokens")
        }
        let hidden = try model.embeddingHiddenStates(input)
        guard hidden.ndim == 3, hidden.dim(0) == 1, hidden.dim(2) >= dimensions else {
          throw BridgeError(message: "Invalid vision embedding output shape")
        }
        let vector = hidden[0, -1, 0..<dimensions].asType(.float32)
        let normalized = vector / MLX.sqrt(MLX.sum(vector * vector))
        normalized.eval()
        let row = normalized.asArray(Float.self)
        guard row.allSatisfy(\.isFinite) else {
          throw BridgeError(message: "Model returned a zero or nonfinite embedding")
        }
        result.append(contentsOf: row)
      }
      return result
    }
  }
}

@_cdecl("ame_vision_load")
public func visionLoad(
  _ path: UnsafePointer<CChar>?, _ maxTokens: Int32,
  _ output: UnsafeMutablePointer<UnsafeMutableRawPointer?>?,
  _ error: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  do {
    guard let path, let path = String(validatingCString: path), let output else {
      throw BridgeError(message: "Invalid vision model directory")
    }
    let directory = URL(fileURLWithPath: path, isDirectory: true)
    let model = try wait { try await VisionEngine(directory: directory, maxTokens: Int(maxTokens)) }
    output.pointee = Unmanaged.passRetained(model).toOpaque()
    return 0
  } catch let problem { return failure(problem, error) }
}

@_cdecl("ame_vision_release")
public func visionRelease(_ handle: UnsafeMutableRawPointer?) {
  if let handle { Unmanaged<VisionEngine>.fromOpaque(handle).release() }
}

@_cdecl("ame_vision_embed")
public func visionEmbed(
  _ handle: UnsafeMutableRawPointer?, _ images: Int32, _ instruction: UnsafePointer<CChar>?,
  _ inputs: UnsafePointer<UnsafePointer<CChar>?>?, _ count: Int32, _ dimensions: Int32,
  _ output: UnsafeMutablePointer<Float>?,
  _ error: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  do {
    guard let handle, let inputs, let output, let instruction,
      let instruction = String(validatingCString: instruction), (1...2).contains(count)
    else { throw BridgeError(message: "Invalid vision embedding batch") }
    let values = try (0..<Int(count)).map { index -> String in
      guard let pointer = inputs[index], let text = String(validatingCString: pointer) else {
        throw BridgeError(message: "Vision input is not valid UTF-8")
      }
      return text
    }
    let model = Unmanaged<VisionEngine>.fromOpaque(handle).takeUnretainedValue()
    let vectors = try wait {
      try await model.embed(values, images: images != 0, instruction: instruction,
                            dimensions: Int(dimensions))
    }
    guard vectors.count == Int(count) * Int(dimensions) else {
      throw BridgeError(message: "Invalid vision embedding result size")
    }
    vectors.withUnsafeBufferPointer { output.update(from: $0.baseAddress!, count: $0.count) }
    return 0
  } catch let problem { return failure(problem, error) }
}
