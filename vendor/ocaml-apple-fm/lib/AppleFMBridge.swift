import Darwin
import Foundation
import FoundationModels

private enum BridgeError: LocalizedError {
  case invalidUTF8, busy, closed
  case invalidJSON(String)
  case invalidSchema(String)
  case unsupported(String)
  case unknownToolCall(Int64)

  var errorDescription: String? {
    switch self {
    case .invalidUTF8: return "invalid UTF-8 passed to the Foundation Models bridge"
    case .invalidJSON(let s): return "invalid bridge JSON: \(s)"
    case .invalidSchema(let s): return "invalid generation schema: \(s)"
    case .unsupported(let s): return s
    case .busy: return "the session is already responding"
    case .closed: return "the session is closed"
    case .unknownToolCall(let id): return "unknown tool call \(id)"
    }
  }
}

private struct ErrorEnvelope: Codable {
  var kind: String
  var message: String
  var contextSize: Int? = nil
  var tokenCount: Int? = nil
  var resetTime: Double? = nil
  var detail: String? = nil
}

@available(macOS, introduced: 26.0, deprecated: 27.0)
private func legacyEnvelope(_ error: Error) -> ErrorEnvelope? {
  guard let error = error as? LanguageModelSession.GenerationError else { return nil }
  let message = error.localizedDescription
  switch error {
  case .exceededContextWindowSize: return .init(kind: "context_size_exceeded", message: message)
  case .assetsUnavailable: return .init(kind: "assets_unavailable", message: message)
  case .guardrailViolation: return .init(kind: "guardrail_violation", message: message)
  case .unsupportedGuide: return .init(kind: "unsupported_guide", message: message)
  case .unsupportedLanguageOrLocale: return .init(kind: "unsupported_language", message: message)
  case .decodingFailure: return .init(kind: "decoding_failure", message: message)
  case .rateLimited: return .init(kind: "rate_limited", message: message)
  case .concurrentRequests: return .init(kind: "concurrent_requests", message: message)
  case .refusal: return .init(kind: "refusal", message: message)
  @unknown default: return .init(kind: "framework", message: message)
  }
}

private func envelope(_ error: Error) -> ErrorEnvelope {
  let message = error.localizedDescription
  if error is CancellationError { return .init(kind: "cancelled", message: message) }
  switch error {
  case BridgeError.closed: return .init(kind: "closed", message: message)
  case BridgeError.busy: return .init(kind: "concurrent_requests", message: message)
  case BridgeError.unsupported: return .init(kind: "unsupported_version", message: message)
  default: break
  }
  if #available(macOS 27.0, *) {
    if let e = error as? LanguageModelError {
      switch e {
      case .contextSizeExceeded(let c):
        return .init(
          kind: "context_size_exceeded", message: message,
          contextSize: c.contextSize, tokenCount: c.tokenCount)
      case .rateLimited(let c):
        return .init(
          kind: "rate_limited", message: message,
          resetTime: c.resetDate?.timeIntervalSince1970)
      case .guardrailViolation: return .init(kind: "guardrail_violation", message: message)
      case .refusal: return .init(kind: "refusal", message: message)
      case .unsupportedCapability(let c):
        return .init(
          kind: "unsupported_capability", message: message,
          detail: String(describing: c.capability))
      case .unsupportedTranscriptContent:
        return .init(kind: "unsupported_transcript", message: message)
      case .unsupportedGenerationGuide(let c):
        return .init(kind: "unsupported_guide", message: message, detail: c.schemaName)
      case .unsupportedLanguageOrLocale(let c):
        return .init(
          kind: "unsupported_language", message: message,
          detail: String(describing: c.languageCode))
      case .timeout: return .init(kind: "timeout", message: message)
      @unknown default: return .init(kind: "framework", message: message)
      }
    }
    if let e = error as? LanguageModelSession.Error {
      switch e {
      case .concurrentRequests: return .init(kind: "concurrent_requests", message: message)
      case .transcriptMutationWhileResponding:
        return .init(kind: "transcript_mutation", message: message)
      @unknown default: return .init(kind: "framework", message: message)
      }
    }
    if error is SystemLanguageModel.Error {
      return .init(kind: "assets_unavailable", message: message)
    }
    if error is GeneratedContent.ParsingError {
      return .init(kind: "decoding_failure", message: message)
    }
  }
  if #unavailable(macOS 27.0) {
    if let legacy = legacyEnvelope(error) { return legacy }
  }
  if error is LanguageModelSession.ToolCallError {
    return .init(kind: "tool_failure", message: message)
  }
  return .init(kind: "framework", message: message)
}

private func encode<T: Encodable>(_ value: T) throws -> String {
  let encoder = JSONEncoder()
  encoder.outputFormatting = [.sortedKeys]
  return String(decoding: try encoder.encode(value), as: UTF8.self)
}

private func decode<T: Decodable>(_ type: T.Type, _ text: String) throws -> T {
  guard let data = text.data(using: .utf8) else { throw BridgeError.invalidUTF8 }
  do { return try JSONDecoder().decode(type, from: data) } catch {
    throw BridgeError.invalidJSON(error.localizedDescription)
  }
}

private func errorString(_ error: Error) -> String {
  (try? encode(envelope(error)))
    ?? "{\"kind\":\"framework\",\"message\":\"unable to encode framework error\"}"
}

private indirect enum SchemaNode: Decodable {
  case null
  case string(String?, [String]?, String?)
  case integer(Int?, Int?)
  case number(Double?, Double?)
  case boolean
  case array(SchemaNode, Int?, Int?)
  case object(String, String?, Bool, [SchemaProperty])
  case oneOf(String, String?, [String])
  case anyOf(String, String?, [SchemaNode])
  case reference(String)

  private enum Keys: String, CodingKey {
    case type, name, description, items, properties, choices, constant, pattern
    case minimum, maximum
    case minimumItems = "minimum_items"
    case maximumItems = "maximum_items"
    case explicitNull = "explicit_null"
  }

  init(from decoder: Decoder) throws {
    let v = try decoder.container(keyedBy: Keys.self)
    let type = try v.decode(String.self, forKey: .type)
    switch type {
    case "null": self = .null
    case "string":
      self = .string(
        try v.decodeIfPresent(String.self, forKey: .constant),
        try v.decodeIfPresent([String].self, forKey: .choices),
        try v.decodeIfPresent(String.self, forKey: .pattern))
    case "integer":
      self = .integer(
        try v.decodeIfPresent(Int.self, forKey: .minimum),
        try v.decodeIfPresent(Int.self, forKey: .maximum))
    case "number":
      self = .number(
        try v.decodeIfPresent(Double.self, forKey: .minimum),
        try v.decodeIfPresent(Double.self, forKey: .maximum))
    case "boolean": self = .boolean
    case "array":
      self = .array(
        try v.decode(SchemaNode.self, forKey: .items),
        try v.decodeIfPresent(Int.self, forKey: .minimumItems),
        try v.decodeIfPresent(Int.self, forKey: .maximumItems))
    case "object":
      self = .object(
        try v.decode(String.self, forKey: .name),
        try v.decodeIfPresent(String.self, forKey: .description),
        try v.decodeIfPresent(Bool.self, forKey: .explicitNull) ?? false,
        try v.decode([SchemaProperty].self, forKey: .properties))
    case "any_of":
      self = .anyOf(
        try v.decode(String.self, forKey: .name),
        try v.decodeIfPresent(String.self, forKey: .description),
        try v.decode([SchemaNode].self, forKey: .choices))
    case "one_of":
      self = .oneOf(
        try v.decode(String.self, forKey: .name),
        try v.decodeIfPresent(String.self, forKey: .description),
        try v.decode([String].self, forKey: .choices))
    case "reference": self = .reference(try v.decode(String.self, forKey: .name))
    default: throw BridgeError.invalidSchema("unknown type \(type)")
    }
  }

  func dynamic() throws -> DynamicGenerationSchema {
    switch self {
    case .null:
      if #available(macOS 26.4, *) { return .null }
      throw BridgeError.unsupported("null schemas require macOS 26.4 or later")
    case .string(let constant, let choices, let pattern):
      var guides: [GenerationGuide<String>] = []
      if let constant { guides.append(.constant(constant)) }
      if let choices { guides.append(.anyOf(choices)) }
      if let pattern { guides.append(.pattern(try Regex(pattern))) }
      return DynamicGenerationSchema(type: String.self, guides: guides)
    case .integer(let min, let max):
      var guides: [GenerationGuide<Int>] = []
      if let min, let max {
        guides.append(.range(min...max))
      } else if let min {
        guides.append(.minimum(min))
      } else if let max {
        guides.append(.maximum(max))
      }
      return DynamicGenerationSchema(type: Int.self, guides: guides)
    case .number(let min, let max):
      var guides: [GenerationGuide<Double>] = []
      if let min, let max {
        guides.append(.range(min...max))
      } else if let min {
        guides.append(.minimum(min))
      } else if let max {
        guides.append(.maximum(max))
      }
      return DynamicGenerationSchema(type: Double.self, guides: guides)
    case .boolean: return DynamicGenerationSchema(type: Bool.self)
    case .array(let item, let min, let max):
      return DynamicGenerationSchema(
        arrayOf: try item.dynamic(),
        minimumElements: min, maximumElements: max)
    case .object(let name, let description, let explicitNull, let properties):
      let properties = try properties.map { try $0.dynamic() }
      if explicitNull {
        if #available(macOS 26.4, *) {
          return DynamicGenerationSchema(
            name: name, description: description,
            representNilExplicitlyInGeneratedContent: true, properties: properties)
        }
        throw BridgeError.unsupported("explicit null fields require macOS 26.4 or later")
      }
      return DynamicGenerationSchema(
        name: name, description: description,
        properties: properties)
    case .oneOf(let name, let description, let choices):
      return DynamicGenerationSchema(name: name, description: description, anyOf: choices)
    case .anyOf(let name, let description, let choices):
      return DynamicGenerationSchema(
        name: name, description: description,
        anyOf: try choices.map { try $0.dynamic() })
    case .reference(let name): return DynamicGenerationSchema(referenceTo: name)
    }
  }
}

private struct SchemaProperty: Decodable {
  let name: String, description: String?
  let optional: Bool, schema: SchemaNode
  func dynamic() throws -> DynamicGenerationSchema.Property {
    DynamicGenerationSchema.Property(
      name: name, description: description,
      schema: try schema.dynamic(), isOptional: optional)
  }
}

private struct SchemaDocument: Decodable {
  let root: SchemaNode, dependencies: [SchemaNode]
  func generationSchema() throws -> GenerationSchema {
    try GenerationSchema(
      root: root.dynamic(),
      dependencies: dependencies.map { try $0.dynamic() })
  }
}

private struct ToolDefinition: Decodable {
  let name: String, description: String
  let schema: SchemaDocument
  let includesSchemaInInstructions: Bool
}

private struct ModelConfiguration: Codable {
  var useCase = "general", guardrails = "default"
}

private func systemModel(_ c: ModelConfiguration) -> SystemLanguageModel {
  SystemLanguageModel(
    useCase: c.useCase == "content_tagging" ? .contentTagging : .general,
    guardrails: c.guardrails == "permissive_content_transformations"
      ? .permissiveContentTransformations : .default)
}

private struct ImageInput: Decodable { let path: String, label: String? }
private struct PromptInput: Decodable { let text: String, images: [ImageInput] }

@available(macOS 27.0, *)
private func makePrompt(_ input: PromptInput) -> Prompt {
  Prompt {
    input.text
    for image in input.images {
      if let label = image.label {
        Attachment(imageURL: URL(fileURLWithPath: image.path)).label(label)
      } else {
        Attachment(imageURL: URL(fileURLWithPath: image.path))
      }
    }
  }
}

private enum Event {
  case text(String)
  case tool(Int64, String, String)
  case done(String)
  case error(String)
}

private final class EventQueue: @unchecked Sendable {
  private let condition = NSCondition()
  private var events: [Event] = []
  private var waiters: [Int64: CheckedContinuation<String, Error>] = [:]
  private var nextID: Int64 = 1
  private var responding = false, closed = false

  func beginResponse() throws {
    condition.lock()
    defer { condition.unlock() }
    if closed { throw BridgeError.closed }
    if responding { throw BridgeError.busy }
    responding = true
  }
  func finishResponse() {
    condition.lock()
    responding = false
    condition.broadcast()
    condition.unlock()
  }
  func push(_ event: Event) {
    condition.lock()
    if !closed {
      events.append(event)
      condition.signal()
    }
    condition.unlock()
  }
  func next(timeoutMilliseconds: Int32) -> Event? {
    condition.lock()
    defer { condition.unlock() }
    if events.isEmpty && !closed {
      if timeoutMilliseconds < 0 {
        while events.isEmpty && !closed { condition.wait() }
      } else if timeoutMilliseconds > 0 {
        let end = Date(timeIntervalSinceNow: Double(timeoutMilliseconds) / 1000.0)
        while events.isEmpty && !closed && condition.wait(until: end) {}
      }
    }
    return events.isEmpty ? nil : events.removeFirst()
  }
  func requestTool(name: String, arguments: String) async throws -> String {
    let id = condition.withLock {
      let id = nextID
      nextID += 1
      return id
    }
    return try await withTaskCancellationHandler {
      try await withCheckedThrowingContinuation { continuation in
        condition.lock()
        if closed || Task.isCancelled {
          condition.unlock()
          continuation.resume(throwing: CancellationError())
          return
        }
        waiters[id] = continuation
        events.append(.tool(id, name, arguments))
        condition.signal()
        condition.unlock()
      }
    } onCancel: {
      self.cancelTool(id)
    }
  }
  private func cancelTool(_ id: Int64) {
    condition.lock()
    let c = waiters.removeValue(forKey: id)
    condition.unlock()
    c?.resume(throwing: CancellationError())
  }
  func resolveTool(id: Int64, result: String) throws {
    condition.lock()
    let c = waiters.removeValue(forKey: id)
    condition.unlock()
    guard let c else { throw BridgeError.unknownToolCall(id) }
    c.resume(returning: result)
  }
  func cancelTools() {
    condition.lock()
    let pending = Array(waiters.values)
    waiters.removeAll()
    condition.unlock()
    for continuation in pending {
      continuation.resume(throwing: CancellationError())
    }
  }
  func close() {
    condition.lock()
    closed = true
    let pending = Array(waiters.values)
    waiters.removeAll()
    condition.broadcast()
    condition.unlock()
    for continuation in pending {
      continuation.resume(throwing: BridgeError.closed)
    }
  }
}

private struct DynamicTool: Tool, @unchecked Sendable {
  typealias Arguments = GeneratedContent
  typealias Output = GeneratedContent
  let name: String, description: String
  let parameters: GenerationSchema
  let includesSchemaInInstructions: Bool
  let queue: EventQueue
  func call(arguments: GeneratedContent) async throws -> GeneratedContent {
    let result = try await queue.requestTool(name: name, arguments: arguments.jsonString)
    return try GeneratedContent(json: result)
  }
}

private func makeTools(_ definitions: [ToolDefinition], queue: EventQueue) throws -> [any Tool] {
  try definitions.map {
    DynamicTool(
      name: $0.name, description: $0.description,
      parameters: try $0.schema.generationSchema(),
      includesSchemaInInstructions: $0.includesSchemaInInstructions, queue: queue)
  }
}

private func makeOptions(
  _ kind: Int32, _ value: Double, _ seed: UInt64?,
  _ temperature: Double?, _ maximum: Int?, _ toolMode: Int32
) throws -> GenerationOptions {
  let sampling: GenerationOptions.SamplingMode?
  switch kind {
  case 1: sampling = .greedy
  case 2: sampling = .random(top: Int(value), seed: seed)
  case 3: sampling = .random(probabilityThreshold: value, seed: seed)
  default: sampling = nil
  }
  var options = GenerationOptions(
    samplingMode: sampling, temperature: temperature,
    maximumResponseTokens: maximum)
  if #available(macOS 27.0, *) {
    switch toolMode {
    case 1: options.toolCallingMode = .required
    case 2: options.toolCallingMode = .disallowed
    default: options.toolCallingMode = .allowed
    }
  } else if toolMode != 0 {
    throw BridgeError.unsupported("tool-calling policy requires macOS 27 or later")
  }
  return options
}

@available(macOS 27.0, *)
private func makeContext(_ includeSchema: Int32, _ reasoning: String?) -> ContextOptions {
  let include = includeSchema < 0 ? nil : includeSchema != 0
  let level: ContextOptions.ReasoningLevel?
  switch reasoning {
  case "light": level = .light
  case "moderate": level = .moderate
  case "deep": level = .deep
  case .some(let value) where value.hasPrefix("custom:"):
    level = .custom(String(value.dropFirst(7)))
  default: level = nil
  }
  return ContextOptions(includeSchemaInPrompt: include, reasoningLevel: level)
}

private struct UsageWire: Codable {
  let inputTokens, cachedInputTokens, outputTokens, reasoningTokens: Int
}

private final class BridgeSession: @unchecked Sendable {
  let queue: EventQueue
  let session: LanguageModelSession
  private let lock = NSLock()
  private var task: Task<Void, Never>?

  init(
    instructions: String?, definitions: [ToolDefinition], config: ModelConfiguration,
    transcript: Transcript?
  ) throws {
    let queue = EventQueue()
    let model = systemModel(config)
    let tools = try makeTools(definitions, queue: queue)
    self.queue = queue
    if let transcript {
      session = LanguageModelSession(model: model, tools: tools, transcript: transcript)
    } else {
      session = LanguageModelSession(model: model, tools: tools, instructions: instructions)
    }
  }

  func prewarm(_ prefix: String?) { session.prewarm(promptPrefix: prefix.map(Prompt.init)) }

  func start(
    prompt input: PromptInput, schema: SchemaDocument?, includeSchema: Int32,
    reasoning: String?, samplingKind: Int32, samplingValue: Double, seed: UInt64?,
    temperature: Double?, maximum: Int?, toolMode: Int32
  ) throws {
    try queue.beginResponse()
    do {
      let options = try makeOptions(
        samplingKind, samplingValue, seed, temperature,
        maximum, toolMode)
      let session = self.session
      let queue = self.queue
      let task = Task {
        do {
          if let schema {
            let schema = try schema.generationSchema()
            let response: LanguageModelSession.Response<GeneratedContent>
            if #available(macOS 27.0, *) {
              response = try await session.respond(
                to: makePrompt(input), schema: schema,
                options: options, contextOptions: makeContext(includeSchema, reasoning))
            } else {
              guard input.images.isEmpty else {
                throw BridgeError.unsupported("image prompts require macOS 27 or later")
              }
              response = try await session.respond(
                to: input.text, schema: schema,
                includeSchemaInPrompt: includeSchema != 0, options: options)
            }
            queue.finishResponse()
            queue.push(.done(response.content.jsonString))
          } else {
            let stream: LanguageModelSession.ResponseStream<String>
            if #available(macOS 27.0, *) {
              stream = session.streamResponse(
                to: makePrompt(input), options: options,
                contextOptions: makeContext(includeSchema, reasoning))
            } else {
              guard input.images.isEmpty else {
                throw BridgeError.unsupported("image prompts require macOS 27 or later")
              }
              guard includeSchema < 0 && reasoning == nil else {
                throw BridgeError.unsupported("context options require macOS 27 or later")
              }
              stream = session.streamResponse(to: input.text, options: options)
            }
            var previous = ""
            for try await snapshot in stream {
              let current = snapshot.content
              let delta =
                current.hasPrefix(previous)
                ? String(current.dropFirst(previous.count)) : current
              previous = current
              if !delta.isEmpty { queue.push(.text(delta)) }
            }
            queue.finishResponse()
            queue.push(.done(previous))
          }
        } catch {
          queue.finishResponse()
          queue.push(.error(errorString(error)))
        }
      }
      lock.lock()
      self.task = task
      lock.unlock()
    } catch {
      queue.finishResponse()
      throw error
    }
  }

  func transcriptJSON() throws -> String { try encode(session.transcript) }
  func replaceTranscript(_ value: Transcript) throws {
    guard #available(macOS 27.0, *) else {
      throw BridgeError.unsupported("transcript mutation requires macOS 27 or later")
    }
    session.transcript = value
  }
  func usageJSON() throws -> String {
    guard #available(macOS 27.0, *) else { return "null" }
    let u = session.usage
    return try encode(
      UsageWire(
        inputTokens: u.input.totalTokenCount,
        cachedInputTokens: u.input.cachedTokenCount,
        outputTokens: u.output.totalTokenCount,
        reasoningTokens: u.output.reasoningTokenCount))
  }
  func cancel() {
    lock.lock()
    let current = task
    lock.unlock()
    current?.cancel()
    queue.cancelTools()
  }
  func close() {
    cancel()
    queue.close()
  }
}

private struct ModelInfo: Codable {
  let contextSize: Int
  let supportedLanguages: [String]
  let variant: String?
  let vision, guidedGeneration, reasoning, toolCalling: Bool
  let localeSupported: Bool?
}

private func copyCString(_ value: String) -> UnsafeMutablePointer<CChar>? { strdup(value) }
private func string(_ pointer: UnsafePointer<CChar>?) throws -> String {
  guard let pointer, let value = String(validatingUTF8: pointer) else {
    throw BridgeError.invalidUTF8
  }
  return value
}
private func optionalString(_ pointer: UnsafePointer<CChar>?) throws -> String? {
  guard let pointer else { return nil }
  return try string(pointer)
}
private func setError(_ output: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?, _ error: Error)
{
  output?.pointee = copyCString(errorString(error))
}

@_cdecl("afm_availability")
public func afmAvailability(_ detail: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?) -> Int32
{
  switch SystemLanguageModel.default.availability {
  case .available:
    detail?.pointee = copyCString("available")
    return 0
  case .unavailable(let reason):
    switch reason {
    case .deviceNotEligible:
      detail?.pointee = copyCString("this device does not support Apple Intelligence")
      return 1
    case .appleIntelligenceNotEnabled:
      detail?.pointee = copyCString("Apple Intelligence is not enabled")
      return 2
    case .modelNotReady:
      detail?.pointee = copyCString("the on-device model is not ready")
      return 3
    @unknown default:
      detail?.pointee = copyCString("the on-device model is unavailable")
      return 4
    }
  }
}

@_cdecl("afm_model_info")
public func afmModelInfo(
  _ configJSON: UnsafePointer<CChar>?, _ locale: UnsafePointer<CChar>?,
  _ output: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?,
  _ errorOutput: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  do {
    let model = systemModel(try decode(ModelConfiguration.self, string(configJSON)))
    let localeSupported = try optionalString(locale).map {
      model.supportsLocale(Locale(identifier: $0))
    }
    var variant: String? = nil
    var vision = false
    var guided = true
    var reasoning = false
    var tools = true
    if #available(macOS 27.0, *) {
      variant = model.variant.displayName
      vision = model.capabilities.contains(.vision)
      guided = model.capabilities.contains(.guidedGeneration)
      reasoning = model.capabilities.contains(.reasoning)
      tools = model.capabilities.contains(.toolCalling)
    }
    output?.pointee = copyCString(
      try encode(
        ModelInfo(
          contextSize: model.contextSize,
          supportedLanguages: model.supportedLanguages.map(\.minimalIdentifier).sorted(),
          variant: variant, vision: vision, guidedGeneration: guided,
          reasoning: reasoning, toolCalling: tools, localeSupported: localeSupported)))
    return 0
  } catch {
    setError(errorOutput, error)
    return -1
  }
}

private final class IntResultBox: @unchecked Sendable {
  var value: Result<Int, Error>?
}

private func waitForInt(_ operation: @escaping @Sendable () async throws -> Int) throws -> Int {
  let semaphore = DispatchSemaphore(value: 0)
  let box = IntResultBox()
  Task {
    let value: Result<Int, Error>
    do { value = .success(try await operation()) } catch { value = .failure(error) }
    box.value = value
    semaphore.signal()
  }
  semaphore.wait()
  return try box.value!.get()
}

@_cdecl("afm_model_token_count")
public func afmModelTokenCount(
  _ configJSON: UnsafePointer<CChar>?, _ kind: Int32,
  _ payload: UnsafePointer<CChar>?, _ output: UnsafeMutablePointer<Int64>?,
  _ errorOutput: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  do {
    guard #available(macOS 26.4, *) else {
      throw BridgeError.unsupported("token counting requires macOS 26.4 or later")
    }
    let model = systemModel(try decode(ModelConfiguration.self, string(configJSON)))
    let text = try string(payload)
    let count: Int
    switch kind {
    case 0:
      let input = try decode(PromptInput.self, text)
      if #available(macOS 27.0, *) {
        count = try waitForInt { try await model.tokenCount(for: makePrompt(input)) }
      } else {
        guard input.images.isEmpty else {
          throw BridgeError.unsupported("image prompts require macOS 27 or later")
        }
        count = try waitForInt { try await model.tokenCount(for: input.text) }
      }
    case 1: count = try waitForInt { try await model.tokenCount(for: Instructions(text)) }
    case 2:
      let schema = try decode(SchemaDocument.self, text).generationSchema()
      count = try waitForInt { try await model.tokenCount(for: schema) }
    case 3:
      let transcript = try decode(Transcript.self, text)
      count = try waitForInt { try await model.tokenCount(for: transcript) }
    case 4:
      let tools = try makeTools(decode([ToolDefinition].self, text), queue: EventQueue())
      count = try waitForInt { try await model.tokenCount(for: tools) }
    default: throw BridgeError.invalidJSON("unknown token-count input")
    }
    output?.pointee = Int64(count)
    return 0
  } catch {
    setError(errorOutput, error)
    return -1
  }
}

private let compactionSegmentID = "ocaml-apple-fm.compaction-summary"

private func compactTranscript(
  _ transcript: Transcript, summary: String, keepLastTurns: Int
) throws -> Transcript {
  guard keepLastTurns >= 0 else {
    throw BridgeError.invalidJSON("keepLastTurns must be nonnegative")
  }
  let entries = Array(transcript)
  let promptIndices = entries.indices.filter {
    if case .prompt = entries[$0] { return true }
    return false
  }
  let retainedPrompts = min(keepLastTurns, promptIndices.count)
  let tailStart = retainedPrompts == 0
    ? entries.endIndex
    : promptIndices[promptIndices.count - retainedPrompts]
  let prefixEnd = promptIndices.first ?? entries.endIndex
  var compacted = Array(entries[..<prefixEnd])
  var inserted = false
  for index in compacted.indices {
    guard case .instructions(var value) = compacted[index] else { continue }
    value.segments.removeAll { $0.id == compactionSegmentID }
    value.segments.append(
      .text(
        Transcript.TextSegment(
          id: compactionSegmentID,
          content:
            "\n[Earlier conversation compacted. Durable task state follows.]\n"
            + summary
            + "\n[End compacted task state. Recent conversation follows.]\n")))
    compacted[index] = .instructions(value)
    inserted = true
    break
  }
  if !inserted {
    throw BridgeError.invalidJSON(
      "cannot compact a transcript without an instructions entry")
  }
  compacted.append(contentsOf: entries[tailStart...])
  return Transcript(entries: compacted)
}

@_cdecl("afm_transcript_compact")
public func afmTranscriptCompact(
  _ transcriptJSON: UnsafePointer<CChar>?, _ summary: UnsafePointer<CChar>?,
  _ keepLastTurns: Int32,
  _ output: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?,
  _ errorOutput: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  do {
    let transcript = try decode(Transcript.self, string(transcriptJSON))
    let value = try compactTranscript(
      transcript, summary: try string(summary),
      keepLastTurns: Int(keepLastTurns))
    output?.pointee = copyCString(try encode(value))
    return 0
  } catch {
    setError(errorOutput, error)
    return -1
  }
}

@_cdecl("afm_session_create")
public func afmSessionCreate(
  _ instructions: UnsafePointer<CChar>?,
  _ toolsJSON: UnsafePointer<CChar>?, _ configJSON: UnsafePointer<CChar>?,
  _ transcriptJSON: UnsafePointer<CChar>?,
  _ errorOutput: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> UnsafeMutableRawPointer? {
  do {
    let session = try BridgeSession(
      instructions: try optionalString(instructions),
      definitions: decode([ToolDefinition].self, string(toolsJSON)),
      config: decode(ModelConfiguration.self, string(configJSON)),
      transcript: try optionalString(transcriptJSON).map { try decode(Transcript.self, $0) })
    return Unmanaged.passRetained(session).toOpaque()
  } catch {
    setError(errorOutput, error)
    return nil
  }
}

@_cdecl("afm_session_destroy")
public func afmSessionDestroy(_ handle: UnsafeMutableRawPointer?) {
  guard let handle else { return }
  Unmanaged<BridgeSession>.fromOpaque(handle).takeRetainedValue().close()
}

@_cdecl("afm_session_prewarm")
public func afmSessionPrewarm(
  _ handle: UnsafeMutableRawPointer?, _ prefix: UnsafePointer<CChar>?,
  _ errorOutput: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  do {
    guard let handle else { throw BridgeError.closed }
    Unmanaged<BridgeSession>.fromOpaque(handle).takeUnretainedValue()
      .prewarm(try optionalString(prefix))
    return 0
  } catch {
    setError(errorOutput, error)
    return -1
  }
}

@_cdecl("afm_session_start")
public func afmSessionStart(
  _ handle: UnsafeMutableRawPointer?,
  _ promptJSON: UnsafePointer<CChar>?, _ schemaJSON: UnsafePointer<CChar>?,
  _ includeSchema: Int32, _ reasoning: UnsafePointer<CChar>?,
  _ samplingKind: Int32, _ samplingValue: Double, _ hasSeed: Int32, _ seed: UInt64,
  _ hasTemperature: Int32, _ temperature: Double, _ maximum: Int32, _ toolMode: Int32,
  _ errorOutput: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  do {
    guard let handle else { throw BridgeError.closed }
    let schema = try optionalString(schemaJSON).map { try decode(SchemaDocument.self, $0) }
    try Unmanaged<BridgeSession>.fromOpaque(handle).takeUnretainedValue().start(
      prompt: decode(PromptInput.self, string(promptJSON)), schema: schema,
      includeSchema: includeSchema, reasoning: try optionalString(reasoning),
      samplingKind: samplingKind, samplingValue: samplingValue,
      seed: hasSeed == 0 ? nil : seed, temperature: hasTemperature == 0 ? nil : temperature,
      maximum: maximum <= 0 ? nil : Int(maximum), toolMode: toolMode)
    return 0
  } catch {
    setError(errorOutput, error)
    return -1
  }
}

@_cdecl("afm_session_next_event")
public func afmSessionNextEvent(
  _ handle: UnsafeMutableRawPointer?, _ timeout: Int32,
  _ kind: UnsafeMutablePointer<Int32>?, _ id: UnsafeMutablePointer<Int64>?,
  _ first: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?,
  _ second: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  guard let handle else { return -1 }
  let queue = Unmanaged<BridgeSession>.fromOpaque(handle).takeUnretainedValue().queue
  guard let event = queue.next(timeoutMilliseconds: timeout) else {
    kind?.pointee = 0
    return 0
  }
  switch event {
  case .text(let s):
    kind?.pointee = 1
    first?.pointee = copyCString(s)
  case .tool(let n, let name, let args):
    kind?.pointee = 2
    id?.pointee = n
    first?.pointee = copyCString(name)
    second?.pointee = copyCString(args)
  case .done(let s):
    kind?.pointee = 3
    first?.pointee = copyCString(s)
  case .error(let s):
    kind?.pointee = 4
    first?.pointee = copyCString(s)
  }
  return 0
}

@_cdecl("afm_session_resolve_tool")
public func afmSessionResolveTool(
  _ handle: UnsafeMutableRawPointer?, _ id: Int64,
  _ result: UnsafePointer<CChar>?,
  _ errorOutput: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  do {
    guard let handle else { throw BridgeError.closed }
    try Unmanaged<BridgeSession>.fromOpaque(handle).takeUnretainedValue().queue
      .resolveTool(id: id, result: string(result))
    return 0
  } catch {
    setError(errorOutput, error)
    return -1
  }
}

private func sessionString(
  _ handle: UnsafeMutableRawPointer?,
  _ output: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?,
  _ errorOutput: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?,
  _ operation: (BridgeSession) throws -> String
) -> Int32 {
  do {
    guard let handle else { throw BridgeError.closed }
    output?.pointee = copyCString(
      try operation(
        Unmanaged<BridgeSession>.fromOpaque(handle)
          .takeUnretainedValue()))
    return 0
  } catch {
    setError(errorOutput, error)
    return -1
  }
}

@_cdecl("afm_session_transcript")
public func afmSessionTranscript(
  _ handle: UnsafeMutableRawPointer?,
  _ output: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?,
  _ error: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  sessionString(handle, output, error) { try $0.transcriptJSON() }
}

@_cdecl("afm_session_usage")
public func afmSessionUsage(
  _ handle: UnsafeMutableRawPointer?,
  _ output: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?,
  _ error: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  sessionString(handle, output, error) { try $0.usageJSON() }
}

@_cdecl("afm_session_replace_transcript")
public func afmSessionReplaceTranscript(
  _ handle: UnsafeMutableRawPointer?,
  _ json: UnsafePointer<CChar>?,
  _ errorOutput: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  do {
    guard let handle else { throw BridgeError.closed }
    try Unmanaged<BridgeSession>.fromOpaque(handle).takeUnretainedValue()
      .replaceTranscript(decode(Transcript.self, string(json)))
    return 0
  } catch {
    setError(errorOutput, error)
    return -1
  }
}

@_cdecl("afm_session_cancel")
public func afmSessionCancel(_ handle: UnsafeMutableRawPointer?) {
  guard let handle else { return }
  Unmanaged<BridgeSession>.fromOpaque(handle).takeUnretainedValue().cancel()
}
