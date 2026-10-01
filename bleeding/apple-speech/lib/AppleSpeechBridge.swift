import AVFoundation
import Darwin
import Foundation
import Speech

// Codes shared with apple_speech_stubs.c and apple_speech.ml.
private let codeUnavailable: Int32 = 1
private let codeUnsupportedLocale: Int32 = 2
private let codeAssetsMissing: Int32 = 3
private let codeUnreadable: Int32 = 4
private let codeFailed: Int32 = 5

private struct BridgeError: Error {
  let code: Int32
  let message: String
}

private struct Segment: Codable {
  let start: Double
  let duration: Double
  let text: String
}

private func copyCString(_ value: String) -> UnsafeMutablePointer<CChar>? { strdup(value) }

private func string(_ pointer: UnsafePointer<CChar>?) throws -> String {
  guard let pointer, let value = String(validatingUTF8: pointer) else {
    throw BridgeError(code: codeFailed, message: "invalid UTF-8 passed to the Speech bridge")
  }
  return value
}

private func encode<T: Encodable>(_ value: T) throws -> String {
  String(decoding: try JSONEncoder().encode(value), as: UTF8.self)
}

private final class ResultBox: @unchecked Sendable {
  var value: Result<String, Error>?
}

// The C caller blocks on this thread while the work runs on the Swift
// concurrency pool, so the OCaml runtime lock must already be released.
private func wait(_ operation: @escaping @Sendable () async throws -> String) -> Result<
  String, Error
> {
  let semaphore = DispatchSemaphore(value: 0)
  let box = ResultBox()
  Task {
    let value: Result<String, Error>
    do { value = .success(try await operation()) } catch { value = .failure(error) }
    box.value = value
    semaphore.signal()
  }
  semaphore.wait()
  return box.value!
}

private func finish(
  _ result: Result<String, Error>,
  _ output: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  switch result {
  case .success(let value):
    output?.pointee = copyCString(value)
    return 0
  case .failure(let error as BridgeError):
    output?.pointee = copyCString(error.message)
    return error.code
  case .failure(let error):
    output?.pointee = copyCString(error.localizedDescription)
    return codeFailed
  }
}

private func resolve(_ identifier: String?) async throws -> Locale {
  let requested = identifier.map { Locale(identifier: $0) } ?? Locale.current
  guard let locale = await SpeechTranscriber.supportedLocale(equivalentTo: requested) else {
    throw BridgeError(
      code: codeUnsupportedLocale,
      message: "speech transcription does not support \(requested.identifier)")
  }
  return locale
}

private func identifiers(_ locales: [Locale]) -> [String] {
  Array(Set(locales.map(\.identifier))).sorted()
}

@_cdecl("asp_available")
public func aspAvailable() -> Int32 { SpeechTranscriber.isAvailable ? 1 : 0 }

@_cdecl("asp_locales")
public func aspLocales(
  _ installed: Int32, _ output: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  finish(
    wait {
      let locales =
        installed != 0
        ? await SpeechTranscriber.installedLocales
        : await SpeechTranscriber.supportedLocales
      return try encode(identifiers(locales))
    }, output)
}

@_cdecl("asp_status")
public func aspStatus(
  _ locale: UnsafePointer<CChar>?, _ output: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  let identifier: String?
  do { identifier = try locale.map { try string($0) } } catch { return finish(.failure(error), output) }
  return finish(
    wait {
      let transcriber = SpeechTranscriber(locale: try await resolve(identifier), preset: .transcription)
      switch await AssetInventory.status(forModules: [transcriber]) {
      case .unsupported: return "unsupported"
      case .supported: return "supported"
      case .downloading: return "downloading"
      case .installed: return "installed"
      @unknown default: return "unsupported"
      }
    }, output)
}

@_cdecl("asp_install")
public func aspInstall(
  _ locale: UnsafePointer<CChar>?, _ output: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  let identifier: String?
  do { identifier = try locale.map { try string($0) } } catch { return finish(.failure(error), output) }
  return finish(
    wait {
      let locale = try await resolve(identifier)
      let transcriber = SpeechTranscriber(locale: locale, preset: .transcription)
      if let request = try await AssetInventory.assetInstallationRequest(supporting: [transcriber]) {
        try await request.downloadAndInstall()
      }
      return locale.identifier
    }, output)
}

@_cdecl("asp_transcribe")
public func aspTranscribe(
  _ path: UnsafePointer<CChar>?, _ locale: UnsafePointer<CChar>?, _ install: Int32,
  _ output: UnsafeMutablePointer<UnsafeMutablePointer<CChar>?>?
) -> Int32 {
  let file: String
  let identifier: String?
  do {
    file = try string(path)
    identifier = try locale.map { try string($0) }
  } catch { return finish(.failure(error), output) }
  return finish(
    wait {
      guard SpeechTranscriber.isAvailable else {
        throw BridgeError(
          code: codeUnavailable, message: "speech transcription is unavailable on this device")
      }
      let locale = try await resolve(identifier)
      let transcriber = SpeechTranscriber(locale: locale, preset: .transcription)
      if await AssetInventory.status(forModules: [transcriber]) != .installed {
        let request = try await AssetInventory.assetInstallationRequest(supporting: [transcriber])
        if let request {
          guard install != 0 else {
            throw BridgeError(
              code: codeAssetsMissing,
              message: "speech assets for \(locale.identifier) are not installed")
          }
          try await request.downloadAndInstall()
        }
      }
      let audio: AVAudioFile
      do {
        audio = try AVAudioFile(forReading: URL(fileURLWithPath: file))
      } catch {
        throw BridgeError(
          code: codeUnreadable, message: "cannot read audio: \(error.localizedDescription)")
      }
      let analyzer = SpeechAnalyzer(modules: [transcriber])
      // Results stream while the analyzer runs, so they are collected
      // concurrently and complete when the analyzer finishes.
      let collect = Task { () -> [Segment] in
        var segments: [Segment] = []
        for try await result in transcriber.results where result.isFinal {
          segments.append(
            Segment(
              start: result.range.start.seconds,
              duration: result.range.duration.seconds,
              text: String(result.text.characters)))
        }
        return segments
      }
      if let last = try await analyzer.analyzeSequence(from: audio) {
        try await analyzer.finalizeAndFinish(through: last)
      } else {
        await analyzer.cancelAndFinishNow()
      }
      return try encode(try await collect.value)
    }, output)
}
