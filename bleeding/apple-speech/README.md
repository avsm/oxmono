# apple-speech

OCaml bindings to Apple's on-device speech transcription, the Speech
framework's `SpeechAnalyzer` and `SpeechTranscriber`. Audio never leaves the
machine. Requires macOS 26 or later and the Xcode command line tools. The
package builds only on macOS.

```ocaml
Eio_main.run @@ fun _ ->
let segments = Apple_speech.transcribe ~locale:"en-GB" "note.ogg" in
print_endline (Apple_speech.text segments)
```

Any file `AVAudioFile` reads is accepted, including WAV, AAC and Opus in Ogg,
which is what Matrix voice messages use. Each segment carries its start time
and duration in seconds. Silence gives no segments.

Each language needs a model that the system downloads once and shares between
applications. `transcribe` downloads a missing model unless `~install:false`
is passed, in which case it raises `Error (Assets_missing _)`. `install`
downloads one explicitly. `supported_locales` and `installed_locales` list
what is available. A locale resolves to the closest supported one, and omitting
it uses the user's current locale.

All functions except `available` block on a system thread and must run inside
Eio.

## Command

    apple-speech transcribe [--locale en-GB] [--timings] [--no-install] FILE...
    apple-speech locales [--installed]
    apple-speech install [--locale en-GB]

## Tests

`dune runtest` synthesises speech with `say` and transcribes it, so it needs a
working Speech framework and an English model, which it downloads on first
use. The Ogg test also needs `ffmpeg` and is skipped without it.
