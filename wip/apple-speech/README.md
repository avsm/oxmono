# apple-speech

OCaml bindings to Apple's on-device speech transcription, the Speech
framework's `SpeechAnalyzer` and `SpeechTranscriber`. Audio never leaves the
machine. Transcription requires macOS 26 or later and the Xcode command line
tools. The package also builds on other platforms without Swift or Apple SDKs.
There, `available ()` returns `false` and transcription, synthesis, locale,
asset, voice, and duration operations raise `Error Unavailable`. Pure helpers
such as `text` remain usable.

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

## Synthesis

```ocaml
Eio_main.run @@ fun env ->
Apple_speech.synthesize (Eio.Stdenv.process_mgr env) ~voice:"Daniel"
  ~text:"Remind me to buy tea at nine." "reply.m4a"
```

`synthesize` writes M4A (AAC), WAV or AIFF with any installed voice, including
enhanced and premium voices downloaded in System Settings. `voices` lists them.
It runs `/usr/bin/say` in a separate process, because Apple's synthesis APIs
deliver audio only through the main thread's run loop, which an OCaml program
does not run. The text goes to `say` on standard input, never as an argument,
and square brackets become parentheses so text cannot carry `say`'s embedded
`[[...]]` commands. An unknown voice is an error, since `say` itself silently
falls back to the default.

## Command

    apple-speech transcribe [--locale en-GB] [--timings] [--no-install] FILE...
    apple-speech locales [--installed]
    apple-speech install [--locale en-GB]
    apple-speech say [--voice NAME] [--rate WPM] [--format m4a|wav|aiff] -o FILE TEXT
    apple-speech voices

## Tests

`dune runtest` synthesises speech with `say` and transcribes it, so it needs a
working Speech framework and an English model, which it downloads on first
use. The Ogg test also needs `ffmpeg` and is skipped without it.
