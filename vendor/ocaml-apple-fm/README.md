# ocaml-apple-fm

`apple-fm` is an OCaml binding to Apple's on-device Foundation Models
framework. It provides language-model sessions, streaming to Eio
flows and cancellation, generation options, runtime-defined guided-generation
schemas, model-initiated calls to OCaml tools, persistent transcripts,
structured responses, token accounting, and multimodal prompts.
`apple-fm-agent` is a small coding agent built on the library.

Model operations require macOS 26 or later, Apple Silicon with Apple Intelligence,
and an Apple SDK containing `FoundationModels.framework`. Apple Intelligence
must be enabled and the system model must be ready.

The package also builds on other platforms without Swift or Apple SDKs.
`Availability.get ()` returns `Unavailable`, and model operations raise
``Eio.Io (Error.E (`Unsupported_version _), _)``. Schema, codec, prompt, and
transcript helpers remain usable.

## Build

```sh
opam install . --deps-only
dune build
dune runtest
```

On a machine with Apple Intelligence enabled, the cancellation lifecycle probe
can be run explicitly with `dune exec ./test/live_cancel.exe`. This is a good
test to make sure nothing weird is going on.

On macOS, the build compiles `AppleFMBridge.swift` into a static archive. Dune
links that archive into native consumers, so the package has no private dynamic
library or runtime search path. The OCaml library is native-code only.

See [DESIGN.md](DESIGN.md) for the API selection, concurrency model, and the
reason a Swift shim is necessary.

Run a one-shot coding task or start an interactive session:

```sh
dune exec apple-fm-agent -- "summarize this repository"
dune exec apple-fm-agent
dune exec apple-fm-agent -- --session .apple-fm-session.json
```

The example agent exposes unrestricted shell execution. Its file tools are
confined to the workspace selected with `-C`, but it is not a security sandbox.
It counts the Apple transcript before each turn and compacts at 75% of the
model context by default. Compaction asks the model for durable task state,
then rebuilds the transcript with that summary and up to two recent complete
turns. `--compact-at=0` disables the automatic policy. In interactive mode,
`/status`, `/compact`, `/save`, `/load`, and `/new` expose the same lifecycle
operations directly. A `--session` path is workspace-relative; it is restored
at startup when present and atomically updated after successful turns.
Read and shell observations are capped relative to the model context so a
single tool result cannot consume an otherwise healthy session.

## Library example

```ocaml
open Apple_fm

let weather =
  let open Codec in
  let arguments =
    Invoke.map "weather" Fun.id
    |> Invoke.param ~enc:Fun.id "city" string ~description:"city name"
    |> Invoke.seal
  in
  Tool.v ~description:"Get the current weather." arguments (fun city ->
      city ^ ": 18 C")

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  try
    let session =
      Session.create ~sw ~instructions:"Answer briefly." [ weather ]
    in
    ignore
      (Session.respond_stream ~output:(Eio.Stdenv.stdout env) session
         "What is the weather in London?");
    Eio.Flow.copy_string "\n" (Eio.Stdenv.stdout env)
  with Eio.Io (Error.E _, _) as exn -> Format.eprintf "%a@." Eio.Exn.pp exn
```

Apple references: [prompting an on-device foundation
model](https://developer.apple.com/documentation/foundationmodels/prompting-an-on-device-foundation-model),
[tool calling](https://developer.apple.com/documentation/foundationmodels/expanding-generation-with-tool-calling),
and [`LanguageModelSession`](https://developer.apple.com/documentation/foundationmodels/languagemodelsession).

## Local patches in oxmono

- The non-macOS C bridge reports unavailability through the existing error
  protocol. The macOS build retains the Swift bridge.
- Codec interfaces retain the portability and contention requirements of
  oxmono's Jsont. Recursive schemas and values use `Portable_lazy` instead of
  casting `Stdlib.Lazy` values to a different representation.
- Run `dune build --force @@vendor/ocaml-apple-fm/test/runtest` after updating
  these patches. Agentkit's `test_core` alias also checks the unsupported
  platform error through the adapter.
