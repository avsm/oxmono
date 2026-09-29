## v1.0

- Initial public release (@avsm).
- Split the public interface into documented submodules and use polymorphic
  variants for availability, generation choices, and framework errors.
- Add Eio-native sessions, streaming, cancellation, typed tools, and generation
  options for Apple's on-device Foundation Models framework.
- Add the `apple-fm-agent` example coding agent.
- Add transcript export, restoration, and macOS 27 transcript replacement.
- Add model information, locale checks, token counting, and macOS 27 usage.
- Add typed framework errors carried as contextual `Eio.Io` exceptions.
- Add jsont-decoded structured responses and structured tool observations.
- Add null, guided, union, reference, and dependency schemas.
- Add system-model use cases and guardrail configuration.
- Add macOS 27 image prompts, capabilities, variants, and reasoning options.
- Add transcript compaction that preserves instructions, tool definitions, and
  recent complete turns.
- Add automatic context compaction, transcript checkpoints, restoration,
  lifecycle commands, and token status to the example agent.
- Add adapter-oriented tool codec construction and invocation introspection,
  without introducing an agent-framework dependency.
- Add typed tool-argument encoding for adapter logging and replay.
- Encode bridge payloads directly with typed jsont codecs instead of building
  intermediate generic JSON trees.
- Construct schemas and jsont codecs together, and enforce scalar ranges,
  choices, finite numbers, array lengths, and defaults during local decoding or
  encoding.
