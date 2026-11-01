(** Binary layers and installation prefixes.

    {!Layer} stores installed files and metadata under a caller-selected cache
    root. {!Prefix} combines layers in dependency order. {!Lock} serializes
    access across processes. Construct {!Config.t} with an Eio environment, a
    {!Sysops.t} and a platform key from {!Os_key.of_platform}.

    {2 Cache layout}

    {v
    <root>/layers/<os_key>/<hash>/
      layer.json              # package, dependencies, status and timestamp
      recipe.json             # optional producer recipe
      fs/                     # installed files
    <root>/prefixes/<os_key>/
      <hash>/                 # permanent per-layer prefix
      <solve_hash>/           # cached union of ordered layers
    v}

    Hashes are supplied by the caller. {!Layer.hash} covers opam metadata and a
    supplied dependency closure. Consumers must also account for source,
    platform, build configuration and any absolute paths embedded in outputs.

    Regular files may be hardlinked to the store. Keep completed layers and
    prefixes immutable. Use {!Prefix.prepare} to obtain a writable prefix.
    Restoring a layer rebases dune-package paths but does not make arbitrary
    binaries, scripts or runtime data relocatable.

    Cache writes require caller serialization across processes and fibers.
    {!Index} provides optional local SQLite queries. {!Remote_index} and the
    remote functions in {!Layer} are opt-in readers with no default registry.
    Use [d10.ir] for recipe execution and standalone Makefile generation. *)

module Config = Config
module Layer = Layer
module Lock = Lock
module Prefix = Prefix
module Index = Index
module Remote_index = Remote_index
module Os_key = Os_key
module Overlay = Overlay
module Sysops = Sysops
