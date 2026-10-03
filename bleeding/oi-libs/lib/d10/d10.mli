(** {1 D10: Content-addressed binary layer cache}

    D10 is a content-addressed binary package cache that stores compiled OCaml
    packages as filesystem layers. Each layer captures the files installed by a
    single opam package, keyed by a hash of the package's opam metadata and its
    transitive dependencies. Layers are assembled into complete prefixes via
    hardlinks for near-instant reuse.

    {2 On-disk layout}

    {v
    ~/.cache/oi/
      layers/
        <os_key>/              # e.g. macos~26~arm64
          <hash>/
            layer.json         # build metadata (package, deps, timestamps)
            fs/                # filesystem snapshot
              bin/
              lib/
              ...
      prefixes/<os_key>/
        <solve_hash>/          # MD5 of sorted layer hashes
          .ready               # marker: assembly complete
          bin/                 # hardlinked from layers
          lib/
          ...
      sources/
        <checksum>             # cached source tarballs
    v}

    {2 Layer hashing}

    Layer hashes are computed from the [effective_part] of each package's opam
    file (which strips volatile metadata like descriptions). The hash of a layer
    includes the package itself and all its direct dependencies, so any change
    to a dependency's opam file invalidates downstream layers.

    {2 Prefix assembly}

    Prefixes are assembled by hardlinking each layer's [fs/] tree into a
    destination directory, in topological order. This means a 37-package prefix
    can be assembled in milliseconds. Assembled prefixes are cached by their
    solve hash (MD5 of sorted layer hashes) and reused across invocations.

    {2 Symlink handling}

    Layers preserve symlinks (important for the relocatable OCaml compiler which
    uses symlinks like [ocamlrun-9100 -> arch-ocamlrun-9100]). During layer
    storage, symlinks are recreated with their original targets; regular files
    are hardlinked. *)

module Config = Config
module Layer = Layer
module Lock = Lock
module Prefix = Prefix
module Index = Index
module Remote_index = Remote_index
module Os_key = Os_key
module Overlay = Overlay
module Sysops = Sysops
