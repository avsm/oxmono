(** Apply an opam [.install] manifest to a prefix.

    A self-contained reimplementation of [opam-installer] without forking:
    parses the [.install] file via [OpamFile.Dot_install], resolves each source
    relative to a build directory, and copies it into the matching subdirectory
    of a prefix. This is the only opam-specific code in d10ir; the rest of the
    executor only knows about archives, scripts, and layers.

    The behaviour matches what oi was doing inline in [Oi.Execute] today; this
    is a code move, not new logic. *)

val apply :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  prefix:string ->
  build_dir:string ->
  install_file:string ->
  unit
(** [apply ~fs ~prefix ~build_dir ~install_file] copies the files declared in
    [install_file] from [build_dir] into the appropriate subdirectories of
    [prefix]. Required (non-[?]) entries that don't exist trigger a failure via
    [failwith]; optional entries are silently skipped. *)
