(** Install files declared by an opam [.install] manifest.

    Sources are resolved relative to a build directory and copied into the
    appropriate prefix subdirectories without invoking opam-installer. *)

val apply :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  prefix:string ->
  build_dir:string ->
  install_file:string ->
  unit
(** [apply ~fs ~prefix ~build_dir ~install_file] copies the files declared in
    [install_file] from [build_dir] into the appropriate subdirectories of
    [prefix]. Required (non-[?]) entries that don't exist trigger a failure via
    [failwith]. Optional entries are silently skipped. *)
