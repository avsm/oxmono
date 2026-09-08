(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Shared XDG-backed login profiles.

    Each successful login is written through {!Jmap_eio.Profile}, so Mosaic and
    other JMAP programs select the same named session URLs and credentials. The
    directory must be private to the user and each profile, including its
    secret, is mode [0600]. An existing shared directory with group or other
    permissions is ignored rather than modified. *)

val profiles_path : Eio_unix.Stdenv.base -> string
(** [profiles_path env] is the directory containing named profiles, normally
    [$XDG_CONFIG_HOME/jmap/profiles]. It is the empty string when the store
    cannot be located, which is when neither [XDG_CONFIG_HOME] nor [HOME] names
    an absolute directory. *)

val read_profile : Eio_unix.Stdenv.base -> string -> Model.login option
(** [read_profile env name] reads the complete named profile, including its
    secret, from the shared store. It is [None] when the name is unsafe, the
    file cannot be read, or the profile is incomplete. A file that is not
    regular, exceeds 64 KiB, or grants any group or other permission is
    unreadable, as is every file of a store directory with those permissions. *)

val profiles : Eio_unix.Stdenv.base -> Model.login list
(** [profiles env] is every complete profile of the shared store in name order.
    Invalid or unreadable files, including files with unsafe permissions, are
    ignored. *)

val write_profile : Eio_unix.Stdenv.base -> Model.login -> unit
(** [write_profile env login] stores [login] under its profile name in the
    shared {!Jmap_eio.Profile} store. The profiles directory is created with
    mode [0700] and the file with mode [0600]. An existing store directory
    carrying group or other permission bits is refused without its mode being
    changed. Replacing a profile is atomic. An invalid profile name, a profile
    over 64 KiB, or a write error is ignored. *)
