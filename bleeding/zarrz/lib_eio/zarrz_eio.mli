(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Zarr stores on a filesystem, through Eio.

    A store is a directory and a key is a relative path under it, so the
    key ["a/b/c/0/0"] is the file [a/b/c/0/0] below the root. That is
    the mapping [zarrs_filesystem] uses, so a hierarchy either library
    writes is one the other reads.

    Nothing here takes an [Eio.Stdenv.t]. A caller passes the one
    directory the store may reach, and the store reaches nothing
    else. *)

val store : ?writable:bool -> _ Eio.Path.t -> Zarrz.Store.t
(** [store root] is the store whose objects are the regular files below
    the directory [root]. [root] itself is never created, and neither is
    it opened until the first operation, so a store over a directory
    that does not exist is a store where every key is absent.

    Every read opens the file, takes its size from the open descriptor
    and reads the bytes it wants straight into a fresh
    {!Base_bigstring.t}, so a chunk never passes through an OCaml
    string. An entry that is not a regular file, a directory in
    particular, is absent: [c/0] is a key of a chunk, not of the
    directory holding [c/0/0].

    [ranged] is [true]. [get_range] and [get_ranges] read at an offset
    within the open file rather than read it whole, and [get_ranges]
    opens once for all of its ranges, which is why a batch is worth
    asking for. A {!Zarrz.Byte_range.Suffix} resolves against the size
    of the open file, which is what the sharding index at the end of a
    shard needs.

    [size] is the file size, and [None] for a key that is absent or is
    not a regular file.

    [list] walks the directory tree below [root] and is the keys of the
    regular files that start with the prefix, sorted. It descends only
    into directories a matching key could lie in, so listing a node does
    not read the whole store. [list] is always present, whether or not
    the store is writable.

    [writable] defaults to [false], in which case [set] and [erase] are
    [None] and the store cannot alter the directory. When it is [true]:

    {ul
    {- [set] creates the missing parent directories of the key with
       mode [0o755], then writes the whole object to the file,
       truncating any earlier one, with mode [0o644]. The umask of the
       process applies to both.}
    {- [erase] unlinks the file and does nothing when it is already
       absent, as {!Zarrz.Store.t} requires. Erasing a key that names a
       directory fails instead, since a directory is not an object.}}

    A key must be a relative path with no empty, ["."] or [".."]
    component, which the keys the specification defines already are. A
    key that is not raises {!Zarrz.Error.E} [(Store _)] rather than
    reaching the filesystem, so a store cannot be turned into a way out
    of [root]. A prefix passed to [list] is held to the same rule, save
    that it may be empty and may end in ['/'].

    That is stricter than the specification twice over, deliberately.
    ["."] and [".."] are ordinary characters to a key and name
    directories to a filesystem, so a key holding one would leave
    [root]. The empty key is a key the specification allows, and it
    names [root] itself, which is a directory here rather than an
    object.

    Every other failure of the filesystem raises {!Zarrz.Error.E}
    [(Store _)] carrying the rendering of the Eio exception. Only the
    Eio not-found error becomes an absent key. *)
