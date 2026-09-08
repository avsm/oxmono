(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Files holding a secret. *)

val not_regular : subject:string -> string
(** [not_regular ~subject] is the message refusing a file that is not a regular
    file, naming it as [subject]. *)

val too_open : subject:string -> int -> string
(** [too_open ~subject permissions] is the message refusing a file whose mode is
    [permissions], naming it as [subject]. *)

val with_open_in :
  subject:string ->
  refused:(string -> 'a) ->
  Eio.Fs.dir_ty Eio.Path.t ->
  (Eio.File.ro_ty Eio.Resource.t -> 'a) ->
  'a
(** [with_open_in ~subject ~refused path f] is [f file] with [path] open for
    reading, and [refused message] when [path] is not a regular file or grants
    group or other access. [message] names the file as [subject] and, for a
    permission failure, the mode it has. Symlinks are followed and the kind and
    mode are those of the target. [file] is closed before this returns.

    @raise Eio.Io if [path] cannot be opened or inspected. *)
