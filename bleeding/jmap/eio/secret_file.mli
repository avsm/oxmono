(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Files holding a secret. *)

val with_open_in :
  subject:string ->
  refused:(string -> 'a) ->
  Eio.Fs.dir_ty Eio.Path.t ->
  (Eio.File.ro_ty Eio.Resource.t -> 'a) ->
  'a
(** [with_open_in ~subject ~refused path f] is [f file] with an owned private
    regular file open through [path]. It rejects symlinks and validates the
    opened descriptor. [refused message] reports validation failure without file
    contents. [f] must bound its reads. The file is closed before returning.

    @raise Eio.Io if [path] cannot be opened or inspected. *)
