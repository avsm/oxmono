(* SPDX-License-Identifier: ISC *)
(** Helpers shared by the spindle tests. *)

val with_dir :
  < fs : Eio.Fs.dir_ty Eio.Path.t ; process_mgr : _ Eio.Process.mgr ; .. > ->
  string ->
  (string -> Eio.Fs.dir_ty Eio.Path.t -> 'a) ->
  'a
(** [with_dir env prefix f] is [f name dir] for a new private directory [name],
    whose path in [env#fs] is [dir]. The directory begins with [prefix] and is
    removed with its contents when [f] returns or raises. *)

val has : string -> string -> bool
(** [has s sub] is [true] when [sub] occurs in [s]. *)
