(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** A client-side cookie jar: storage per
    {{:https://datatracker.ietf.org/doc/html/rfc6265}RFC 6265} §5.3,
    retrieval per §5.4, and persistence in the Netscape cookies.txt
    format that curl reads and writes.

    The jar is bounded as §6.1 expects: a cookie's name and value
    together may not exceed 4096 bytes, and the jar holds at most 50
    cookies per domain and 3000 in all, evicting the least recently used
    when it is full. It also refuses, per RFC 6265bis, a plaintext
    cookie that would shadow a stored [Secure] one, and a [__Secure-] or
    [__Host-] cookie arriving over plaintext.

    An HTTP client calls {!set} with each [Set-Cookie] value a response
    carries and {!header_for} to build the [Cookie] header for a
    request. Both take the request's canonical (lowercase) host, its
    path, and whether the scheme is https. *)

type t
(** A cookie store. All operations are safe to call from concurrent
    fibers. *)

val in_memory : clock:_ Eio.Time.clock -> unit -> t
(** [in_memory ~clock ()] is an empty jar that is never written to
    disk. *)

val of_file :
  clock:_ Eio.Time.clock ->
  ?save:[ `On_change | `Manual ] ->
  _ Eio.Path.t -> t
(** [of_file ~clock path] is a jar backed by [path] in the Netscape
    cookies.txt format, curl-compatible including its [#HttpOnly_]
    line marking. The file is loaded if it exists and created on the
    first save, and saves are atomic. [`On_change], the default, saves
    after every change, while [`Manual] saves only on {!flush}. The jar
    retains [path] and no other filesystem access. *)

val flush : t -> unit
(** [flush t] writes [t] out now, and does nothing for an in-memory
    jar. *)

val clear : t -> unit
(** [clear t] discards every cookie in [t]. *)

val set :
  t -> host:string -> path:string -> https:bool -> string ->
  (unit, string) result
(** [set t ~host ~path ~https line] stores the [Set-Cookie] value
    [line] as received by a request to [host] at [path] over https or
    not, applying the parse rules of {!Cookeio.parse_set_cookie} and the
    jar's own bounds and [Secure] rules. [Error reason] says why a value
    was refused; per §5.2 the refusal is otherwise not an error, and the
    jar is unchanged. *)

val header_for : t -> host:string -> path:string -> https:bool -> string option
(** [header_for t ~host ~path ~https] is the [Cookie] header value for a
    request to [host] at [path], or [None] if no cookie matches. A
    [Secure] cookie is only offered when [https]. Matched cookies have
    their last-access time updated, and any expired cookie the lookup
    passes is evicted (§5.4). *)

val cookies : t -> Cookeio.t list
(** [cookies t] is a snapshot of every cookie in [t], for inspection. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints the jar's cookies for debugging. *)
