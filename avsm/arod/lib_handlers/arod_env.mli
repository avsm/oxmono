(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Capabilities for arod handlers.

    A handler is portable, so it cannot capture domain-bound state such as a
    search handle, an access log, a clock or a filesystem capability. Each of
    those is reached through a closure in {!t} instead. *)

type t = {
  ctx : Arod.Ctx.t;
      (** The entries and configuration the site renders from. *)
  cache : Proffer.Cache.t;
      (** The memoization cache for rendered pages. It crosses domains, so
          every domain shares one cache. *)
  search : limit:int -> string -> Arod_search.result list;
      (** [search ~limit query] is at most [limit] results for [query], from
          this domain's search handle. *)
  read_image : string list -> string option;
      (** [read_image segs] is the contents of the file named by [segs] under
          the served image directory, and [None] when that file is missing or
          unreadable. The read is confined to that directory, so [segs]
          naming a path that would leave it is [None]. *)
  read_paper : string -> string option;
      (** [read_paper name] is the contents of the file [name] under the
          served paper directory, and [None] when that file is missing or
          unreadable. The read is confined to that directory, so a [name]
          that would leave it is [None]. *)
  reader : unit -> Sqlite3_eio.t;
      (** [reader ()] is this domain's read-only handle on the access log
          database, for the stats dashboard. It is a separate connection from
          the one request logging writes on. *)
  now : unit -> float;
      (** [now ()] is the current time in seconds since the epoch, read from
          this domain's clock. *)
}
(** The capabilities a handler reaches through its [env] argument. One value
    is built per domain, where the domain-bound resources exist. *)
