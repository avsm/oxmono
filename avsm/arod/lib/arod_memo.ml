(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Memoization cache with TTL *)

type 'a entry = { value : 'a; timestamp : float }

type 'a t = {
  ttl : float;
  entries : (string, 'a entry) Hashtbl.t;
  mutable hits : int;
  mutable misses : int;
}

let create ~ttl () =
  { ttl; entries = Hashtbl.create 16; hits = 0; misses = 0 }

let get cache ~key =
  match Hashtbl.find_opt cache.entries key with
  | None ->
      cache.misses <- cache.misses + 1;
      None
  | Some entry ->
      let now = Unix.gettimeofday () in
      if now -. entry.timestamp > cache.ttl then begin
        Hashtbl.remove cache.entries key;
        cache.misses <- cache.misses + 1;
        None
      end
      else begin
        cache.hits <- cache.hits + 1;
        Some entry.value
      end

let set cache ~key value =
  let entry = { value; timestamp = Unix.gettimeofday () } in
  Hashtbl.replace cache.entries key entry

let clear cache =
  Hashtbl.clear cache.entries;
  cache.hits <- 0;
  cache.misses <- 0

let stats cache = (cache.hits, cache.misses)

let memoize cache key handler =
  match get cache ~key with
  | Some response -> response
  | None ->
      let response = handler () in
      set cache ~key response;
      response
