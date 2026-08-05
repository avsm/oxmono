(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** TTL-based cache for rendered HTML responses *)

type entry = {
  value : string;
  timestamp : float
}

type t = {
  ttl : float;
  entries : (string, entry) Hashtbl.t;
  mutable hits : int;
  mutable misses : int;
}

let create ~ttl =
  { ttl; entries = Hashtbl.create 64; hits = 0; misses = 0 }

let cache_key ~path ~query =
  if query = [] then path
  else
    let sorted_query =
      query
      |> List.sort compare
      |> List.map (fun (k, v) -> k ^ "=" ^ v)
      |> String.concat "&"
    in
    path ^ "?" ^ sorted_query

let get t key =
  match Hashtbl.find_opt t.entries key with
  | None ->
      t.misses <- t.misses + 1;
      None
  | Some entry ->
      let now = Unix.gettimeofday () in
      if now -. entry.timestamp > t.ttl then begin
        Hashtbl.remove t.entries key;
        t.misses <- t.misses + 1;
        None
      end
      else begin
        t.hits <- t.hits + 1;
        Some entry.value
      end

let set t key value =
  let entry = { value; timestamp = Unix.gettimeofday () } in
  Hashtbl.replace t.entries key entry

let clear t =
  Hashtbl.clear t.entries;
  t.hits <- 0;
  t.misses <- 0

let stats t = (t.hits, t.misses)

let size t = Hashtbl.length t.entries

let memoize t key f =
  match get t key with
  | Some value -> value
  | None ->
      let value = f () in
      set t key value;
      value
