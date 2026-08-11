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
}

let create ~ttl =
  { ttl; entries = Hashtbl.create 64 }

let get t key =
  match Hashtbl.find_opt t.entries key with
  | None -> None
  | Some entry ->
      let now = Unix.gettimeofday () in
      if now -. entry.timestamp > t.ttl then begin
        Hashtbl.remove t.entries key;
        None
      end
      else Some entry.value

let set t key value =
  let entry = { value; timestamp = Unix.gettimeofday () } in
  Hashtbl.replace t.entries key entry
