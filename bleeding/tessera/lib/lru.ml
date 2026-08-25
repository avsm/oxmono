(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Recency is a doubly linked list of the same nodes the hash table
   holds, so a hit moves one node to the front without walking anything
   and an eviction takes the tail. A counter and a scan for the oldest
   stamp would be simpler, but the scan is linear in the capacity on
   every miss, which is the common case for a cold tile cache. *)

type ('k, 'v) node = {
  key : 'k;
  mutable value : 'v;
  mutable prev : ('k, 'v) node option;
  mutable next : ('k, 'v) node option;
}

type ('k, 'v) t = {
  capacity : int;
  tbl : ('k, ('k, 'v) node) Hashtbl.t;
  mutable head : ('k, 'v) node option;  (* Most recently used. *)
  mutable tail : ('k, 'v) node option;  (* Least recently used. *)
}

let create ~capacity =
  if capacity < 1 then
    invalid_arg
      (Printf.sprintf "Lru.create: capacity %d is not positive" capacity);
  { capacity; tbl = Hashtbl.create capacity; head = None; tail = None }

let capacity t = t.capacity
let length t = Hashtbl.length t.tbl

let unlink t n =
  (match n.prev with Some p -> p.next <- n.next | None -> t.head <- n.next);
  (match n.next with Some s -> s.prev <- n.prev | None -> t.tail <- n.prev);
  n.prev <- None;
  n.next <- None

let push_front t n =
  n.prev <- None;
  n.next <- t.head;
  (match t.head with Some h -> h.prev <- Some n | None -> t.tail <- Some n);
  t.head <- Some n

let evict t =
  match t.tail with
  | None -> ()
  | Some n ->
      unlink t n;
      Hashtbl.remove t.tbl n.key

let find_opt t k =
  match Hashtbl.find_opt t.tbl k with
  | None -> None
  | Some n ->
      unlink t n;
      push_front t n;
      Some n.value

let add t k v =
  (match Hashtbl.find_opt t.tbl k with
  | Some n ->
      n.value <- v;
      unlink t n;
      push_front t n
  | None ->
      let n = { key = k; value = v; prev = None; next = None } in
      Hashtbl.replace t.tbl k n;
      push_front t n);
  if Hashtbl.length t.tbl > t.capacity then evict t

let clear t =
  Hashtbl.reset t.tbl;
  t.head <- None;
  t.tail <- None
