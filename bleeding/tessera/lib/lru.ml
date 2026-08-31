(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Recency is a doubly linked list of the same nodes the hash table
   holds, so a hit moves one node to the front without walking anything
   and an eviction takes the tail. A counter and a scan for the oldest
   stamp would be simpler, but the scan is linear in the capacity on
   every miss, which is the common case for a cold tile cache.

   The links are [or_null] rather than [option] so that relinking a node
   writes a pointer instead of allocating a box. A cache hit is on the
   inner loop of a point read and allocates nothing but the [Some] it
   answers with. *)

type ('k, 'v) node = {
  key : 'k;
  mutable value : 'v;
  mutable prev : ('k, 'v) node or_null;
  mutable next : ('k, 'v) node or_null;
}

type ('k, 'v) t = {
  capacity : int;
  tbl : ('k, ('k, 'v) node) Hashtbl.t;
  mutable head : ('k, 'v) node or_null;  (* Most recently used. *)
  mutable tail : ('k, 'v) node or_null;  (* Least recently used. *)
}

let create ~capacity =
  if capacity < 1 then
    invalid_arg
      (Printf.sprintf "Lru.create: capacity %d is not positive" capacity);
  { capacity; tbl = Hashtbl.create capacity; head = Null; tail = Null }

let capacity t = t.capacity
let length t = Hashtbl.length t.tbl

let unlink t n =
  (match n.prev with This p -> p.next <- n.next | Null -> t.head <- n.next);
  (match n.next with This s -> s.prev <- n.prev | Null -> t.tail <- n.prev);
  n.prev <- Null;
  n.next <- Null

let push_front t n =
  n.prev <- Null;
  n.next <- t.head;
  (match t.head with This h -> h.prev <- This n | Null -> t.tail <- This n);
  t.head <- This n

let evict t =
  match t.tail with
  | Null -> ()
  | This n ->
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
      let n = { key = k; value = v; prev = Null; next = Null } in
      Hashtbl.replace t.tbl k n;
      push_front t n);
  if Hashtbl.length t.tbl > t.capacity then evict t

let clear t =
  Hashtbl.reset t.tbl;
  t.head <- Null;
  t.tail <- Null
