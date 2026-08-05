(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** In-memory CID to block mapping using immutable maps. *)

type t = string Cid.Map.t
type with_missing = { blocks : t; missing : Cid.t list }

let empty = Cid.Map.empty
let singleton cid data = Cid.Map.singleton cid data

let of_list pairs =
  List.fold_left (fun acc (cid, data) -> Cid.Map.add cid data acc) empty pairs

let of_seq seq =
  Seq.fold_left (fun acc (cid, data) -> Cid.Map.add cid data acc) empty seq

let is_empty = Cid.Map.is_empty
let size = Cid.Map.cardinal
let get cid t = Cid.Map.find_opt cid t
let get_exn cid t = Cid.Map.find cid t
let has cid t = Cid.Map.mem cid t

let get_many cids t =
  let blocks, missing =
    List.fold_left
      (fun (blocks, missing) cid ->
        match Cid.Map.find_opt cid t with
        | Some data -> (Cid.Map.add cid data blocks, missing)
        | None -> (blocks, cid :: missing))
      (empty, []) cids
  in
  { blocks; missing = List.rev missing }

let set cid data t = Cid.Map.add cid data t

let set_many pairs t =
  List.fold_left (fun acc (cid, data) -> Cid.Map.add cid data acc) t pairs

let remove cid t = Cid.Map.remove cid t
let merge a b = Cid.Map.union (fun _ _ v -> Some v) a b
let to_list t = Cid.Map.bindings t
let to_seq t = Cid.Map.to_seq t
let iter f t = Cid.Map.iter f t
let fold f t acc = Cid.Map.fold f t acc
