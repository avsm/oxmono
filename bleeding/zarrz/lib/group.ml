(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { store : Store.t; path : string; meta : Metadata.group_meta }

let of_json store ~path j =
  match Metadata.group_of_json j with
  | Ok meta -> { store; path; meta }
  | Error m -> Error.raise_ (Error.Metadata m)

let open_ store ~path =
  of_json store ~path (Store.get_json store ~key:(Chunk_key.meta_key ~path))

let create ?attributes store ~path =
  let meta = { Metadata.group_attributes = attributes; group_unknown = [] } in
  let json = Metadata.group_to_json meta in
  let s =
    match Jsont_bytesrw.encode_string Jsont.json json with
    | Ok s -> s
    | Error m -> Error.raise_ (Error.Metadata m)
  in
  let key = Chunk_key.meta_key ~path in
  (match store.Store.set with
  | Some f -> f ~key (Base_bigstring.of_string s)
  | None -> Error.raise_ (Error.Store "the store does not support writing"));
  { store; path; meta }

let store t = t.store
let path t = t.path
let metadata t = t.meta
let attributes t = t.meta.group_attributes

(* A child is a name [n] such that the store holds the metadata document
   [n] would own. Listing the prefix and taking first components instead
   would report a chunk directory as a node. *)
let children t =
  match t.store.Store.list with
  | None -> None
  | Some list ->
      let prefix = Chunk_key.data_key ~path:t.path "" in
      let plen = String.length prefix in
      let leaf = "/zarr.json" in
      let llen = String.length leaf in
      let name k =
        let n = String.length k in
        if n <= plen + llen then None
        else
          let rest = String.sub k plen (n - plen) in
          let cut = String.length rest - llen in
          if not (String.equal (String.sub rest cut llen) leaf) then None
          else
            let n = String.sub rest 0 cut in
            if String.contains n '/' then None else Some n
      in
      Some (List.sort_uniq String.compare (List.filter_map name (list ~prefix)))
