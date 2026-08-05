(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Layer computation: count leading binary zeros in SHA-256 hash, divide by 2.
   This determines where a key sits in the MST hierarchy.

   Algorithm per draft-holmgren-at-repository.md Section 4.1.3 (lines 241-245):
   1. Compute the SHA-256 hash of the key (byte string) with binary output
   2. Count the number of leading binary zeros in the hash
   3. Divide by 2, rounding down to the nearest integer

   This provides an average fanout of 4 (since 2^2 = 4). *)
let layer_of_key key =
  let digest = Digestif.SHA256.(digest_string key |> to_raw_string) in
  (* Count leading zero bits across the entire hash *)
  let rec count_leading_zeros idx bit_count =
    if idx >= String.length digest then bit_count
    else
      let byte = Char.code digest.[idx] in
      if byte = 0 then
        (* Full byte of zeros = 8 bits *)
        count_leading_zeros (idx + 1) (bit_count + 8)
      else
        (* Count leading zeros in this byte using CLZ *)
        let leading =
          if byte >= 128 then 0
          else if byte >= 64 then 1
          else if byte >= 32 then 2
          else if byte >= 16 then 3
          else if byte >= 8 then 4
          else if byte >= 4 then 5
          else if byte >= 2 then 6
          else 7 (* byte = 1 *)
        in
        bit_count + leading
  in
  let zero_bits = count_leading_zeros 0 0 in
  (* Layer = floor(zero_bits / 2) *)
  zero_bits / 2

(* Shared prefix length for key compression in serialization *)
let shared_prefix_len a b =
  let rec loop idx =
    if idx >= String.length a || idx >= String.length b then idx
    else if a.[idx] = b.[idx] then loop (idx + 1)
    else idx
  in
  loop 0

(* Error handling - defined early so Raw module can use it *)

type error =
  [ `Mst_invalid_key of string
  | `Mst_missing_block of Cid.t
  | `Mst_corrupt_node of string
  | `Mst_keys_not_sorted ]

let pp_error ppf = function
  | `Mst_invalid_key s -> Fmt.pf ppf "invalid MST key: %s" s
  | `Mst_missing_block cid -> Fmt.pf ppf "missing MST block: %a" Cid.pp cid
  | `Mst_corrupt_node s -> Fmt.pf ppf "corrupt MST node: %s" s
  | `Mst_keys_not_sorted -> Fmt.string ppf "MST keys not sorted"

type Eio.Exn.err += E of error

let () = Eio_error.register_pp pp_error (function E e -> Some e | _ -> None)
let raise_error e = Eio_error.raise_ (E e)

module Raw = struct
  type entry = { p : int; k : string; v : Cid.t; t : Cid.t option }
  type node = { l : Cid.t option; e : entry list }

  let encode_entry e : Dagcbor.value =
    `Map
      [
        ("k", `Bytes e.k);
        ("p", `Int (Int64.of_int e.p));
        ("t", match e.t with Some cid -> `Link cid | None -> `Null);
        ("v", `Link e.v);
      ]

  let encode node : Dagcbor.value =
    `Map
      [
        ("e", `List (List.map encode_entry node.e));
        ("l", match node.l with Some cid -> `Link cid | None -> `Null);
      ]

  let decode_entry (v : Dagcbor.value) : entry =
    match v with
    | `Map entries ->
        let find_field name = List.assoc_opt name entries in
        let p =
          match find_field "p" with Some (`Int i) -> Int64.to_int i | _ -> 0
        in
        let k =
          match find_field "k" with
          | Some (`Bytes s) -> s
          | Some (`String s) -> s
          | _ -> ""
        in
        let v =
          match find_field "v" with
          | Some (`Link cid) -> cid
          | _ -> raise_error (`Mst_corrupt_node "missing v field in MST entry")
        in
        let t =
          match find_field "t" with Some (`Link cid) -> Some cid | _ -> None
        in
        { p; k; v; t }
    | _ -> raise_error (`Mst_corrupt_node "MST entry must be a map")

  let decode (v : Dagcbor.value) : node =
    match v with
    | `Map entries ->
        let find_field name = List.assoc_opt name entries in
        let l =
          match find_field "l" with Some (`Link cid) -> Some cid | _ -> None
        in
        let e =
          match find_field "e" with
          | Some (`List items) -> List.map decode_entry items
          | _ -> []
        in
        { l; e }
    | _ -> raise_error (`Mst_corrupt_node "MST node must be a map")

  let decode_bytes s = decode (Dagcbor.decode_string s)
  let encode_bytes node = Dagcbor.encode_string (encode node)
end

type entry = {
  layer : int;
  key : string;
  value : Cid.t;
  mutable right : node option Lazy.t;
}

and node = {
  layer : int;
  mutable left : node option Lazy.t;
  mutable entries : entry list;
}

(* Creation *)

let empty = { layer = 0; left = lazy None; entries = [] }

let leaf key value =
  let layer = layer_of_key key in
  {
    layer;
    left = lazy None;
    entries = [ { layer; key; value; right = lazy None } ];
  }

(* Loading from blockstore *)

let rec load_node (cid : Cid.t) ~(store : Blockstore.readable) : node =
  try
    match store#get cid with
    | None -> raise_error (`Mst_missing_block cid)
    | Some data ->
        let raw = Raw.decode_bytes data in
        raw_to_node raw ~store
  with Eio.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context ex bt "loading MST node %a" Cid.pp cid

and raw_to_node (raw : Raw.node) ~(store : Blockstore.readable) : node =
  (* Reconstruct full keys from prefix-compressed entries *)
  let _, entries =
    List.fold_left
      (fun (prev_key, acc) (e : Raw.entry) ->
        let prefix = String.sub prev_key 0 (min e.p (String.length prev_key)) in
        let full_key = prefix ^ e.k in
        let layer = layer_of_key full_key in
        let entry =
          {
            layer;
            key = full_key;
            value = e.v;
            right =
              lazy
                (match e.t with
                | None -> None
                | Some cid -> Some (load_node cid ~store));
          }
        in
        (full_key, entry :: acc))
      ("", []) raw.e
  in
  let entries = List.rev entries in
  let layer = match entries with [] -> 0 | e :: _ -> e.layer in
  {
    layer;
    left =
      lazy
        (match raw.l with
        | None -> None
        | Some cid -> Some (load_node cid ~store));
    entries;
  }

let of_cid cid ~store = load_node cid ~store

(* Saving to blockstore *)

let rec save_node (node : node) ~(store : Blockstore.writable) : Cid.t =
  let raw = node_to_raw node ~store in
  let data = Raw.encode_bytes raw in
  let cid = Cid.create `Dag_cbor data in
  store#put cid data;
  cid

and node_to_raw (node : node) ~(store : Blockstore.writable) : Raw.node =
  let save_child child = save_node child ~store in
  let l = Option.map save_child (Lazy.force node.left) in
  let _, entries =
    List.fold_left
      (fun (prev_key, acc) entry ->
        let p = shared_prefix_len prev_key entry.key in
        let k = String.sub entry.key p (String.length entry.key - p) in
        let t = Option.map save_child (Lazy.force entry.right) in
        let raw_entry : Raw.entry = { p; k; v = entry.value; t } in
        (entry.key, raw_entry :: acc))
      ("", []) node.entries
  in
  { l; e = List.rev entries }

let to_cid node ~store = save_node node ~store

(* Queries *)

let rec get key node ~(store : Blockstore.readable) =
  (* Binary search through entries *)
  let rec search entries =
    match entries with
    | [] -> (
        (* Check left subtree *)
        match Lazy.force node.left with
        | None -> None
        | Some child -> get key child ~store)
    | entry :: rest -> (
        let cmp = String.compare key entry.key in
        if cmp = 0 then Some entry.value
        else if cmp < 0 then
          (* Key is before this entry, check left or previous right *)
          match Lazy.force node.left with
          | None -> None
          | Some child -> get key child ~store
        else
          (* Key is after this entry, check right subtree or continue *)
          match rest with
          | next :: _ when String.compare key next.key < 0 -> (
              (* Key is between this entry and next, check right subtree *)
              match Lazy.force entry.right with
              | None -> None
              | Some child -> get key child ~store)
          | _ ->
              (* Continue to next entry *)
              search rest)
  in
  search node.entries

let mem key node ~store = Option.is_some (get key node ~store)

let rec leaves node ~(store : Blockstore.readable) =
  let rec entries_seq entries () =
    match entries with
    | [] -> Seq.Nil
    | entry :: rest ->
        (* First yield from right subtree of previous entry (if any - handled by caller) *)
        (* Then yield this entry *)
        Seq.Cons
          ( (entry.key, entry.value),
            fun () ->
              (* Then yield from right subtree *)
              let right_seq =
                match Lazy.force entry.right with
                | None -> Seq.empty
                | Some child -> leaves child ~store
              in
              Seq.append right_seq (entries_seq rest) () )
  in
  (* Start with left subtree *)
  let left_seq =
    match Lazy.force node.left with
    | None -> Seq.empty
    | Some child -> leaves child ~store
  in
  Seq.append left_seq (entries_seq node.entries)

(* Mutations - simplified implementation *)

let add key value node ~(store : Blockstore.writable) =
  Repo_key.validate_exn key;
  let key_layer = layer_of_key key in
  (* Simple implementation: collect all leaves, add/update, rebuild *)
  let all_leaves = leaves node ~store:(store :> Blockstore.readable) in
  let updated = ref false in
  let new_leaves =
    Seq.filter_map
      (fun (k, v) ->
        if k = key then begin
          updated := true;
          Some (k, value)
        end
        else Some (k, v))
      all_leaves
    |> List.of_seq
  in
  let final_leaves =
    if !updated then new_leaves
    else
      (* Insert in sorted position *)
      let rec insert acc = function
        | [] -> List.rev ((key, value) :: acc)
        | (k, v) :: rest when String.compare key k < 0 ->
            List.rev_append acc ((key, value) :: (k, v) :: rest)
        | kv :: rest -> insert (kv :: acc) rest
      in
      insert [] new_leaves
  in
  (* Rebuild tree - simple approach: create leaf for single entry *)
  match final_leaves with
  | [] -> empty
  | [ (k, v) ] -> leaf k v
  | _ ->
      (* For multiple entries, build a simple tree *)
      (* This is a simplified implementation - a full implementation would
         properly layer the tree based on key hashes *)
      let entries =
        List.map
          (fun (k, v) ->
            let layer = layer_of_key k in
            { layer; key = k; value = v; right = lazy None })
          final_leaves
      in
      { layer = key_layer; left = lazy None; entries }

let remove key node ~(store : Blockstore.writable) =
  let all_leaves = leaves node ~store:(store :> Blockstore.readable) in
  let filtered =
    Seq.filter (fun (k, _) -> k <> key) all_leaves |> List.of_seq
  in
  match filtered with
  | [] -> empty
  | [ (k, v) ] -> leaf k v
  | _ ->
      let entries =
        List.map
          (fun (k, v) ->
            let layer = layer_of_key k in
            { layer; key = k; value = v; right = lazy None })
          filtered
      in
      let layer = match entries with [] -> 0 | e :: _ -> e.layer in
      { layer; left = lazy None; entries }

(* Export *)

let to_blocks node ~(store : Blockstore.readable) =
  let _ = store in
  (* API consistency, not needed for in-memory traversal *)
  (* BFS traversal to collect all blocks *)
  let visited = Hashtbl.create 64 in
  let rec collect_node n acc =
    let raw = node_to_raw_for_export n in
    let data = Raw.encode_bytes raw in
    let cid = Cid.create `Dag_cbor data in
    if Hashtbl.mem visited (Cid.to_string cid) then acc
    else begin
      Hashtbl.add visited (Cid.to_string cid) ();
      let acc = (cid, data) :: acc in
      let acc =
        Option.fold ~none:acc
          ~some:(fun c -> collect_node c acc)
          (Lazy.force n.left)
      in
      List.fold_left
        (fun acc entry ->
          Option.fold ~none:acc
            ~some:(fun c -> collect_node c acc)
            (Lazy.force entry.right))
        acc n.entries
    end
  and node_to_raw_for_export (node : node) : Raw.node =
    let save_child child =
      let raw = node_to_raw_for_export child in
      let data = Raw.encode_bytes raw in
      Cid.create `Dag_cbor data
    in
    let l = Option.map save_child (Lazy.force node.left) in
    let _, entries =
      List.fold_left
        (fun (prev_key, acc) entry ->
          let p = shared_prefix_len prev_key entry.key in
          let k = String.sub entry.key p (String.length entry.key - p) in
          let t = Option.map save_child (Lazy.force entry.right) in
          let raw_entry : Raw.entry = { p; k; v = entry.value; t } in
          (entry.key, raw_entry :: acc))
        ("", []) node.entries
    in
    { l; e = List.rev entries }
  in
  List.to_seq (List.rev (collect_node node []))

(* Diff *)

type diff_op =
  [ `Add of string * Cid.t
  | `Update of string * Cid.t * Cid.t
  | `Remove of string * Cid.t ]

let diff ~old ~new_ ~(store : Blockstore.readable) =
  let old_leaves = leaves old ~store |> List.of_seq in
  let new_leaves = leaves new_ ~store |> List.of_seq in
  let old_map = List.rev_map (fun (k, v) -> (k, v)) old_leaves in
  let new_map = List.rev_map (fun (k, v) -> (k, v)) new_leaves in
  (* Find additions and updates *)
  let ops =
    List.fold_left
      (fun acc (k, new_v) ->
        match List.assoc_opt k old_map with
        | None -> `Add (k, new_v) :: acc
        | Some old_v when not (Cid.equal old_v new_v) ->
            `Update (k, old_v, new_v) :: acc
        | Some _ -> acc)
      [] new_map
  in
  (* Find removals *)
  let ops =
    List.fold_left
      (fun acc (k, old_v) ->
        if not (List.mem_assoc k new_map) then `Remove (k, old_v) :: acc
        else acc)
      ops old_map
  in
  List.to_seq (List.rev ops)
