(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Opening a store and finding the nodes in it.

   A node path is held here as a relative string with no slash at
   either end, so the root is [""]. {!disp} puts it back into the
   absolute form a user typed and reads.

   The root document is read once, when the store is opened, and in one
   request. Every command needs it: it is the only place consolidated
   metadata can be, and the extensions block reports whether that
   metadata is there. A hierarchy whose root carries no document of its
   own is still readable, so the root is an option rather than a
   failure. *)

module Store = Zarrz.Store
module Metadata = Zarrz.Metadata
module Chunk_key = Zarrz.Chunk_key
module Consolidated = Zarrz.Consolidated

type meta = [ `Array of Metadata.array_meta | `Group of Metadata.group_meta ]

type t = {
  spec : string;  (* The store argument exactly as the user gave it. *)
  store : Store.t;
  root : Jsont.json option;
  root_group : Metadata.group_meta option;
      (* None when the root has no document or is an array. *)
  cons : Consolidated.t option;
}

type source =
  | From_consolidated  (* The node map in the root document. *)
  | From_listing  (* A walk of the store's keys. *)
  | From_neither  (* Only the named node can be shown. *)

(* {1 Paths} *)

let norm p =
  let n = String.length p in
  let i = ref 0 and j = ref n in
  while !i < !j && p.[!i] = '/' do
    incr i
  done;
  while !j > !i && p.[!j - 1] = '/' do
    decr j
  done;
  String.sub p !i (!j - !i)

let disp p = if String.equal p "" then "/" else "/" ^ p
let join base p = if String.equal base "" then p else base ^ "/" ^ p

(* The two key mappings the specification defines, taken from the
   library rather than spelled again, so the command reads the keys the
   library writes. *)
let meta_key p = Chunk_key.meta_key ~path:p
let data_prefix p = Chunk_key.data_key ~path:p ""

let base_name p =
  match String.rindex_opt p '/' with
  | None -> p
  | Some i -> String.sub p (i + 1) (String.length p - i - 1)

let parent p =
  match String.rindex_opt p '/' with None -> "" | Some i -> String.sub p 0 i

(* [under ~base p] is [p] relative to [base] when [p] is strictly below
   it, and [None] otherwise. *)
let under ~base p =
  if String.equal base "" then if String.equal p "" then None else Some p
  else
    let n = String.length base in
    if String.length p > n && String.starts_with ~prefix:(base ^ "/") p then
      Some (String.sub p (n + 1) (String.length p - n - 1))
    else None

let depth p =
  if String.equal p "" then 0
  else String.fold_left (fun n c -> if c = '/' then n + 1 else n) 1 p

(* {1 Opening} *)

let is_url s =
  String.starts_with ~prefix:"https://" s
  || String.starts_with ~prefix:"http://" s

let rec chop_slash s =
  let n = String.length s in
  if n > 1 && s.[n - 1] = '/' then chop_slash (String.sub s 0 (n - 1)) else s

let store_of_spec ~sw env spec =
  if is_url spec then
    Zarrz_fetch.store ~base_url:(chop_slash spec) (Fetch_curl.std ~sw env)
  else Zarrz_eio.store Eio.Path.(Eio.Stdenv.fs env / spec)

(* One [get], never a probe followed by a read, so that a consolidated
   store really does cost one request. *)
let json_opt store ~key =
  match store.Store.get ~key with
  | None -> None
  | Some b -> (
      match
        Jsont_bytesrw.decode_string Jsont.json (Base_bigstring.to_string b)
      with
      | Ok j -> Some j
      | Error m -> Zarrz.Error.raise_ (Zarrz.Error.Metadata (key ^ ": " ^ m)))

let open_ ~sw env spec =
  let store = store_of_spec ~sw env spec in
  let root = json_opt store ~key:"zarr.json" in
  let root_group =
    Option.bind root (fun j -> Result.to_option (Metadata.group_of_json j))
  in
  let cons = Option.bind root_group Consolidated.of_group in
  { spec; store; root; root_group; cons }

(* {1 Node documents} *)

let meta_of_json ~path j =
  let fail m =
    Zarrz.Error.raise_ (Zarrz.Error.Metadata (disp path ^ ": " ^ m))
  in
  let of_result = function Ok v -> v | Error m -> fail m in
  match j with
  | Jsont.Object (o, _) -> (
      match Jsont.Json.find_mem "node_type" o with
      | Some (_, Jsont.String ("array", _)) ->
          `Array (of_result (Metadata.array_of_json j))
      | Some (_, Jsont.String ("group", _)) ->
          `Group (of_result (Metadata.group_of_json j))
      | Some (_, Jsont.String (s, _)) -> fail ("unknown node_type " ^ s)
      | Some _ -> fail "node_type is not a string"
      | None -> fail "no node_type member")
  | _ -> fail "the metadata document is not an object"

(* The consolidated map answers without a request. Otherwise the
   document is read from the store, which raises when it is not
   there. *)
let json_opt_at t ~path =
  if String.equal path "" then t.root
  else
    match Option.bind t.cons (fun c -> Consolidated.node c path) with
    | Some j -> Some j
    | None -> json_opt t.store ~key:(meta_key path)

let json_at t ~path =
  match json_opt_at t ~path with
  | Some j -> j
  | None ->
      Zarrz.Error.raise_ (Zarrz.Error.Store (meta_key path ^ ": not found"))

let meta_at t ~path = meta_of_json ~path (json_at t ~path)

(* [meta_opt_at t ~path] is [None] for a node the hierarchy implies but
   no document describes, which the root of a store whose writer never
   wrote one is. *)
let meta_opt_at t ~path = Option.map (meta_of_json ~path) (json_opt_at t ~path)

(* {1 Enumeration} *)

(* The consolidated tier is used only when the map really covers the
   requested path, so a node outside it still falls through to a key
   listing rather than being reported as childless. *)
let covered t ~path =
  match t.cons with
  | None -> false
  | Some c -> String.equal path "" || Consolidated.node c path <> None

let listing t ~path =
  match t.store.Store.list with
  | None -> None
  | Some list ->
      let prefix = data_prefix path in
      let keep k =
        if String.equal (Filename.basename k) "zarr.json" then
          let d = Filename.dirname k in
          under ~base:path (if String.equal d "." then "" else d)
        else None
      in
      Some (List.filter_map keep (list ~prefix))

(* [descendants t ~path] are the paths of the nodes strictly below
   [path], each relative to [path], and how they were found. *)
let descendants t ~path =
  if covered t ~path then
    let c = Option.get t.cons in
    ( From_consolidated,
      List.filter_map (under ~base:path) (Consolidated.paths c) )
  else
    match listing t ~path with
    | Some l -> (From_listing, l)
    | None -> (From_neither, [])
