(** Group representation and operations for Zarr v3 *)

open Ztypes

(** Group handle with store and metadata *)
type 'store t = {
  store : 'store;
  path : string;
  metadata : group_metadata;
}

(** Module type for store operations needed by group *)
module type STORE_OPS = sig
  type t
  val get : t -> string -> bytes option
  val set : t -> string -> bytes -> unit
  val erase : t -> string -> unit
  val exists : t -> string -> bool
  val list_dir : t -> string -> string list * string list
end

(** Functor to create group operations for a given store type *)
module Make (S : STORE_OPS) = struct
  type store = S.t
  type nonrec t = S.t t

  (** Create a new group *)
  let create store ~path ?attributes () =
    let metadata = Metadata.create_group_metadata ?attributes () in

    (* Write metadata *)
    let meta_path = Chunk_key.metadata_path path in
    let meta_json = Metadata.group_to_json metadata in
    S.set store meta_path (Bytes.of_string meta_json);

    Ok { store; path; metadata }

  (** Open an existing group *)
  let open_ store ~path =
    let meta_path = Chunk_key.metadata_path path in
    match S.get store meta_path with
    | None -> Error (`Not_found ("group not found: " ^ path))
    | Some meta_bytes ->
      match Metadata.group_of_json (Bytes.to_string meta_bytes) with
      | Error e -> Error e
      | Ok metadata -> Ok { store; path; metadata }

  (** Get group metadata *)
  let metadata group = group.metadata

  (** Get group path *)
  let path group = group.path

  (** List children (both arrays and groups) *)
  let children group =
    let dir_path = if group.path = "" || group.path = "/" then "" else group.path ^ "/" in
    let (files, dirs) = S.list_dir group.store dir_path in
    let dir_path_len = String.length dir_path in

    (* Filter for zarr.json files and get their parent names *)
    let from_files = List.filter_map (fun f ->
      if String.ends_with ~suffix:"/zarr.json" f then
        let parent = String.sub f 0 (String.length f - String.length "/zarr.json") in
        if String.contains parent '/' then
          let idx = String.rindex parent '/' in
          Some (String.sub parent (idx + 1) (String.length parent - idx - 1))
        else
          Some parent
      else if f = "zarr.json" then
        None
      else
        None
    ) files in

    (* Extract child names from directories (remove prefix and trailing slash) *)
    let from_dirs = List.filter_map (fun d ->
      if String.length d > dir_path_len then
        let rest = String.sub d dir_path_len (String.length d - dir_path_len) in
        (* Remove trailing slash if present *)
        let rest = if String.ends_with ~suffix:"/" rest then
          String.sub rest 0 (String.length rest - 1)
        else rest in
        if rest <> "" then Some rest else None
      else None
    ) dirs in

    List.sort_uniq String.compare (from_files @ from_dirs)

  (** Get the metadata path for a child node *)
  let child_meta_path group name =
    if group.path = "" || group.path = "/" then
      name ^ "/zarr.json"
    else
      group.path ^ "/" ^ name ^ "/zarr.json"

  (** Check if a child exists *)
  let child_exists group name =
    S.exists group.store (child_meta_path group name)

  (** Get the type of a child (Array or Group) *)
  let child_type group name =
    match S.get group.store (child_meta_path group name) with
    | None -> None
    | Some bytes ->
      let json_str = Bytes.to_string bytes in
      try
        let json = Yojson.Safe.from_string json_str in
        let open Yojson.Safe.Util in
        match json |> member "node_type" |> to_string with
        | "array" -> Some `Array
        | "group" -> Some `Group
        | _ -> None
      with _ -> None

  (** Get group attributes *)
  let attrs group = Option.value ~default:`Null group.metadata.attributes

  (** Set group attributes *)
  let set_attrs group new_attrs =
    let metadata = { group.metadata with attributes = Some new_attrs } in
    let meta_path = Chunk_key.metadata_path group.path in
    let meta_json = Metadata.group_to_json metadata in
    S.set group.store meta_path (Bytes.of_string meta_json)

  (** Delete the group (metadata only - children remain) *)
  let delete group =
    let meta_path = Chunk_key.metadata_path group.path in
    S.erase group.store meta_path
end

(** Hierarchy operations *)
module Hierarchy = struct
  module type STORE_OPS = sig
    type t
    val get : t -> string -> bytes option
    val exists : t -> string -> bool
    val erase_prefix : t -> string -> unit
    val list_prefix : t -> string -> string list
  end

  module Make (S : STORE_OPS) = struct
    type store = S.t

    (** Walk all nodes in the hierarchy *)
    let walk store f =
      let all_keys = S.list_prefix store "" in
      let meta_files = List.filter (fun k ->
        String.ends_with ~suffix:"zarr.json" k
      ) all_keys in

      List.iter (fun meta_path ->
        let node_path =
          if meta_path = "zarr.json" then "/"
          else
            let len = String.length meta_path - String.length "/zarr.json" in
            "/" ^ String.sub meta_path 0 len
        in
        match S.get store meta_path with
        | None -> ()
        | Some bytes ->
          let json_str = Bytes.to_string bytes in
          (try
             let json = Yojson.Safe.from_string json_str in
             let open Yojson.Safe.Util in
             match json |> member "node_type" |> to_string with
             | "array" -> f node_path `Array
             | "group" -> f node_path `Group
             | _ -> ()
           with _ -> ())
      ) meta_files

    (** Check if a node exists *)
    let exists store path =
      let meta_path = Chunk_key.metadata_path path in
      S.exists store meta_path

    (** Delete a node and all its children *)
    let delete store path =
      let prefix = if path = "/" then "" else path ^ "/" in
      S.erase_prefix store prefix;
      (* Also delete the node's own metadata if not root *)
      if path <> "/" then begin
        let meta_path = Chunk_key.metadata_path path in
        S.erase_prefix store meta_path
      end
  end
end
