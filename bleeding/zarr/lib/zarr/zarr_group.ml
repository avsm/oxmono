(** {1 Zarr v3 Group Operations}

    Groups are container nodes in the Zarr hierarchy.
    See Zarr v3.1 spec, "Group" section. *)

type 'store t = {
  store : 'store;
  path : string;
  metadata : Zarr_metadata.group_metadata;
}

module type STORE_OPS = sig
  type t
  val get : t -> string -> bytes option
  val set : t -> string -> bytes -> unit
  val erase : t -> string -> unit
  val exists : t -> string -> bool
  val list_dir : t -> string -> string list * string list
end

module Make (S : STORE_OPS) = struct
  type store = S.t
  type nonrec t = S.t t

  let create store ~path ?attributes () =
    let metadata = Zarr_metadata.create_group_metadata ?attributes () in
    let meta_path = Chunk_key.metadata_path path in
    S.set store meta_path (Bytes.of_string (Zarr_metadata.group_to_json metadata));
    { store; path; metadata }

  let open_ store ~path =
    let meta_path = Chunk_key.metadata_path path in
    match S.get store meta_path with
    | None -> failwith ("group not found: " ^ path)
    | Some bytes ->
      let metadata = Zarr_metadata.group_of_json (Bytes.to_string bytes) in
      { store; path; metadata }

  let metadata g = g.metadata
  let path g = g.path

  let children g =
    let dir = if g.path = "" || g.path = "/" then "" else g.path ^ "/" in
    let (files, dirs) = S.list_dir g.store dir in
    let dir_len = String.length dir in
    let from_files = List.filter_map (fun f ->
      if String.ends_with ~suffix:"/zarr.json" f then
        let parent = String.sub f 0 (String.length f - 10) in
        if String.contains parent '/' then
          let i = String.rindex parent '/' in
          Some (String.sub parent (i + 1) (String.length parent - i - 1))
        else Some parent
      else None
    ) files in
    let from_dirs = List.filter_map (fun d ->
      if String.length d > dir_len then
        let rest = String.sub d dir_len (String.length d - dir_len) in
        let rest = if String.ends_with ~suffix:"/" rest then
            String.sub rest 0 (String.length rest - 1) else rest in
        if rest <> "" then Some rest else None
      else None
    ) dirs in
    List.sort_uniq String.compare (from_files @ from_dirs)

  let child_meta_path g name =
    if g.path = "" || g.path = "/" then name ^ "/zarr.json"
    else g.path ^ "/" ^ name ^ "/zarr.json"

  let child_exists g name = S.exists g.store (child_meta_path g name)

  let child_type g name =
    match S.get g.store (child_meta_path g name) with
    | None -> None
    | Some bytes ->
      (try
         let json = Json_util.of_string (Bytes.to_string bytes) in
         match Json_util.(member "node_type" json |> to_string_opt) with
         | Some "array" -> Some `Array
         | Some "group" -> Some `Group
         | _ -> None
       with _ -> None)

  let attrs g = Option.value ~default:Json_util.null g.metadata.attributes

  let set_attrs g new_attrs =
    let metadata = { g.metadata with attributes = Some new_attrs } in
    let meta_path = Chunk_key.metadata_path g.path in
    S.set g.store meta_path (Bytes.of_string (Zarr_metadata.group_to_json metadata))

  let delete g = S.erase g.store (Chunk_key.metadata_path g.path)
end

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

    let walk store f =
      let all_keys = S.list_prefix store "" in
      List.iter (fun key ->
        if String.ends_with ~suffix:"zarr.json" key then
          let node_path =
            if key = "zarr.json" then "/"
            else "/" ^ String.sub key 0 (String.length key - 10) in
          match S.get store key with
          | None -> ()
          | Some bytes ->
            (try
               let json = Json_util.of_string (Bytes.to_string bytes) in
               match Json_util.(member "node_type" json |> to_string_opt) with
               | Some "array" -> f node_path `Array
               | Some "group" -> f node_path `Group
               | _ -> ()
             with _ -> ())
      ) all_keys

    let exists store path =
      S.exists store (Chunk_key.metadata_path path)

    let delete store path =
      let prefix = if path = "/" then "" else path ^ "/" in
      S.erase_prefix store prefix;
      if path <> "/" then
        S.erase_prefix store (Chunk_key.metadata_path path)
  end
end
