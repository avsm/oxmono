(** In-memory store implementation for Zarr *)

(** In-memory store using a hash table *)
type t = {
  mutable data : (string, bytes) Hashtbl.t;
}

(** Create a new empty memory store *)
let create () = { data = Hashtbl.create 64 }

(** Get the full contents of a key *)
let get store key =
  Hashtbl.find_opt store.data key

(** Get partial content from a key *)
let get_partial store key ranges =
  match Hashtbl.find_opt store.data key with
  | None -> None
  | Some bytes ->
    let len = Bytes.length bytes in
    Some (List.map (fun (offset, length) ->
      let length = Option.value ~default:(len - offset) length in
      let offset = max 0 (min offset len) in
      let length = max 0 (min length (len - offset)) in
      Bytes.sub bytes offset length
    ) ranges)

(** Check if a key exists *)
let exists store key =
  Hashtbl.mem store.data key

(** Set the contents of a key *)
let set store key bytes =
  Hashtbl.replace store.data key bytes

(** Set partial content in a key *)
let set_partial store updates =
  List.iter (fun (key, offset, bytes) ->
    match Hashtbl.find_opt store.data key with
    | None ->
      (* Create new key with data at offset *)
      let new_bytes = Bytes.make (offset + Bytes.length bytes) '\x00' in
      Bytes.blit bytes 0 new_bytes offset (Bytes.length bytes);
      Hashtbl.replace store.data key new_bytes
    | Some existing ->
      let existing_len = Bytes.length existing in
      let new_len = max existing_len (offset + Bytes.length bytes) in
      let new_bytes = Bytes.make new_len '\x00' in
      Bytes.blit existing 0 new_bytes 0 existing_len;
      Bytes.blit bytes 0 new_bytes offset (Bytes.length bytes);
      Hashtbl.replace store.data key new_bytes
  ) updates

(** Erase a key *)
let erase store key =
  Hashtbl.remove store.data key

(** Erase all keys with given prefix *)
let erase_prefix store prefix =
  let keys_to_remove = Hashtbl.fold (fun k _ acc ->
    if String.starts_with ~prefix k then
      k :: acc
    else
      acc
  ) store.data [] in
  List.iter (Hashtbl.remove store.data) keys_to_remove

(** List all keys in the store *)
let list store =
  Hashtbl.fold (fun k _ acc -> k :: acc) store.data []

(** List all keys with given prefix *)
let list_prefix store prefix =
  Hashtbl.fold (fun k _ acc ->
    if String.starts_with ~prefix k then
      k :: acc
    else
      acc
  ) store.data []

(** List directory contents *)
let list_dir store prefix =
  let prefix_len = String.length prefix in
  let (keys, prefixes) =
    Hashtbl.fold (fun k _ (keys, prefixes) ->
      if String.starts_with ~prefix k then
        let rest = String.sub k prefix_len (String.length k - prefix_len) in
        match String.index_opt rest '/' with
        | None -> (k :: keys, prefixes)
        | Some idx ->
          let subdir = String.sub rest 0 idx in
          let full_prefix = prefix ^ subdir ^ "/" in
          if List.mem full_prefix prefixes then (keys, prefixes)
          else (keys, full_prefix :: prefixes)
      else
        (keys, prefixes)
    ) store.data ([], [])
  in
  (List.sort String.compare keys, List.sort String.compare prefixes)

(** Clear all data in the store *)
let clear store =
  Hashtbl.clear store.data

(** Get the number of keys in the store *)
let length store =
  Hashtbl.length store.data
