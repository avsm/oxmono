(** Filesystem store implementation for Zarr using Unix I/O *)

(** Filesystem store *)
type t = {
  root : string;
}

(** Create a filesystem store at the given root directory *)
let create root =
  (* Ensure root exists *)
  if not (Sys.file_exists root) then
    Unix.mkdir root 0o755;
  { root }

(** Open an existing filesystem store *)
let open_ root =
  if Sys.file_exists root && Sys.is_directory root then
    Some { root }
  else
    None

(** Convert a key to a filesystem path *)
let key_to_path store key =
  Filename.concat store.root key

(** Ensure parent directories exist *)
let ensure_parent_dirs path =
  let dir = Filename.dirname path in
  let rec mkdir_p d =
    if not (Sys.file_exists d) then begin
      mkdir_p (Filename.dirname d);
      try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end
  in
  mkdir_p dir

(** Get the full contents of a key *)
let get store key =
  let path = key_to_path store key in
  if Sys.file_exists path && not (Sys.is_directory path) then begin
    let ic = open_in_bin path in
    let len = in_channel_length ic in
    let bytes = Bytes.create len in
    really_input ic bytes 0 len;
    close_in ic;
    Some bytes
  end else
    None

(** Get partial content from a key *)
let get_partial store key ranges =
  let path = key_to_path store key in
  if Sys.file_exists path && not (Sys.is_directory path) then begin
    let ic = open_in_bin path in
    let file_len = in_channel_length ic in
    let result = List.map (fun (offset, length) ->
      let length = Option.value ~default:(file_len - offset) length in
      let offset = max 0 (min offset file_len) in
      let length = max 0 (min length (file_len - offset)) in
      seek_in ic offset;
      let bytes = Bytes.create length in
      really_input ic bytes 0 length;
      bytes
    ) ranges in
    close_in ic;
    Some result
  end else
    None

(** Check if a key exists *)
let exists store key =
  let path = key_to_path store key in
  Sys.file_exists path && not (Sys.is_directory path)

(** Set the contents of a key *)
let set store key bytes =
  let path = key_to_path store key in
  ensure_parent_dirs path;
  let oc = open_out_bin path in
  output_bytes oc bytes;
  close_out oc

(** Set partial content in a key *)
let set_partial store updates =
  List.iter (fun (key, offset, bytes) ->
    let path = key_to_path store key in
    ensure_parent_dirs path;
    let fd = Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT] 0o644 in
    ignore (Unix.lseek fd offset Unix.SEEK_SET);
    ignore (Unix.write fd bytes 0 (Bytes.length bytes));
    Unix.close fd
  ) updates

(** Erase a key *)
let erase store key =
  let path = key_to_path store key in
  if Sys.file_exists path && not (Sys.is_directory path) then
    Sys.remove path

(** Recursively remove a directory *)
let rec remove_dir path =
  if Sys.file_exists path then begin
    if Sys.is_directory path then begin
      let entries = Sys.readdir path in
      Array.iter (fun entry ->
        remove_dir (Filename.concat path entry)
      ) entries;
      Unix.rmdir path
    end else
      Sys.remove path
  end

(** Erase all keys with given prefix *)
let erase_prefix store prefix =
  let prefix_path = key_to_path store prefix in
  if Sys.file_exists prefix_path then begin
    if Sys.is_directory prefix_path then
      remove_dir prefix_path
    else
      Sys.remove prefix_path
  end

(** List all files recursively under a path *)
let rec list_files_recursive path prefix acc =
  if Sys.file_exists path then begin
    if Sys.is_directory path then begin
      let entries = Sys.readdir path in
      Array.fold_left (fun acc entry ->
        let full_path = Filename.concat path entry in
        let new_prefix = if prefix = "" then entry else prefix ^ "/" ^ entry in
        list_files_recursive full_path new_prefix acc
      ) acc entries
    end else
      prefix :: acc
  end else
    acc

(** List all keys in the store *)
let list store =
  list_files_recursive store.root "" []

(** List all keys with given prefix *)
let list_prefix store prefix =
  let keys = list store in
  List.filter (String.starts_with ~prefix) keys

(** List directory contents *)
let list_dir store prefix =
  let path = key_to_path store prefix in
  if Sys.file_exists path && Sys.is_directory path then begin
    let entries = Sys.readdir path in
    let (keys, prefixes) =
      Array.fold_left (fun (keys, prefixes) entry ->
        let full_path = Filename.concat path entry in
        let key = if prefix = "" then entry else prefix ^ entry in
        if Sys.is_directory full_path then
          (keys, (key ^ "/") :: prefixes)
        else
          (key :: keys, prefixes)
      ) ([], []) entries
    in
    (List.sort String.compare keys, List.sort String.compare prefixes)
  end else
    ([], [])

(** Get the root path *)
let root store = store.root
