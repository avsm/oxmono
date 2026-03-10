(** Filesystem store implementation for Zarr using Eio *)

(** Filesystem store *)
type t = {
  root : Eio.Fs.dir_ty Eio.Path.t;
}

(** Create a filesystem store at the given root directory *)
let create ~fs root_path =
  let root = Eio.Path.(fs / root_path) in
  (* Ensure root exists *)
  (try Eio.Path.mkdir ~perm:0o755 root
   with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ());
  { root }

(** Open an existing filesystem store *)
let open_ ~fs root_path =
  let root = Eio.Path.(fs / root_path) in
  match Eio.Path.kind ~follow:true root with
  | `Directory -> Some { root }
  | _ -> None
  | exception _ -> None

(** Convert a key to an Eio path *)
let key_to_path store key =
  Eio.Path.(store.root / key)

(** Ensure parent directories exist *)
let ensure_parent_dirs path =
  let rec ensure p =
    let parent = Eio.Path.split p |> Option.map fst in
    match parent with
    | None -> ()
    | Some parent_path ->
      (try
         match Eio.Path.kind ~follow:true parent_path with
         | `Directory -> ()
         | _ -> ensure parent_path
       with Eio.Io _ ->
         ensure parent_path;
         try Eio.Path.mkdir ~perm:0o755 parent_path
         with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ())
  in
  ensure path

(** Get the full contents of a key *)
let get store key =
  let path = key_to_path store key in
  try
    match Eio.Path.kind ~follow:true path with
    | `Regular_file ->
      let content = Eio.Path.load path in
      Some (Bytes.of_string content)
    | _ -> None
  with Eio.Io _ -> None

(** Get partial content from a key *)
let get_partial store key ranges =
  match get store key with
  | None -> None
  | Some bytes ->
    let len = Bytes.length bytes in
    Some (List.map (fun (offset, length) ->
      let length = match length with
        | Some l -> l
        | None -> len - offset
      in
      let offset = max 0 (min offset len) in
      let length = max 0 (min length (len - offset)) in
      Bytes.sub bytes offset length
    ) ranges)

(** Check if a key exists *)
let exists store key =
  let path = key_to_path store key in
  try
    match Eio.Path.kind ~follow:true path with
    | `Regular_file -> true
    | _ -> false
  with Eio.Io _ -> false

(** Set the contents of a key *)
let set store key bytes =
  let path = key_to_path store key in
  ensure_parent_dirs path;
  Eio.Path.save ~create:(`Or_truncate 0o644) path (Bytes.to_string bytes)

(** Set partial content in a key - falls back to read-modify-write *)
let set_partial store updates =
  List.iter (fun (key, offset, bytes) ->
    let path = key_to_path store key in
    ensure_parent_dirs path;
    let existing = match get store key with
      | Some b -> b
      | None -> Bytes.empty
    in
    let existing_len = Bytes.length existing in
    let new_len = max existing_len (offset + Bytes.length bytes) in
    let new_bytes = Bytes.make new_len '\x00' in
    Bytes.blit existing 0 new_bytes 0 existing_len;
    Bytes.blit bytes 0 new_bytes offset (Bytes.length bytes);
    Eio.Path.save ~create:(`Or_truncate 0o644) path (Bytes.to_string new_bytes)
  ) updates

(** Erase a key *)
let erase store key =
  let path = key_to_path store key in
  try Eio.Path.unlink path
  with Eio.Io _ -> ()

(** Recursively remove a directory *)
let rec remove_recursive path =
  try
    match Eio.Path.kind ~follow:false path with
    | `Directory ->
      Eio.Path.read_dir path |> List.iter (fun entry ->
        remove_recursive Eio.Path.(path / entry)
      );
      Eio.Path.rmdir path
    | _ -> Eio.Path.unlink path
  with Eio.Io _ -> ()

(** Erase all keys with given prefix *)
let erase_prefix store prefix =
  if prefix = "" then
    (* Don't delete root, just contents *)
    Eio.Path.read_dir store.root |> List.iter (fun entry ->
      remove_recursive Eio.Path.(store.root / entry)
    )
  else
    remove_recursive (key_to_path store prefix)

(** List all files recursively under a path *)
let rec list_files_recursive path prefix acc =
  try
    match Eio.Path.kind ~follow:true path with
    | `Directory ->
      Eio.Path.read_dir path |> List.fold_left (fun acc entry ->
        let full_path = Eio.Path.(path / entry) in
        let new_prefix = if prefix = "" then entry else prefix ^ "/" ^ entry in
        list_files_recursive full_path new_prefix acc
      ) acc
    | `Regular_file -> prefix :: acc
    | _ -> acc
  with Eio.Io _ -> acc

(** List all keys in the store *)
let list store =
  list_files_recursive store.root "" []

(** List all keys with given prefix *)
let list_prefix store prefix =
  let keys = list store in
  List.filter (fun k ->
    String.length k >= String.length prefix &&
    String.sub k 0 (String.length prefix) = prefix
  ) keys

(** List directory contents *)
let list_dir store prefix =
  let path = if prefix = "" then store.root else key_to_path store prefix in
  try
    match Eio.Path.kind ~follow:true path with
    | `Directory ->
      let entries = Eio.Path.read_dir path in
      let keys = ref [] in
      let prefixes = ref [] in
      List.iter (fun entry ->
        let full_path = Eio.Path.(path / entry) in
        let key = if prefix = "" then entry else prefix ^ entry in
        try
          match Eio.Path.kind ~follow:true full_path with
          | `Directory -> prefixes := (key ^ "/") :: !prefixes
          | `Regular_file -> keys := key :: !keys
          | _ -> ()
        with Eio.Io _ -> ()
      ) entries;
      (List.sort String.compare !keys, List.sort String.compare !prefixes)
    | _ -> ([], [])
  with Eio.Io _ -> ([], [])

(** Get the root path as a string *)
let root_path store =
  Eio.Path.native_exn store.root
