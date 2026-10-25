exception Error of string

let fail message = raise (Error message)

let io f =
  try f () with
  | Eioext.Native.Error message -> fail message
  | Eioext.Native.Busy _ -> fail "Secret store is busy"
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      fail "Secret file or key is missing. Initialize with sekrit init if new."
  | Unix.Unix_error _ | Sys_error _ ->
      fail "Cannot access private secret storage"

module Secret = struct
  type t = string

  let of_string value = value
  let with_value value f = f value
  let pp ppf _ = Format.pp_print_string ppf "<secret>"
end

let line value =
  let value =
    if String.ends_with ~suffix:"\r\n" value then
      String.sub value 0 (String.length value - 2)
    else if String.ends_with ~suffix:"\n" value then
      String.sub value 0 (String.length value - 1)
    else value
  in
  if
    value = "" || String.contains value '\n' || String.contains value '\r'
    || String.contains value '\000'
  then fail "Secret must contain one nonempty line";
  value

let read_file ?(max_bytes = 65536) path =
  io (fun () ->
      try Eioext.Native.read_private ~max_bytes path |> line
      with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
        fail "Secret file is missing")

let single_line = line

let validate_name name =
  if
    name = ""
    || String.length name > 256
    || (not
          (String.for_all
             (function
               | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' | '/' ->
                   true
               | _ -> false)
             name))
    || List.exists
         (fun part -> part = "" || part = "." || part = "..")
         (String.split_on_char '/' name)
  then fail "Secret name must contain nonempty ASCII name components"

module Reference = struct
  type t = File of string | Named of string

  let file path = File path

  let named name =
    validate_name name;
    Named name

  let of_string text =
    if String.starts_with ~prefix:"sekrit:" text then
      named (String.sub text 7 (String.length text - 7))
    else file text

  let to_string = function File path -> path | Named name -> "sekrit:" ^ name
  let source = function File path -> `File path | Named name -> `Named name
  let pp ppf value = Format.pp_print_string ppf (to_string value)
end

module Backend = struct
  type t = { find : string -> Secret.t option }

  let v ~find = { find }
end

let resolve ~backend = function
  | Reference.File path -> Secret.of_string (read_file path)
  | Reference.Named name -> (
      match backend.Backend.find name with
      | Some value -> value
      | None -> fail "Named secret is not configured")

module Source = struct
  type t = unit -> Secret.t

  let v ~backend reference () = resolve ~backend reference
  let get source = source ()

  let snapshot source =
    let value = get source in
    fun () -> value
end

module Paths = struct
  let override name default =
    match Sys.getenv_opt name with
    | None | Some "" -> default ()
    | Some path ->
        if Filename.is_relative path then fail (name ^ " must be absolute");
        path

  let root () =
    override "SEKRIT_STORE_DIR" (fun () ->
        let xdg = Xdg.create ~env:Sys.getenv_opt () in
        Filename.concat (Xdg.data_dir xdg) "sekrit")

  let key_file () =
    override "SEKRIT_KEY_FILE" (fun () ->
        let xdg = Xdg.create ~env:Sys.getenv_opt () in
        Filename.concat (Xdg.config_dir xdg) "sekrit/master.key")
end

let exists path =
  match Unix.lstat path with
  | _ -> true
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false

let rec absolute path =
  if exists path then Unix.realpath path
  else
    let parent = Filename.dirname path in
    if parent = path then path
    else Filename.concat (absolute parent) (Filename.basename path)

let separate ~key_file ~directory =
  let key = absolute key_file and root = absolute directory in
  if key = root || String.starts_with ~prefix:(root ^ "/") key then
    fail "Master key must be outside encrypted storage"

module Key = struct
  let create path =
    io (fun () ->
        Eioext.Native.mkdir_private (Filename.dirname path);
        Eioext.Native.require_private_dir (Filename.dirname path);
        if exists path then fail "Master key already exists";
        Eioext.Native.create_private path (Mirage_crypto_rng_unix.getrandom 32))
end

module Envelope = struct
  let magic = "SEKRIT\000\001"
  let max_bytes = 4 * 1024 * 1024
  let is_sealed data = String.starts_with ~prefix:magic data

  let key secret =
    let bytes = Secret.with_value secret Fun.id in
    if String.length bytes <> 32 then fail "Invalid master key length";
    Mirage_crypto.Chacha20.of_secret bytes

  let associated context =
    if context = "" || String.length context > 4096 then
      invalid_arg "Sekrit: invalid encryption context";
    magic ^ context

  let open_ ~key:secret ~context envelope =
    if
      (not (is_sealed envelope))
      || String.length envelope < 36
      || String.length envelope > max_bytes + 36
    then fail "Invalid encrypted secret envelope";
    let nonce = String.sub envelope 8 12 in
    let ciphertext = String.sub envelope 20 (String.length envelope - 20) in
    match
      Mirage_crypto.Chacha20.authenticate_decrypt ~key:(key secret) ~nonce
        ~adata:(associated context) ciphertext
    with
    | Some value -> value
    | None -> fail "Secret authentication failed (wrong key or altered data)"

  let seal ~key:secret ~context plaintext =
    if String.length plaintext > max_bytes then fail "Secret exceeds size limit";
    let key = key secret in
    let nonce = Mirage_crypto_rng_unix.getrandom 12 in
    let ciphertext =
      Mirage_crypto.Chacha20.authenticate_encrypt ~key ~nonce
        ~adata:(associated context) plaintext
    in
    magic ^ nonce ^ ciphertext
end

module Sealed_file = struct
  let is_sealed = Envelope.is_sealed

  let read ~key_file ~context path =
    io (fun () ->
        separate ~key_file ~directory:(Filename.dirname path);
        let key =
          Eioext.Native.read_private ~max_bytes:32 key_file |> Secret.of_string
        in
        let envelope =
          Eioext.Native.read_private ~max_bytes:(Envelope.max_bytes + 36) path
        in
        Envelope.open_ ~key ~context envelope)

  let write ~key_file ~context path plaintext =
    io (fun () ->
        separate ~key_file ~directory:(Filename.dirname path);
        Eioext.Native.require_private_dir (Filename.dirname path);
        let key =
          Eioext.Native.read_private ~max_bytes:32 key_file |> Secret.of_string
        in
        Eioext.Native.atomic_write path (Envelope.seal ~key ~context plaintext))
end

module Store = struct
  type storage = { read : unit -> string; update : (string -> string) -> unit }
  type t = { native : (string * string) option; storage : storage }

  let codec =
    Jsont.list
      (Jsont.t2
         ~dec:(fun name value -> (name, value))
         ~enc:(fun (name, value) i -> if i = 0 then name else value)
         Jsont.string)

  let decode raw =
    match Jsont_bytesrw.decode_string codec raw with
    | Error _ -> fail "Invalid encrypted secret store"
    | Ok entries ->
        List.iter (fun (name, _) -> validate_name name) entries;
        if
          List.length entries
          <> List.length (List.sort_uniq String.compare (List.map fst entries))
        then fail "Duplicate secret names";
        entries

  let encode entries =
    match Jsont_bytesrw.encode_string codec entries with
    | Error _ -> fail "Cannot encode secret store"
    | Ok raw -> raw

  let of_storage ~read ~update = { native = None; storage = { read; update } }

  let v ?root ?key_file () =
    let root = match root with Some root -> root | None -> Paths.root () in
    let key_file =
      match key_file with Some path -> path | None -> Paths.key_file ()
    in
    let path = Filename.concat root "vault.sekrit" in
    let locked f =
      io (fun () ->
          Eioext.Native.require_private_dir root;
          Eioext.Native.with_lock ~path:(Filename.concat root ".lock") f)
    in
    let load () = Sealed_file.read ~key_file ~context:"sekrit/vault/v1" path in
    let save raw =
      Sealed_file.write ~key_file ~context:"sekrit/vault/v1" path raw
    in
    {
      native = Some (root, key_file);
      storage =
        {
          read = (fun () -> locked load);
          update = (fun change -> locked (fun () -> save (change (load ()))));
        };
    }

  let init t =
    io (fun () ->
        match t.native with
        | None ->
            invalid_arg
              "Sekrit.Store.init: supplied storage owns initialization"
        | Some (root, key_file) ->
            separate ~key_file ~directory:root;
            Eioext.Native.mkdir_private root;
            Eioext.Native.require_private_dir root;
            Eioext.Native.with_lock ~path:(Filename.concat root ".lock") (fun () ->
                let path = Filename.concat root "vault.sekrit" in
                if exists path then
                  ignore
                    (decode
                       (Sealed_file.read ~key_file ~context:"sekrit/vault/v1"
                          path))
                else (
                  if not (exists key_file) then Key.create key_file;
                  Sealed_file.write ~key_file ~context:"sekrit/vault/v1" path
                    (encode []))))

  let list t =
    decode (t.storage.read ()) |> List.map fst |> List.sort String.compare

  let find t name =
    validate_name name;
    Option.map Secret.of_string
      (List.assoc_opt name (decode (t.storage.read ())))

  let put t ~name ~replace value =
    validate_name name;
    Secret.with_value value (fun value ->
        if String.length value > 65536 then fail "Secret exceeds size limit";
        t.storage.update (fun raw ->
            let entries = decode raw in
            if List.mem_assoc name entries && not replace then
              fail "Secret already exists; use replace explicitly";
            encode ((name, value) :: List.remove_assoc name entries)))

  let remove t name =
    validate_name name;
    t.storage.update (fun raw ->
        let entries = decode raw in
        if not (List.mem_assoc name entries) then
          fail "Named secret is not configured";
        encode (List.remove_assoc name entries))

  let backend t = Backend.v ~find:(find t)
end

let read_reference ?max_bytes reference =
  match Reference.of_string reference with
  | Reference.File path -> read_file ?max_bytes path
  | Reference.Named name -> (
      match Store.find (Store.v ()) name with
      | Some secret ->
          Secret.with_value secret (fun value ->
              if String.length value > Option.value max_bytes ~default:65536
              then fail "Secret exceeds size limit";
              line value)
      | None -> fail "Named secret is not configured")
