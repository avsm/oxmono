(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type credential =
  | Bearer of string
  | Basic of { user : string; password : string }

type t = { name : string; session_url : string; credential : credential }

type error =
  | Invalid_profile of string
  | Storage_error of { path : string; message : string }
  | Connection_error of Client.error

type store = { fs_path : Eio.Fs.dir_ty Eio.Path.t; directory : string }

let diagnostic = Httpz_media.sanitize_diagnostic

let pp_error ppf = function
  | Invalid_profile message ->
      Fmt.pf ppf "invalid profile: %s" (diagnostic message)
  | Storage_error { path; message } ->
      Fmt.pf ppf "%s: %s" (diagnostic path) (diagnostic message)
  | Connection_error error -> Client.pp_error ppf error

let error_to_string error = Fmt.str "%a" pp_error error

let valid_name name =
  let valid_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_' -> true
    | _ -> false
  in
  name <> "" && name <> "." && name <> ".." && String.for_all valid_char name

let one_line value =
  not (String.contains value '\n' || String.contains value '\r')

let contains_control value = not (String.equal value (diagnostic value))
let invalid fmt = Fmt.kstr (fun message -> Error (Invalid_profile message)) fmt

let invalid_name name =
  invalid
    "name %S must contain only letters, digits, '.', '-' and '_', and may not \
     be '.' or '..'"
    name

let v ~name ~session_url credential =
  let session_url = String.trim session_url in
  let ( let* ) = Result.bind in
  let* () = if valid_name name then Ok () else invalid_name name in
  let* () =
    if session_url = "" then invalid "%s has no session URL" name
    else if not (one_line session_url) then
      invalid "%s has a session URL containing a line ending" name
    else if contains_control session_url then
      invalid "%s has a session URL containing a control character" name
    else Ok ()
  in
  (* {!Auth.bearer} and {!Auth.basic} admit a b64token and printable ASCII,
     so neither a line ending nor any other control byte reaches a profile
     through them. *)
  let* credential =
    match credential with
    | Bearer token -> (
        match Auth.bearer token with
        | _ -> Ok (Bearer token)
        | exception Invalid_argument _ ->
            invalid "%s has an invalid bearer token" name)
    | Basic { user; password } -> (
        let user = String.trim user in
        if user = "" then invalid "%s has no basic-auth user" name
        else if password = "" then invalid "%s has no basic-auth password" name
        else
          match Auth.basic ~user ~password with
          | _ -> Ok (Basic { user; password })
          | exception Invalid_argument _ ->
              invalid "%s has an invalid basic-auth credential" name)
  in
  Ok { name; session_url; credential }

let name t = t.name
let session_url t = t.session_url
let credential t = t.credential

let auth_of_credential = function
  | Bearer token -> Auth.bearer token
  | Basic { user; password } -> Auth.basic ~user ~password

let auth t = auth_of_credential t.credential

(* A validated credential is one {!Auth.bearer} or {!Auth.basic} accepted, so
   rebuilding it here cannot fail. *)
let pp_credential ppf credential = Auth.pp ppf (auth_of_credential credential)

let pp ppf t =
  Fmt.pf ppf "@[<v>profile %s@,session URL: %s@,credential: %a@]" t.name
    (diagnostic t.session_url) pp_credential t.credential

let of_directory ~fs directory =
  { fs_path = Eio.Path.(fs / directory); directory }

let directory store = store.directory
let absolute path = not (Filename.is_relative path)

let config_home () =
  match Sys.getenv_opt "XDG_CONFIG_HOME" with
  | Some path when path <> "" && absolute path -> Ok path
  | Some _ | None -> (
      match Sys.getenv_opt "HOME" with
      | Some path when path <> "" && absolute path ->
          Ok (Filename.concat path ".config")
      | Some _ | None ->
          Error
            (Storage_error
               {
                 path = "JMAP profile store";
                 message =
                   "neither XDG_CONFIG_HOME nor an absolute HOME names a \
                    configuration directory";
               }))

let xdg_store env =
  Result.map
    (fun base ->
      let directory =
        List.fold_left Filename.concat base [ "jmap"; "profiles" ]
      in
      of_directory ~fs:(Eio.Stdenv.fs env) directory)
    (config_home ())

let path store name = Eio.Path.(store.fs_path / name)
let path_name store name = Filename.concat store.directory name

let storage_error ~operation path exn =
  Storage_error { path; message = Error_context.describe ~operation exn }

let with_operation operation message = Fmt.str "%s, %s" message operation

let with_io ~operation path f =
  match f () with
  | value -> Ok value
  | exception (Eio.Io _ as exn) -> Error (storage_error ~operation path exn)

let store_kind ~operation store =
  let inspect () =
    match Eio.Path.stat ~follow:true store.fs_path with
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> Ok `Missing
    | stat -> (
        match stat.Eio.File.Stat.kind with
        | `Directory ->
            if stat.Eio.File.Stat.perm land 0o077 = 0 then Ok `Directory
            else
              Error
                (Storage_error
                   {
                     path = store.directory;
                     message =
                       Fmt.str
                         "profile store has permissions %03o; remove group and \
                          other access (for example, chmod 700)"
                         stat.Eio.File.Stat.perm
                       |> with_operation operation;
                   })
        | _ ->
            Error
              (Storage_error
                 {
                   path = store.directory;
                   message =
                     with_operation operation "profile store is not a directory";
                 }))
  in
  match with_io ~operation store.directory inspect with
  | Error _ as error -> error
  | Ok result -> result

let max_file_bytes = 64 * 1024

type fields = {
  url : string option;
  scheme : Auth.scheme option;
  user : string option;
  secret : string option;
}

let empty_fields = { url = None; scheme = None; user = None; secret = None }

let drop_cr value =
  if String.ends_with ~suffix:"\r" value then
    String.sub value 0 (String.length value - 1)
  else value

let field line =
  match String.index_opt line '=' with
  | None -> None
  | Some index ->
      Some
        ( String.trim (String.sub line 0 index),
          drop_cr (String.sub line (index + 1) (String.length line - index - 1))
        )

let parse_fields contents =
  let set fields line =
    match field line with
    | Some ("url", value) -> { fields with url = Some (String.trim value) }
    | Some ("auth", value) ->
        let scheme =
          match String.lowercase_ascii (String.trim value) with
          | "bearer" -> Some Auth.Bearer
          | "basic" -> Some Auth.Basic
          | _ -> None
        in
        { fields with scheme }
    | Some ("user", value) -> { fields with user = Some (String.trim value) }
    | Some ("secret", value) -> { fields with secret = Some value }
    | Some _ | None -> fields
  in
  List.fold_left set empty_fields (String.split_on_char '\n' contents)

let parse ~name contents =
  let fields = parse_fields contents in
  let ( let* ) = Result.bind in
  let* session_url =
    match fields.url with
    | Some url -> Ok url
    | None -> invalid "%s has no url field" name
  in
  let* scheme =
    match fields.scheme with
    | Some scheme -> Ok scheme
    | None -> invalid "%s has no valid auth field" name
  in
  let* secret =
    match fields.secret with
    | Some secret -> Ok secret
    | None -> invalid "%s has no secret field" name
  in
  let* credential =
    match scheme with
    | Auth.Bearer -> Ok (Bearer secret)
    | Auth.Basic -> (
        match fields.user with
        | Some user -> Ok (Basic { user; password = secret })
        | None -> invalid "%s has no user field for basic authentication" name)
  in
  v ~name ~session_url credential

let load store name =
  if not (valid_name name) then invalid_name name
  else
    let ( let* ) = Result.bind in
    let operation = Fmt.str "loading profile %s" name in
    let* kind = store_kind ~operation store in
    if kind = `Missing then
      Error
        (Storage_error
           {
             path = store.directory;
             message = with_operation operation "profile store does not exist";
           })
    else
      let native_path = path_name store name in
      let refused message =
        Error
          (Storage_error
             { path = native_path; message = with_operation operation message })
      in
      let open_file () =
        Secret_file.with_open_in ~subject:"profile" ~refused (path store name)
        @@ fun input ->
        match
          Eio.Buf_read.parse ~max_size:(max_file_bytes + 1)
            Eio.Buf_read.take_all input
        with
        | Ok contents when String.length contents <= max_file_bytes ->
            parse ~name contents
        | Ok _ | Error _ -> invalid "%s exceeds %d bytes" name max_file_bytes
      in
      match with_io ~operation native_path open_file with
      | Error _ as error -> error
      | Ok result -> result

let list store =
  let ( let* ) = Result.bind in
  let operation = "listing JMAP profiles" in
  let* kind = store_kind ~operation store in
  match kind with
  | `Missing -> Ok []
  | `Directory ->
      with_io ~operation store.directory (fun () ->
          Eio.Path.read_dir store.fs_path
          |> List.filter valid_name |> List.sort String.compare
          |> List.filter_map (fun name -> Result.to_option (load store name)))

let scheme_to_string = function
  | Auth.Bearer -> "bearer"
  | Auth.Basic -> "basic"

let serialize t =
  let scheme, user, secret =
    match t.credential with
    | Bearer token -> (Auth.Bearer, "", token)
    | Basic { user; password } -> (Auth.Basic, user, password)
  in
  Fmt.str "url=%s\nauth=%s\nuser=%s\nsecret=%s\n" t.session_url
    (scheme_to_string scheme) user secret

let save_serial = Atomic.make 0

let save store profile =
  let contents = serialize profile in
  if String.length contents > max_file_bytes then
    invalid "%s exceeds %d bytes when encoded" profile.name max_file_bytes
  else
    let ( let* ) = Result.bind in
    let action = Fmt.str "saving profile %s" profile.name in
    let* () =
      with_io ~operation:action store.directory (fun () ->
          Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 store.fs_path)
    in
    let* _kind = store_kind ~operation:action store in
    let serial = Atomic.fetch_and_add save_serial 1 in
    let temporary_name =
      Fmt.str ".%s.tmp~%d-%d" profile.name (Unix.getpid ()) serial
    in
    let temporary = path store temporary_name in
    let destination = path store profile.name in
    let write () =
      Fun.protect
        ~finally:(fun () ->
          (* A cancelled fiber cannot unlink, and the temporary file would
             outlive the save. *)
          Eio.Cancel.protect (fun () ->
              try Eio.Path.unlink ~missing_ok:true temporary
              with Eio.Io _ -> ()))
        (fun () ->
          (* A process id can eventually be reused after an interrupted save. *)
          Eio.Path.unlink ~missing_ok:true temporary;
          Eio.Path.save ~create:(`Exclusive 0o600) temporary contents;
          Eio.Path.rename temporary destination)
    in
    with_io ~operation:action (path_name store profile.name) write

let connect ~sw ?transport ?timeout ?(allow_insecure = false) env profile =
  let result =
    match transport with
    | Some transport ->
        Client.connect ~sw ~auth:(auth profile) ?timeout ~allow_insecure
          transport profile.session_url
    | None ->
        Client.connect_env ~sw ~auth:(auth profile) ?timeout ~allow_insecure env
          profile.session_url
  in
  Result.map_error
    (fun error ->
      let error =
        match error with
        | Client.Transport (cause, message) ->
            Client.Transport
              (cause, Fmt.str "%s, connecting profile %s" message profile.name)
        | error -> error
      in
      Connection_error error)
    result

let connect_name ~sw ?store ?transport ?timeout ?allow_insecure env name =
  let ( let* ) = Result.bind in
  let* store =
    match store with Some store -> Ok store | None -> xdg_store env
  in
  let* profile = load store name in
  connect ~sw ?transport ?timeout ?allow_insecure env profile
