(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The memo of a thunk is written even though a thunk is never answered from
   it, so that {!pp} can report the last value it produced without calling
   it. *)
type source =
  | Const of string
  | File of string * (unit -> string)
  | Thunk of (unit -> string)

type secret = {
  source : source;
  cache : string option ref;
  mutable generation : int;
}

type t =
  | Anonymous
  | Bearer of secret
  | Basic of secret (* The decoded "user:password" of RFC 7617 §2. *)

let secret source = { source; cache = ref None; generation = 0 }

let get s =
  match (s.source, !(s.cache)) with
  | Const v, _ -> v
  | File _, Some v -> v
  | File (_, read), None | Thunk read, _ ->
      let generation = s.generation in
      let v = read () in
      (* A {!refresh} while the read was suspended has already dropped the
         cache, and the value read before it is the stale one. *)
      if generation = s.generation then s.cache := Some v;
      v

let reset s =
  match s.source with
  | File _ ->
      s.generation <- s.generation + 1;
      s.cache := None
  | _ -> ()

let denied fmt = Fmt.kstr (fun msg -> raise (Fetch.err (Fetch.Denied msg))) fmt
let diagnostic = Httpz_media.sanitize_diagnostic

let read_secret_file ?fs path =
  let subject = Fmt.str "the credential file %s" (diagnostic path) in
  try
    Ok
      (match fs with
      | Some fs -> Sekrit_eio.read_reference ~fs path
      | None -> Sekrit.read_reference path)
  with
  | Sekrit.Error message -> Error (Fmt.str "%s: %s" subject message)
  | Eio.Io _ as exn ->
      Error
        (Error_context.describe ~operation:(Fmt.str "reading %s" subject) exn)

let read_file ?fs path () =
  match read_secret_file ?fs path with
  | Ok secret -> secret
  | Error msg -> denied "%s" msg

let none = Anonymous

let valid_bearer ~name token =
  match Fetch.Credential.bearer token with
  | _ -> token
  | exception Invalid_argument _ ->
      denied "%s is not an RFC 6750 bearer token" name

let basic_credential ~user ~password =
  let credential = Fetch.Credential.basic ~user ~password in
  let printable = String.for_all (fun c -> c >= '\x20' && c <= '\x7e') in
  if not (printable user && printable password) then
    invalid_arg "Jmap_eio.Auth.basic: user and password must be printable ASCII";
  credential

let split_basic s =
  match String.index_opt s ':' with
  | None -> (s, "")
  | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

let valid_basic ~name value =
  if not (String.contains value ':') then
    denied "%s must contain a user and password separated by ':'" name
  else
    let user, password = split_basic value in
    match basic_credential ~user ~password with
    | _ -> value
    | exception Invalid_argument _ ->
        denied "%s is not a valid Basic credential" name

let bearer token =
  ignore (Fetch.Credential.bearer token);
  Bearer (secret (Const token))

let basic ~user ~password =
  ignore (basic_credential ~user ~password);
  Basic (secret (Const (user ^ ":" ^ password)))

let bearer_from_file ?fs path =
  let read () =
    valid_bearer
      ~name:(Fmt.str "the credential in %s" (diagnostic path))
      (read_file ?fs path ())
  in
  Bearer (secret (File (path, read)))

let basic_from_file ?fs path =
  let read () =
    valid_basic
      ~name:(Fmt.str "the basic credential in %s" (diagnostic path))
      (read_file ?fs path ())
  in
  Basic (secret (File (path, read)))

let refreshing ~refresh =
  let read () =
    let token =
      Error_context.with_io ~operation:"refreshing the JMAP bearer credential"
        refresh
    in
    valid_bearer ~name:"the refreshed credential" token
  in
  Bearer (secret (Thunk read))

let refresh = function Anonymous -> () | Bearer s | Basic s -> reset s

let to_credentials = function
  | Anonymous -> []
  | Bearer s -> [ Fetch.Credential.Bearer (fun () -> get s) ]
  | Basic s -> [ Fetch.Credential.Basic (fun () -> split_basic (get s)) ]

let redact _ = "***"

let pp_secret ~scheme ~show ppf s =
  match (s.source, !(s.cache)) with
  | Const v, _ -> Fmt.pf ppf "%s %s" scheme (show v)
  | File (path, _), _ -> Fmt.pf ppf "%s <file:%s>" scheme (diagnostic path)
  | Thunk _, Some v -> Fmt.pf ppf "%s %s" scheme (show v)
  | Thunk _, None -> Fmt.pf ppf "%s <refreshing>" scheme

let pp ppf = function
  | Anonymous -> Format.pp_print_string ppf "none"
  | Bearer s -> pp_secret ~scheme:"bearer" ~show:redact ppf s
  | Basic s ->
      let show v =
        let user, password = split_basic v in
        user ^ ":" ^ redact password
      in
      pp_secret ~scheme:"basic" ~show ppf s

type scheme = Bearer | Basic

let scheme_to_string = function Bearer -> "bearer" | Basic -> "basic"

let scheme_of_string ~name = function
  | "bearer" -> Ok Bearer
  | "basic" -> Ok Basic
  | other ->
      Error (Fmt.str "%s: expected \"bearer\" or \"basic\", got %S" name other)

let of_scheme ?fs ~key_name ~auth_name scheme = function
  | `File path ->
      Ok
        (match scheme with
        | Bearer -> bearer_from_file ?fs path
        | Basic -> basic_from_file ?fs path)
  | `Key key -> (
      match scheme with
      | Bearer -> (
          try Ok (bearer key)
          with Invalid_argument _ ->
            Error
              (Fmt.str "%s: with %s the key is not a valid Bearer token"
                 key_name auth_name))
      | Basic when String.contains key ':' -> (
          let user, password = split_basic key in
          try Ok (basic ~user ~password)
          with Invalid_argument _ ->
            Error
              (Fmt.str "%s: with %s the key is not a valid Basic credential"
                 key_name auth_name))
      | Basic ->
          Error
            (Fmt.str "%s: with %s the key must be \"user:password\"" key_name
               auth_name))

let getenv name =
  match Sys.getenv_opt name with Some v when v <> "" -> Some v | _ -> None

let of_env ?(prefix = "JMAP") ?fs () =
  let ( let* ) = Result.bind in
  let var suffix = getenv (prefix ^ "_" ^ suffix) in
  let* scheme =
    match var "AUTH" with
    | None -> Ok Bearer
    | Some s -> scheme_of_string ~name:(prefix ^ "_AUTH") s
  in
  let of_scheme =
    of_scheme ?fs ~key_name:(prefix ^ "_API_KEY")
      ~auth_name:(prefix ^ "_AUTH=" ^ scheme_to_string scheme)
      scheme
  in
  match (var "API_KEY_FILE", var "API_KEY") with
  | None, None -> Ok None
  | Some path, _ -> Result.map Option.some (of_scheme (`File path))
  | None, Some key -> Result.map Option.some (of_scheme (`Key key))
