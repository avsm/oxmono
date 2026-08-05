(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Authentication method for Immich. *)
type auth_method =
  | Jwt of { access_token : string; user_id : string; email : string }
  | Api_key of { key : string; name : string option }

type t = {
  server_url : string;
  auth : auth_method;
  created_at : string;
}

let auth_method_jsont =
  let jwt_jsont =
    Jsont.Object.map ~kind:"Jwt"
      (fun access_token user_id email ->
        Jwt { access_token; user_id; email })
    |> Jsont.Object.mem "access_token" Jsont.string ~enc:(function
         | Jwt { access_token; _ } -> access_token
         | Api_key _ -> "")
    |> Jsont.Object.mem "user_id" Jsont.string ~enc:(function
         | Jwt { user_id; _ } -> user_id
         | Api_key _ -> "")
    |> Jsont.Object.mem "email" Jsont.string ~enc:(function
         | Jwt { email; _ } -> email
         | Api_key _ -> "")
    |> Jsont.Object.finish
  in
  let api_key_jsont =
    Jsont.Object.map ~kind:"ApiKey"
      (fun key name -> Api_key { key; name })
    |> Jsont.Object.mem "key" Jsont.string ~enc:(function
         | Api_key { key; _ } -> key
         | Jwt _ -> "")
    |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(function
         | Api_key { name; _ } -> name
         | Jwt _ -> None)
    |> Jsont.Object.finish
  in
  Jsont.Object.map ~kind:"AuthMethod"
    (fun type_ jwt api_key ->
      match type_ with
      | "jwt" -> Option.get jwt
      | "api_key" -> Option.get api_key
      | _ -> failwith ("Unknown auth type: " ^ type_))
  |> Jsont.Object.mem "type" Jsont.string ~enc:(function
       | Jwt _ -> "jwt"
       | Api_key _ -> "api_key")
  |> Jsont.Object.opt_mem "jwt" jwt_jsont ~enc:(function
       | Jwt _ as j -> Some j
       | Api_key _ -> None)
  |> Jsont.Object.opt_mem "api_key" api_key_jsont ~enc:(function
       | Api_key _ as a -> Some a
       | Jwt _ -> None)
  |> Jsont.Object.finish

let jsont =
  Jsont.Object.map ~kind:"Session"
    (fun server_url auth created_at -> { server_url; auth; created_at })
  |> Jsont.Object.mem "server_url" Jsont.string ~enc:(fun s -> s.server_url)
  |> Jsont.Object.mem "auth" auth_method_jsont ~enc:(fun s -> s.auth)
  |> Jsont.Object.mem "created_at" Jsont.string ~enc:(fun s -> s.created_at)
  |> Jsont.Object.finish

(* App config stores the current profile *)
type app_config = { current_profile : string }

let app_config_jsont =
  Jsont.Object.map ~kind:"AppConfig" (fun current_profile ->
      { current_profile })
  |> Jsont.Object.mem "current_profile" Jsont.string ~enc:(fun c ->
      c.current_profile)
  |> Jsont.Object.finish

let default_profile = "default"
let app_name = "immich"

(* Base config directory for the app *)
let base_config_dir fs =
  let home = Sys.getenv "HOME" in
  let config_path = Eio.Path.(fs / home / ".config" / app_name) in
  (try Eio.Path.mkdir ~perm:0o700 config_path
   with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ());
  config_path

(* Profiles directory *)
let profiles_dir fs =
  let base = base_config_dir fs in
  let profiles = Eio.Path.(base / "profiles") in
  (try Eio.Path.mkdir ~perm:0o700 profiles
   with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ());
  profiles

(* Config directory for a specific profile *)
let config_dir fs ?profile () =
  let profile_name = Option.value ~default:default_profile profile in
  let profiles = profiles_dir fs in
  let profile_dir = Eio.Path.(profiles / profile_name) in
  (try Eio.Path.mkdir ~perm:0o700 profile_dir
   with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ());
  profile_dir

(* App config file (stores current profile) *)
let app_config_file fs =
  Eio.Path.(base_config_dir fs / "config.json")

let load_app_config fs =
  let path = app_config_file fs in
  try
    Eio.Path.load path
    |> Jsont_bytesrw.decode_string app_config_jsont
    |> Result.to_option
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

let save_app_config fs config =
  let path = app_config_file fs in
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent app_config_jsont config
  with
  | Ok content -> Eio.Path.save ~create:(`Or_truncate 0o600) path content
  | Error e -> failwith ("Failed to encode app config: " ^ e)

(* Get the current profile name *)
let get_current_profile fs =
  match load_app_config fs with
  | Some config -> config.current_profile
  | None -> default_profile

(* Set the current profile *)
let set_current_profile fs profile =
  save_app_config fs { current_profile = profile }

(* List all available profiles *)
let list_profiles fs =
  let profiles = profiles_dir fs in
  try
    Eio.Path.read_dir profiles
    |> List.filter (fun name ->
        (* Check if it's a directory with a session.json *)
        let dir = Eio.Path.(profiles / name) in
        let session = Eio.Path.(dir / "session.json") in
        try
          ignore (Eio.Path.load session);
          true
        with _ -> false)
    |> List.sort String.compare
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> []

(* Session file within a profile directory *)
let session_file fs ?profile () =
  Eio.Path.(config_dir fs ?profile () / "session.json")

let load fs ?profile () =
  let profile =
    match profile with
    | Some p -> Some p
    | None ->
        (* Use current profile if none specified *)
        let current = get_current_profile fs in
        Some current
  in
  let path = session_file fs ?profile () in
  try
    Eio.Path.load path |> Jsont_bytesrw.decode_string jsont |> Result.to_option
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

let save fs ?profile session =
  let profile =
    match profile with
    | Some p -> Some p
    | None -> Some (get_current_profile fs)
  in
  let path = session_file fs ?profile () in
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont session with
  | Ok content -> Eio.Path.save ~create:(`Or_truncate 0o600) path content
  | Error e -> failwith ("Failed to encode session: " ^ e)

let clear fs ?profile () =
  let profile =
    match profile with
    | Some p -> Some p
    | None -> Some (get_current_profile fs)
  in
  let path = session_file fs ?profile () in
  try Eio.Path.unlink path
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()

(* JWT payload type for expiration check *)
type jwt_payload = { exp : float option }

let jwt_payload_jsont =
  Jsont.Object.map ~kind:"JwtPayload" (fun exp -> { exp })
  |> Jsont.Object.opt_mem "exp" Jsont.number ~enc:(fun p -> p.exp)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

(* JWT expiration check using base64 decoding *)
let is_jwt_expired ?(leeway = 60) token =
  try
    let parts = String.split_on_char '.' token in
    if List.length parts < 2 then true
    else begin
      let payload = List.nth parts 1 in
      (* Add padding if needed *)
      let padding = String.length payload mod 4 in
      let padded =
        if padding > 0 then payload ^ String.make (4 - padding) '='
        else payload
      in
      let decoded = Base64.decode_exn ~alphabet:Base64.uri_safe_alphabet padded in
      (* Parse the JSON payload to find exp *)
      match Jsont_bytesrw.decode_string jwt_payload_jsont decoded with
      | Ok { exp = Some exp_time } ->
          let now = Ptime.to_float_s (Ptime_clock.now ()) in
          now >= exp_time -. (Float.of_int leeway)
      | Ok { exp = None } -> true
      | Error _ -> true
    end
  with _ -> true

let is_expired ?leeway session =
  match session.auth with
  | Jwt { access_token; _ } -> is_jwt_expired ?leeway access_token
  | Api_key _ -> false  (* API keys don't expire *)

(* Styled output helpers *)
let label_style = Fmt.(styled `Faint string)
let value_style = Fmt.(styled (`Fg `Cyan) string)
let auth_type_style = Fmt.(styled (`Fg `Green) string)

let pp ppf session =
  match session.auth with
  | Jwt { user_id; email; _ } ->
      Fmt.pf ppf "@[<v>%a %a@,%a %a@,%a %a@,%a %a@,%a %a@]"
        label_style "Email:" value_style email
        label_style "User ID:" value_style user_id
        label_style "Server:" value_style session.server_url
        label_style "Created:" value_style session.created_at
        label_style "Auth:" auth_type_style "JWT"
  | Api_key { name; _ } ->
      let name_str = Option.value ~default:"<unnamed>" name in
      Fmt.pf ppf "@[<v>%a %a@,%a %a@,%a %a@,%a %a@]"
        label_style "API Key:" value_style name_str
        label_style "Server:" value_style session.server_url
        label_style "Created:" value_style session.created_at
        label_style "Auth:" auth_type_style "API Key"

let server_url t = t.server_url
let auth t = t.auth
let created_at t = t.created_at

let create ~server_url ~auth () =
  { server_url; auth; created_at = Ptime.to_rfc3339 (Ptime_clock.now ()) }
