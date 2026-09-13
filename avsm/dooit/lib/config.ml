(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common
module T = Tomlt.Toml
module Url = Fetch.Middleware.Url

type secret = File of string | Inline of string

type webdav = {
  url : string;
  subdir : string;
  collection : string;
  username : string;
  secret : secret;
}

type t = {
  path : string;
  root : string;
  webdav : webdav option;
  jmap_profile : string option;
  jmap_account : string option;
  jmap_service : string option;
}

let expand ~base p =
  let p =
    if String.starts_with ~prefix:"~/" p then
      Filename.concat
        (Option.value (Sys.getenv_opt "HOME")
           ~default:(Unix.getpwuid (Unix.getuid ())).pw_dir)
        (String.sub p 2 (String.length p - 2))
    else p
  in
  if Filename.is_relative p then Filename.concat base p else p

let nonempty key s =
  if String.trim s = "" then fail "%s must not be empty" key;
  s

let text key table =
  match T.find_opt key table with
  | None -> None
  | Some v -> (
      match T.to_string_opt v with
      | Some s -> Some s
      | None -> fail "config %s must be a string" key)

let required k t =
  match text k t with
  | Some s -> nonempty k s
  | None -> fail "missing config %s" k

let known allowed t =
  if not (T.is_table t) then fail "expected a TOML table";
  List.iter
    (fun k -> if not (List.mem k allowed) then fail "unknown config key: %s" k)
    (T.keys t)

let collection ~url ~subdir =
  if String.contains url '?' || String.contains url '#' then
    fail "WebDAV URL cannot contain a query or fragment";
  let u = get_ok (Url.of_string url) in
  if Url.scheme u <> `Https then fail "WebDAV requires HTTPS";
  let base = Url.to_string u in
  if not (String.ends_with ~suffix:"/" base) then
    fail "WebDAV base URL must end in /";
  let parts = String.split_on_char '/' subdir in
  if
    List.exists
      (fun s ->
        s = "" || s = "." || s = ".." || String.contains s '\\'
        || String.exists (fun c -> Char.code c < 32 || Char.code c = 127) s)
      parts
  then
    fail "webdav.subdir must be a nonempty relative path without dot segments";
  let percent s =
    let b = Buffer.create 30 in
    String.iter
      (fun c ->
        match c with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' | '~' ->
            Buffer.add_char b c
        | _ -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
      s;
    Buffer.contents b
  in
  base ^ String.concat "/" (List.map percent parts) ^ "/"

let parse ~path ~default_root raw =
  (* Parser errors can quote the input, which may contain a password. *)
  let t =
    match Tomlt_bytesrw.of_string raw with
    | Ok t -> t
    | Error _ -> fail "invalid TOML in %s" path
  in
  known [ "root"; "webdav"; "jmap" ] t;
  let base = Filename.dirname path in
  let root =
    match text "root" t with
    | None -> default_root
    | Some p -> expand ~base (nonempty "root" p)
  in
  let webdav =
    Option.map
      (fun w ->
        known [ "url"; "subdir"; "username"; "password_file"; "password" ] w;
        let url = required "url" w
        and subdir = required "subdir" w
        and username = required "username" w in
        let secret =
          match (text "password_file" w, text "password" w) with
          | Some p, None -> File (expand ~base (nonempty "password_file" p))
          | None, Some s -> Inline (nonempty "password" s)
          | _ ->
              fail
                "configure exactly one of webdav.password_file or \
                 webdav.password"
        in
        { url; subdir; collection = collection ~url ~subdir; username; secret })
      (T.find_opt "webdav" t)
  in
  let j = T.find_opt "jmap" t in
  Option.iter (known [ "profile"; "account"; "service" ]) j;
  let jf k = Option.bind j (text k) in
  {
    path;
    root;
    webdav;
    jmap_profile = jf "profile";
    jmap_account = jf "account";
    jmap_service = jf "service";
  }

let private_file p =
  regular p;
  let st = Unix.stat p in
  if st.st_uid <> Unix.getuid () || st.st_perm land 0o077 <> 0 then
    fail "credential file must be owned by you and mode 0600: %s" p

let load ~fs ?path ?root () =
  let explicit = Option.is_some path in
  let xdg = Xdge.create ~create_dirs:false fs "dooit" in
  let path =
    Option.value path
      ~default:Eio.Path.(native_exn (Xdge.config_dir xdg / "config.toml"))
  in
  let default_root = Eio.Path.native_exn (Xdge.data_dir xdg) in
  let c =
    if exists path then parse ~path ~default_root (read ~limit:65536 path)
    else if explicit then fail "config file does not exist: %s" path
    else
      {
        path;
        root = default_root;
        webdav = None;
        jmap_profile = None;
        jmap_account = None;
        jmap_service = None;
      }
  in
  (match c.webdav with
  | Some { secret = Inline _; _ } -> private_file path
  | _ -> ());
  match root with
  | None -> c
  | Some p -> { c with root = expand ~base:(Sys.getcwd ()) p }

let password w =
  let s =
    match w.secret with
    | Inline s -> s
    | File p ->
        private_file p;
        let s = read ~limit:65536 p in
        let s =
          if String.ends_with ~suffix:"\n" s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        if String.ends_with ~suffix:"\r" s then
          String.sub s 0 (String.length s - 1)
        else s
  in
  if s = "" || String.exists (fun c -> Char.code c < 32 || Char.code c > 126) s
  then fail "app password must be one nonempty line of printable ASCII";
  s

let require_webdav c =
  match c.webdav with
  | Some w -> w
  | None -> fail "configure [webdav] in %s" c.path
