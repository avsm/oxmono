(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
module C = Dooit.Common
module Fields = Tomlt.Fields

let field f = try f () with Fields.Invalid message -> C.fail "%s" message
let known keys table = field (fun () -> Fields.known keys table)
let text key table = field (fun () -> Fields.string_opt key table)
let required key table = field (fun () -> Fields.required_string key table)
let nonempty key value = field (fun () -> Fields.nonempty key value)

module T = Tomlt.Toml
module U = Fetch.Middleware.Url

let secret_reference ~base value =
  try
    let reference = Sekrit.Reference.of_string value in
    match Sekrit.Reference.source reference with
    | `Named _ -> Sekrit.Reference.to_string reference
    | `File path -> Xdge.Paths.expand ~base path
  with Sekrit.Error message -> C.fail "%s" message

let parse_url s =
  if String.contains s '?' || String.contains s '#' then
    C.fail "CardDAV URLs cannot contain queries or fragments";
  let u = C.get_ok (U.of_string s) in
  if U.scheme u <> `Https then C.fail "CardDAV requires HTTPS";
  u

type carddav = {
  url : string;
  collection : string option;
  username : string;
  password_file : string;
}

type t = {
  path : string;
  demo_root : string option;
  draft_root : string option;
  identity : string option;
  signature : string option;
  profile : string option;
  account : string option;
  service : string option;
  sortal_root : string option;
  vcard_root : string option;
  carddav : carddav option;
  dooit_config : string option;
  dooit_root : string option;
  capture_tags : string list;
}

let parse ~path raw =
  let table =
    match Tomlt_bytesrw.of_string raw with
    | Ok t -> t
    | Error _ -> C.fail "invalid TOML in %s" path
  in
  known [ "mail"; "contacts"; "dooit" ] table;
  let section key allowed =
    match T.find_opt key table with
    | None -> T.table []
    | Some t ->
        known allowed t;
        t
  in
  let mail =
    section "mail"
      [ "profile"; "account"; "service"; "identity"; "draft_root"; "signature" ]
  in
  let contacts =
    section "contacts" [ "sortal_root"; "vcard_root"; "carddav" ]
  in
  let tasks = section "dooit" [ "config"; "root"; "capture_tags" ] in
  let path_value key t =
    Option.map
      (fun p ->
        Xdge.Paths.expand ~base:(Filename.dirname path) (nonempty key p))
      (text key t)
  in
  let carddav =
    Option.map
      (fun t ->
        known [ "url"; "collection"; "username"; "password_file" ] t;
        let url = required "url" t in
        ignore (parse_url url);
        if String.contains url '?' then
          C.fail "CardDAV URL cannot contain a query";
        let collection = text "collection" t in
        Option.iter
          (fun c ->
            let module U = Fetch.Middleware.Url in
            if U.origin (parse_url c) <> U.origin (parse_url url) then
              C.fail "CardDAV collection must use the configured origin")
          collection;
        {
          url;
          collection;
          username = required "username" t;
          password_file =
            secret_reference ~base:(Filename.dirname path)
              (required "password_file" t);
        })
      (T.find_opt "carddav" contacts)
  in
  let capture_tags =
    match T.find_opt "capture_tags" tasks with
    | None -> [ "inbox" ]
    | Some v -> (
        match T.to_array_opt v with
        | None -> C.fail "dooit.capture_tags must be an array of strings"
        | Some xs ->
            List.map
              (fun v ->
                match T.to_string_opt v with
                | Some s when s <> "" -> s
                | _ -> C.fail "dooit.capture_tags must contain nonempty strings")
              xs)
  in
  if
    List.length capture_tags
    <> List.length (List.sort_uniq String.compare capture_tags)
  then C.fail "duplicate capture tag";
  {
    path;
    demo_root = None;
    draft_root = path_value "draft_root" mail;
    identity = text "identity" mail;
    signature = text "signature" mail;
    profile = text "profile" mail;
    account = text "account" mail;
    service = Option.map Dooit.Link.service (text "service" mail);
    sortal_root = path_value "sortal_root" contacts;
    vcard_root = path_value "vcard_root" contacts;
    carddav;
    dooit_config = path_value "config" tasks;
    dooit_root = path_value "root" tasks;
    capture_tags;
  }

let config_path ?path () =
  match path with
  | Some path -> Xdge.Paths.expand ~base:(Sys.getcwd ()) path
  | None -> Filename.concat (Xdge.Paths.config_dir "termanil") "config.toml"

let load ?path () =
  let explicit = Option.is_some path in
  let path = config_path ?path () in
  if C.exists path then parse ~path (C.read ~limit:65536 path)
  else if explicit then C.fail "config does not exist: %s" path
  else parse ~path ""

let example =
  {|# Credentials use the shared JMAP profile store. See termanil/README.md.
[mail]
profile = "personal"
# account = "account-id"  # defaults to the primary mail account
# service = "https://mail.example.net/jmap/session"
# identity = "identity-id"  # required when more than one identity exists
# signature = "-- \nAnil"  # override JMAP identity signature; empty disables it
# draft_root = "~/bushel/replies"  # default: XDG data directory / termanil/replies

[contacts]
vcard_root = "~/bushel/sortal-carddav"

# Uncomment to read CardDAV alongside Sortal.
# [contacts.carddav]
# url = "https://carddav.fastmail.com/"
# username = "your-login"
# password_file = "carddav.password"  # relative to this file, mode 0600
# collection = "https://carddav.fastmail.com/your/addressbook/"

[dooit]
# Uses Dooit's XDG config by default, including its WebDAV subdirectory.
# config = "~/.config/dooit/config.toml"
# root = "~/bushel/dooit"
capture_tags = ["inbox"]
|}

let init ?path () =
  let path = config_path ?path () in
  C.mkdir (Filename.dirname path);
  C.create_file path example;
  path

let data_path name = Filename.concat (Xdge.Paths.data_dir "termanil") name

let replies t =
  match t.draft_root with Some root -> root | None -> data_path "replies"

let demo ?root () =
  let root =
    match root with
    | Some p -> Xdge.Paths.expand ~base:(Sys.getcwd ()) p
    | None -> data_path "demo"
  in
  let rec canonical p =
    if C.exists p then Unix.realpath p
    else Filename.concat (canonical (Filename.dirname p)) (Filename.basename p)
  in
  let root = canonical root in
  {
    (parse ~path:(Filename.concat root "config.toml") "") with
    demo_root = Some root;
    draft_root = Some (Filename.concat root "replies");
    dooit_root = Some (Filename.concat root "tasks");
    identity = Some "demo";
  }
