type t = {
  site : string;
  email : string;
  api_key : string;
  credential : Fetch.Credential.t;
}

module Url = Fetch.Middleware.Url

let create ~site ~email ~api_key =
  let site = String.trim site in
  let has_scheme =
    match String.index_opt site ':' with
    | Some i ->
        String.length site > i + 2 && site.[i + 1] = '/' && site.[i + 2] = '/'
    | None -> false
  in
  let site = if has_scheme then site else "https://" ^ site in
  match Url.of_string site with
  | Error reason ->
      Error (Error.Invalid_request ("Invalid Zulip site: " ^ reason))
  | Ok url when Url.has_query url || Url.has_fragment url ->
      Error
        (Error.Invalid_request "Zulip site must not contain a query or fragment")
  | Ok url -> (
      if email = "" || api_key = "" then
        Error (Error.Invalid_request "Email and API key are required")
      else
        try
          let credential =
            Fetch.Credential.basic ~user:email ~password:api_key
          in
          let site = Url.to_string url in
          let rec trim_end n =
            if n > 0 && site.[n - 1] = '/' then trim_end (n - 1) else n
          in
          Ok
            {
              site = String.sub site 0 (trim_end (String.length site));
              email;
              api_key;
              credential;
            }
        with Invalid_argument reason -> Error (Error.Invalid_request reason))

let site t = t.site
let email t = t.email
let api_key t = t.api_key
let credential t = t.credential
let pp ppf t = Format.fprintf ppf "Zulip(%s, %s)" t.site t.email

let of_zuliprc text =
  if String.length text > 65536 then
    Error (Error.Invalid_request "zuliprc exceeds 64 KiB")
  else
    let fields = Hashtbl.create 3 in
    let active = ref false in
    let invalid = ref None in
    String.split_on_char '\n' text
    |> List.iteri (fun index line ->
        let line = String.trim line in
        if line = "" || line.[0] = '#' || line.[0] = ';' then ()
        else if line.[0] = '[' then active := line = "[api]"
        else if !active then
          match String.index_opt line '=' with
          | None ->
              invalid :=
                Some (Printf.sprintf "Invalid zuliprc line %d" (index + 1))
          | Some split ->
              let key = String.trim (String.sub line 0 split) in
              let value =
                String.trim
                  (String.sub line (split + 1) (String.length line - split - 1))
              in
              if List.mem key [ "site"; "email"; "key" ] then
                if Hashtbl.mem fields key then
                  invalid := Some ("Duplicate zuliprc key: " ^ key)
                else Hashtbl.add fields key value);
    match !invalid with
    | Some reason -> Error (Error.Invalid_request reason)
    | None ->
        let field name =
          Option.value (Hashtbl.find_opt fields name) ~default:""
        in
        if field "site" = "" then
          Error (Error.Invalid_request "Missing site in zuliprc [api] section")
        else
          create ~site:(field "site") ~email:(field "email")
            ~api_key:(field "key")

let load_zuliprc path =
  try
    Eio.Path.with_open_in path (fun flow ->
        let text = Eio.Buf_read.(take_all (of_flow ~max_size:65537 flow)) in
        if String.length text > 65536 then
          Error (Error.Storage "zuliprc exceeds 64 KiB")
        else of_zuliprc text)
  with
  | Eio.Io _ as exn -> Error (Error.Storage (Printexc.to_string exn))
  | Eio.Buf_read.Buffer_limit_exceeded ->
      Error (Error.Storage "zuliprc exceeds 64 KiB")
