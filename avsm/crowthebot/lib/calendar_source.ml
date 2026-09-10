module Remote = Jmap_eio.Calendars

type settings = {
  url : string;
  token : string;
  account : string option;
  max_bytes : int;
}

let codec =
  Jsont.Object.map (fun url token account max_bytes ->
      { url; token; account; max_bytes })
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun s -> s.url)
  |> Jsont.Object.mem "token" Jsont.string ~enc:(fun s -> s.token)
  |> Jsont.Object.mem "account" (Jsont.option Jsont.string) ~enc:(fun s ->
      s.account)
  |> Jsont.Object.mem "max_bytes" Jsont.int ~enc:(fun s -> s.max_bytes)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let token_input value =
  let token = String.trim value in
  try
    ignore (Jmap_eio.Auth.bearer token);
    token
  with Invalid_argument _ ->
    let message =
      if token = "" then "Paste the API token itself. The input is empty."
      else if
        String.starts_with ~prefix:"bearer " (String.lowercase_ascii token)
      then "Paste only the API token, without the Bearer prefix."
      else if token.[0] = '\'' || token.[0] = '"' then
        "Paste the API token without surrounding quotes."
      else if String.contains token '\027' then
        "The pasted token contains terminal escape characters. Use a 0600 \
         token file with --token-file instead."
      else if
        String.exists (fun c -> Char.code c <= 32 || Char.code c >= 127) token
      then
        "The token contains internal whitespace or invisible characters. Copy \
         only the API token from your provider's token field."
      else
        "Invalid API token text. Paste the token itself, without quotes or an \
         Authorization header. It does not need base64 encoding."
    in
    invalid_arg message

let validate s =
  ignore (Tool_config.endpoint ~allow_http:false s.url);
  ignore (token_input s.token);
  Option.iter
    (fun value ->
      match Jmap.Proto.Id.of_string_received value with
      | Ok _ -> ()
      | Error _ -> invalid_arg "Invalid calendar account ID.")
    s.account;
  if s.max_bytes < 1024 * 1024 || s.max_bytes > 256 * 1024 * 1024 then
    invalid_arg "Calendar response limit must be between 1 and 256 MiB."

let configuration =
  let open Cmdliner in
  Tool_config.v ~name:"calendar"
    ~doc:"Named read-only JMAP calendar connections."
    Term.(
      const (fun url token_file account max_mb () ->
          if max_mb < 1 || max_mb > 256 then
            invalid_arg "Response limit must be 1 to 256 MiB.";
          let token =
            Tool_config.secret ~label:"Read-only JMAP API token (token only)"
              token_file
            |> token_input
          in
          let s = { url; token; account; max_bytes = max_mb * 1024 * 1024 } in
          validate s;
          Tool_config.encode codec s)
      $ Arg.(
          required
          & opt (some string) None
          & info [ "url" ] ~docv:"SESSION_URL"
              ~doc:
                "HTTPS JMAP session/discovery URL. Advertised endpoints must \
                 share its origin.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "token-file" ] ~docv:"FILE"
              ~doc:
                "0600 file containing a token restricted to calendar reads. \
                 Otherwise prompt without echo.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "account-id" ] ~docv:"ID"
              ~doc:
                "Calendar account ID. Defaults to the session's calendar \
                 primary account.")
      $ Arg.(
          value & opt int 32
          & info [ "max-response-mib" ]
              ~doc:
                "Maximum JSON response or attachment size, 1 to 256 MiB. \
                 Default 32."))

type identity = {
  account : string;
  username : string;
  key : string;
  page_size : int;
}

type t = { connect : unit -> Remote.t; key : string -> string -> string }

let initialize ~sw ~fetch ~clock json =
  let s = Tool_config.decode codec json in
  let s = { s with token = token_input s.token } in
  validate s;
  let uri = Uri.of_string s.url in
  let origin =
    Uri.with_path uri "/" |> fun u ->
    Uri.with_query u [] |> fun u -> Uri.with_fragment u None |> Uri.to_string
  in
  let fetch = Fetch.restrict ~under:[ origin ] ~methods:[ `GET; `POST ] fetch in
  let cached = ref None in
  let mutex = Eio.Mutex.create () in
  let connect () =
    Persistence.locked mutex @@ fun () ->
    match !cached with
    | Some client -> client
    | None ->
        let transport = Jmap_eio.Transport.of_fetch ~clock fetch in
        let client =
          Jmap_eio.Client.connect ~sw
            ~auth:(Jmap_eio.Auth.bearer s.token)
            ~timeout:20. ~max_body:s.max_bytes transport s.url
          |> function
          | Ok client -> client
          | Error error -> raise (Jmap_eio.Client.Jmap_client_error error)
        in
        let client = Remote.create ?account_id:s.account client in
        ignore (Remote.identity client);
        cached := Some client;
        client
  in
  let key account username =
    Digestif.SHA256.(
      to_hex (digest_string (s.url ^ "\000" ^ account ^ "\000" ^ username)))
  in
  { connect; key }

let identity t =
  let id = Remote.identity (t.connect ()) in
  {
    account = id.account;
    username = id.username;
    key = t.key id.account id.username;
    page_size = id.page_size;
  }

let download t ~blob = Remote.download (t.connect ()) ~blob
let mirror_source t kind = Remote.mirror_source (t.connect ()) kind
