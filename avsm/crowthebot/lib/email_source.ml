module R = Email_client

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

let validate s =
  ignore (Tool_config.endpoint ~allow_http:false s.url);
  (try ignore (Jmap_eio.Auth.bearer s.token)
   with Invalid_argument _ ->
     invalid_arg
       "Paste only the API token, without quotes, whitespace or a Bearer \
        prefix.");
  Option.iter
    (fun id ->
      match Jmap.Proto.Id.of_string_received id with
      | Ok _ -> ()
      | Error _ -> invalid_arg "Invalid mail account ID.")
    s.account;
  if s.max_bytes < 1048576 || s.max_bytes > 33554432 then
    invalid_arg "Email response limit must be 1 to 32 MiB."

let configuration ~writable =
  let open Cmdliner in
  Tool_config.v
    ~name:(if writable then "email-rw" else "email-ro")
    ~doc:
      (if writable then
         "Separate JMAP bearer connections for email label updates."
       else "Read-only JMAP bearer connections for email reads and queries.")
    Term.(
      const (fun url token_file account max_mb () ->
          if max_mb < 1 || max_mb > 32 then
            invalid_arg "Response limit must be 1 to 32 MiB.";
          let token =
            Tool_config.secret
              ~label:
                (if writable then "JMAP label-write API token (token only)"
                 else "Read-only JMAP API token (token only)")
              token_file
            |> String.trim
          in
          let s = { url; token; account; max_bytes = max_mb * 1048576 } in
          validate s;
          Tool_config.encode codec s)
      $ Arg.(
          value
          & opt string "https://api.fastmail.com/jmap/session"
          & info [ "url" ] ~docv:"SESSION_URL"
              ~doc:"HTTPS JMAP session URL. API must share its origin.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "token-file" ] ~docv:"FILE"
              ~doc:
                "Owned 0600 bearer-token file. Otherwise prompt without echo. \
                 Use a separate token for each access mode.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "account-id" ] ~docv:"ID"
              ~doc:
                "Mail account ID, defaulting to the session's primary mail \
                 account.")
      $ Arg.(
          value & opt int 32
          & info [ "max-response-mib" ]
              ~doc:"Maximum response size, 1 to 32 MiB."))

let read_configuration = configuration ~writable:false
let write_configuration = configuration ~writable:true

type reader = unit -> R.reader
type writer = unit -> R.writer

let initialize connect ~sw ~fetch ~clock json =
  let s = Tool_config.decode codec json in
  let s = { s with token = String.trim s.token } in
  validate s;
  let origin =
    Uri.of_string s.url |> fun u -> Uri.with_path u "/" |> Uri.to_string
  in
  let fetch = Fetch.restrict ~under:[ origin ] ~methods:[ `GET; `POST ] fetch in
  let cached = ref None and mutex = Eio.Mutex.create () in
  fun () ->
    Persistence.locked mutex (fun () ->
        match !cached with
        | Some client -> client
        | None ->
            let client =
              connect ~sw ~fetch ~clock ~token:s.token ?account_id:s.account
                ?max_body:(Some s.max_bytes) s.url
            in
            cached := Some client;
            client)

let initialize_reader ~sw ~fetch ~clock json =
  initialize R.connect_read_only ~sw ~fetch ~clock json

let initialize_writer ~sw ~fetch ~clock json =
  initialize R.connect_read_write ~sw ~fetch ~clock json

let reader t = t ()
let writer t = t ()
