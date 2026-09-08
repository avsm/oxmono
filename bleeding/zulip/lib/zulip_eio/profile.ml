type t = { name : string; auth : Auth.t }

let ( let* ) = Result.bind

let valid_name name =
  let alnum = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
    | _ -> false
  in
  String.length name > 0
  && String.length name <= 64
  && alnum name.[0]
  && String.for_all (fun c -> alnum c || c = '-' || c = '_' || c = '.') name

let create ~name ~auth =
  if valid_name name then Ok { name; auth }
  else
    Error
      (Error.Invalid_request
         "Profile names must start with an ASCII letter/digit and contain at \
          most 64 letters, digits, dots, hyphens or underscores")

let name t = t.name
let auth t = t.auth

type fields = { site : string; email : string; api_key : string }

let codec =
  Jsont.Object.map ~kind:"Zulip profile" (fun site email api_key ->
      { site; email; api_key })
  |> Jsont.Object.mem "site" Jsont.string ~enc:(fun t -> t.site)
  |> Jsont.Object.mem "email" Jsont.string ~enc:(fun t -> t.email)
  |> Jsont.Object.mem "api_key" Jsont.string ~enc:(fun t -> t.api_key)
  |> Jsont.Object.finish

let fields auth =
  {
    site = Auth.site auth;
    email = Auth.email auth;
    api_key = Auth.api_key auth;
  }

let io f =
  try f () with
  | Eio.Io _ as exn -> Error (Error.Storage (Printexc.to_string exn))
  | Eio.Buf_read.Buffer_limit_exceeded ->
      Error (Error.Storage "Profile exceeds 64 KiB")
  | Xdge.Invalid_xdg_path message -> Error (Error.Storage message)
  | Error.E error -> Error error

let xdg ~fs =
  try Xdge.create fs "zulip"
  with Failure message -> raise (Error.E (Error.Storage message))

let private_directory path =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 path;
  let stat = Eio.Path.stat ~follow:false path in
  if stat.kind <> `Directory || stat.perm land 0o077 <> 0 then
    raise
      (Error.E
         (Error.Storage "Profile directory must be a private directory (0700)"))

let config_directory ~fs = Eio.Path.(Xdge.config_dir (xdg ~fs) / "profiles")
let profile_path ~fs name = Eio.Path.(config_directory ~fs / (name ^ ".json"))

let load ~fs name =
  if not (valid_name name) then
    Error (Error.Invalid_request "Invalid profile name")
  else
    io (fun () ->
        let directory = config_directory ~fs in
        let stat = Eio.Path.stat ~follow:false directory in
        if stat.kind <> `Directory || stat.perm land 0o077 <> 0 then
          Error (Error.Storage "Profile directory must have mode 0700")
        else
          Eio.Path.with_open_in (profile_path ~fs name) (fun flow ->
              let stat = Eio.File.stat flow in
              if stat.kind <> `Regular_file || stat.perm land 0o077 <> 0 then
                Error
                  (Error.Storage "Profile must be a regular private file (0600)")
              else
                let text =
                  Eio.Buf_read.(take_all (of_flow ~max_size:65537 flow))
                in
                if String.length text > 65536 then
                  Error (Error.Storage "Profile exceeds 64 KiB")
                else
                  let* fields =
                    Fetch.Json.decode_string' codec text
                    |> Result.map_error (fun e -> Error.Json e)
                  in
                  let* auth =
                    Auth.create ~site:fields.site ~email:fields.email
                      ~api_key:fields.api_key
                  in
                  create ~name ~auth))

let serial = Atomic.make 0

let save ~fs t =
  io (fun () ->
      let* text =
        Jsont_bytesrw.encode_string' ~format:Jsont.Indent codec (fields t.auth)
        |> Result.map_error (fun error -> Error.Json error)
      in
      if String.length text >= 65536 then
        Error (Error.Storage "Profile exceeds 64 KiB")
      else
        let directory = config_directory ~fs in
        let* () =
          match Error.catch (fun () -> private_directory directory) with
          | Ok () -> Ok ()
          | Error e -> Error e
        in
        let rec write attempts =
          if attempts = 0 then
            Error (Error.Storage "Unable to allocate profile temporary file")
          else
            let tmp =
              Eio.Path.(
                directory
                / Printf.sprintf ".profile-%d-%d.tmp" (Unix.getpid ())
                    (Atomic.fetch_and_add serial 1))
            in
            let created = ref false in
            Fun.protect
              ~finally:(fun () ->
                if !created then
                  Eio.Cancel.protect (fun () ->
                      Eio.Path.unlink ~missing_ok:true tmp))
              (fun () ->
                match
                  Eio.Path.with_open_out ~create:(`Exclusive 0o600) tmp
                    (fun flow ->
                      created := true;
                      Eio.Flow.copy_string (text ^ "\n") flow;
                      Eio.File.sync flow)
                with
                | () ->
                    Eio.Path.rename tmp (profile_path ~fs t.name);
                    created := false;
                    Ok ()
                | exception Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) ->
                    write (attempts - 1))
        in
        write 10)

let resolve ~fs ?site ?email ?api_key name =
  if not (valid_name name) then
    Error (Error.Invalid_request "Invalid profile name")
  else
    io (fun () ->
        let* stored =
          if Eio.Path.kind ~follow:true (profile_path ~fs name) = `Not_found
          then Ok None
          else Result.map Option.some (load ~fs name)
        in
        let choose explicit env get =
          match explicit with
          | Some value -> value
          | None -> (
              match Sys.getenv_opt env with
              | Some value -> value
              | None -> Option.fold ~none:"" ~some:(fun p -> get p.auth) stored)
        in
        let* auth =
          Auth.create
            ~site:(choose site "ZULIP_SITE" Auth.site)
            ~email:(choose email "ZULIP_EMAIL" Auth.email)
            ~api_key:(choose api_key "ZULIP_API_KEY" Auth.api_key)
        in
        create ~name ~auth)

let import_zuliprc ~fs ~name path =
  let* auth = Auth.load_zuliprc path in
  let* profile = create ~name ~auth in
  let* () = save ~fs profile in
  Ok profile

let data_dir ~fs t =
  io (fun () ->
      let directory =
        Eio.Path.(Xdge.data_dir (xdg ~fs) / "profiles" / t.name)
      in
      private_directory directory;
      Ok directory)

let pp ppf t = Format.fprintf ppf "Profile(%s, %a)" t.name Auth.pp t.auth
