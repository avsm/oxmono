open Cmdliner

type http_policy = {
  retries : int option;
  rate_limit : float option;
  max_concurrent : int option;
  connect_timeout : float option;
  idle_timeout : float option;
}

let http_policy_default =
  {
    retries = None;
    rate_limit = None;
    max_concurrent = None;
    connect_timeout = None;
    idle_timeout = None;
  }

type http_options = {
  retry : Fetch.Retry.config option;
  min_interval : Duration.t option;
  max_concurrent : int option;
  connect_timeout : Duration.t option;
  idle_timeout : Duration.t option;
}

let http_options ?homeserver (p : http_policy) =
  let duration name seconds =
    if not (Float.is_finite seconds && seconds > 0.) then
      invalid_arg
        ("Matrix_cli.http_options: " ^ name ^ " must be finite and positive");
    let value = Duration.of_f seconds in
    if value = 0L then
      invalid_arg
        ("Matrix_cli.http_options: " ^ name ^ " is below one nanosecond");
    value
  in
  let interval rate =
    if not (Float.is_finite rate && rate > 0.) then
      invalid_arg
        "Matrix_cli.http_options: rate_limit must be finite and positive";
    duration "min_interval" (1. /. rate)
  in
  let retry =
    match homeserver with
    | Some homeserver ->
        Some (Matrix_client.Http_retry.v ?max_retries:p.retries ~homeserver ())
    | None -> Option.map (fun n -> Fetch.Retry.v ~max_retries:n ()) p.retries
  in
  {
    retry;
    min_interval = Option.map interval p.rate_limit;
    max_concurrent = p.max_concurrent;
    connect_timeout = Option.map (duration "connect_timeout") p.connect_timeout;
    idle_timeout = Option.map (duration "idle_timeout") p.idle_timeout;
  }

let nonnegative_int =
  let parse s =
    match int_of_string_opt s with
    | Some n when n >= 0 -> Ok n
    | _ -> Error (`Msg "must be a non-negative integer")
  in
  (parse, Format.pp_print_int)

let positive_int =
  let parse s =
    match int_of_string_opt s with
    | Some n when n > 0 -> Ok n
    | _ -> Error (`Msg "must be a positive integer")
  in
  (parse, Format.pp_print_int)

let positive_float =
  let parse s =
    match float_of_string_opt s with
    | Some n when Float.is_finite n && n > 0. -> Ok n
    | _ -> Error (`Msg "must be a positive number")
  in
  (parse, Format.pp_print_float)

let http_policy_term =
  let opt conv flags doc docv =
    let c = Arg.conv ~docv conv in
    Term.(
      const (fun x -> x)
      $ Arg.(value & Arg.opt (some c) None & info flags ~docv ~doc))
  in
  let make retries rate_limit max_concurrent connect_timeout idle_timeout =
    { retries; rate_limit; max_concurrent; connect_timeout; idle_timeout }
  in
  Term.(
    const make
    $ opt nonnegative_int [ "retries" ]
        "Maximum retries (default: 3) for idempotent HTTP methods and the \
         replay-safe Matrix /keys/query POST. Other POST requests are never \
         replayed."
        "N"
    $ opt positive_float [ "rate-limit" ]
        "Maximum HTTP request rate in requests per second (positive)."
        "REQUESTS_PER_SECOND"
    $ opt positive_int [ "max-concurrent" ]
        "Maximum concurrent HTTP requests (positive)." "N"
    $ opt positive_float [ "connect-timeout" ]
        "Connection and TLS handshake timeout in seconds (positive)." "SECONDS"
    $ opt positive_float [ "idle-timeout" ]
        "Per-read/write idle timeout in seconds (positive)." "SECONDS")

let conv_of_id (type a) ~(of_string : string -> (a, [ `Msg of string ]) result)
    ~(to_string : a -> string) =
  let pp fmt id = Format.pp_print_string fmt (to_string id) in
  Arg.conv (of_string, pp)

let user_id_conv =
  conv_of_id ~of_string:Matrix_proto.Id.User_id.of_string
    ~to_string:Matrix_proto.Id.User_id.to_string

let room_id_conv =
  conv_of_id ~of_string:Matrix_proto.Id.Room_id.of_string
    ~to_string:Matrix_proto.Id.Room_id.to_string

let uri_conv =
  let pp fmt uri = Format.pp_print_string fmt (Uriz.to_string uri) in
  let parse value =
    match Uriz.of_string value with
    | This uri -> Ok uri
    | Null -> Error (`Msg "invalid URI reference")
  in
  Arg.conv (parse, pp)

let arg_pair value_conv ?env_var ~doc ~flags ~docv () =
  let env = Option.map (fun v -> Cmd.Env.info v ~doc) env_var in
  let arg = Arg.(opt (some value_conv) None & info flags ?env ~docv ~doc) in
  (Arg.(value & arg), Arg.(required & arg))

let homeserver_opt_term, homeserver_term =
  arg_pair uri_conv ()
    ~doc:
      "Matrix homeserver URL (e.g., https://matrix.org). Can also be set via \
       $(b,MATRIX_HOMESERVER)."
    ~env_var:"MATRIX_HOMESERVER" ~flags:[ "homeserver"; "s" ] ~docv:"URL"

let username_opt_term, username_term =
  arg_pair Arg.string ()
    ~doc:
      "Username (localpart or full @user:server format). Can also be set via \
       $(b,MATRIX_USERNAME)."
    ~env_var:"MATRIX_USERNAME" ~flags:[ "username"; "u" ] ~docv:"USER"

let profile_term =
  let doc =
    "Profile name for session storage. Profiles are stored in \
     $(b,\\$XDG_DATA_HOME/matrix/profiles/NAME/). Use different profiles for \
     multiple accounts."
  in
  Arg.(value & opt string "default" & info [ "profile"; "P" ] ~docv:"NAME" ~doc)

let room_opt_term, room_term =
  arg_pair room_id_conv () ~doc:"Room ID in !roomid:server format."
    ~flags:[ "room"; "r" ] ~docv:"ROOM_ID"

let recipient_opt_term, recipient_term =
  arg_pair user_id_conv ()
    ~doc:"Recipient user ID in @user:server format (for direct messages)."
    ~flags:[ "to"; "t" ] ~docv:"USER_ID"

let message_opt_term, message_term =
  let doc = "Message text to send." in
  let arg = Arg.(pos 0 (some string) None & info [] ~docv:"MESSAGE" ~doc) in
  (Arg.(value & arg), Arg.(required & arg))

let encrypted_term =
  let doc =
    "Enable end-to-end encryption for newly created rooms. Encryption requires \
     key management, and messages may not be decryptable without a key backup."
  in
  Arg.(value & flag & info [ "encrypted"; "e" ] ~doc)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let verbosity_term =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let password_env_var = "MATRIX_PASSWORD"

let password_file_term =
  let doc =
    Printf.sprintf
      "Read the password from $(docv). Without it the password is taken from \
       $(b,%s). There is no flag that takes the password itself, because a \
       command line is readable by every process on the machine."
      password_env_var
  in
  Arg.(
    value & opt (some file) None & info [ "password-file" ] ~docv:"FILE" ~doc)

(* A password file is written by hand, so the newline a text editor leaves
   behind is not part of the secret. *)
let read_password_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let contents = really_input_string ic (in_channel_length ic) in
      match String.length contents with
      | 0 -> contents
      | n when contents.[n - 1] = '\n' -> String.sub contents 0 (n - 1)
      | _ -> contents)

let password_opt_term =
  let pick = function
    | Some path -> Some (read_password_file path)
    | None -> Sys.getenv_opt password_env_var
  in
  Term.(const pick $ password_file_term)

let password_term =
  let require = function
    | Some password -> `Ok password
    | None ->
        `Error
          ( true,
            Printf.sprintf "no password: pass --password-file or set %s"
              password_env_var )
  in
  Term.(ret (const require $ password_opt_term))

type login_credentials = {
  homeserver : Uriz.t;
  username : string;
  password : string;
  profile : string;
}

let login_credentials_term =
  let make homeserver username password profile =
    { homeserver; username; password; profile }
  in
  Term.(
    const make $ homeserver_term $ username_term $ password_term $ profile_term)

let exit_ok = Cmd.Exit.ok
let exit_usage = Cmd.Exit.cli_error
let exit_auth = 77
let exit_network = 69
let exit_internal = 70
