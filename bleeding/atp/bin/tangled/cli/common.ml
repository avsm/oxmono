(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api

let create_api ~sw ~env ~app_name ~pds () =
  let http = Xrpc.Trace.wrap (Fetch_httpz.std env) in
  let plc = Sys.getenv_opt "TANGLED_PLC" in
  Api.create ~sw ~env ~app_name ?plc ~pds ~http ()

let with_api ?(authenticated = true) ?busy env f =
  let f =
    match busy with
    | None -> f
    | Some l -> fun api -> Ui.busy l (fun () -> f api)
  in
  Eio.Switch.run @@ fun sw ->
  let session = Xrpc_auth.Session.load env#fs ~app_name:"tangled" () in
  let pds =
    match (Sys.getenv_opt "TANGLED_PDS", session) with
    | Some pds, _ -> pds
    | None, Some session -> session.pds
    | None, None -> "https://bsky.social"
  in
  let api = create_api ~sw ~env ~app_name:"tangled" ~pds () in
  Option.iter
    (fun session ->
      if
        authenticated
        && Xrpc.Client.normalize_service session.Xrpc_auth.Session.pds
           = Xrpc.Client.normalize_service pds
      then Api.resume api ~session)
    session;
  f api

(* [cmd info term] is a command whose output options, from
   {!Xrpc_auth.Cli.term}, take effect before [term] runs. *)
let cmd info term =
  Cmd.v info
    Term.(const (fun () r -> r) $ Xrpc_auth.Cli.term ~env:"TANGLED" () $ term)

let run f =
  try
    Eio_main.run (fun env ->
        Ui.set_clock (Eio.Stdenv.mono_clock env);
        f env);
    Ok ()
  with
  (* A reader such as [head] that has seen enough is not an error. The
     runtime words a failed write as strerror does. *)
  | Sys_error message when message = Unix.error_message Unix.EPIPE -> Ok ()
  | Failure message | Invalid_argument message | Sys_error message ->
      Error (`Msg message)
  | Eio.Io (Xrpc.Error.E e, _) -> Error (`Msg (Xrpc.Error.to_string e))
  | Httpz_websocket.Protocol_error (_, message) | Httpz_tls.Error message ->
      Error (`Msg message)
  | Httpz_websocket_eio.Upgrade_rejected status ->
      Error
        (`Msg (Printf.sprintf "WebSocket upgrade rejected (HTTP %d)" status))
  | Eio.Io _ as ex -> Error (`Msg (Printexc.to_string ex))

let json value =
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent Jsont.json value with
  | Ok text -> print_endline text
  | Error message -> failwith message

let print codec value = json (Api.encode codec value)

let optional names docv doc =
  Arg.(value & opt (some string) None & info names ~docv ~doc)

let positional index docv doc =
  Arg.(required & pos index (some string) None & info [] ~docv ~doc)

let knot = optional [ "knot"; "k" ] "KNOT" "Knot hostname or HTTP(S) origin."

(* For test gateways only, so it stays out of the manual. *)
let audience =
  Arg.(
    value
    & opt (some string) None
    & info [ "audience" ] ~docv:"DID" ~docs:Manpage.s_none
        ~env:(Cmd.Env.info "TANGLED_AUDIENCE")
        ~doc:"Override the audience of service tokens.")

let repo_doc =
  "Repository as handle/name, or a name of your own. Its DID or declaration \
   URI also work. The default is the repository the current directory is a \
   clone of."

(* The repository a command acts in. *)
let repo =
  Arg.(
    value
    & opt (some string) None
    & info [ "repo"; "r" ] ~docv:"REPO" ~doc:repo_doc)

(* The repository a command acts on, as its argument. *)
let repo_arg =
  Arg.(value & pos 0 (some string) None & info [] ~docv:"REPO" ~doc:repo_doc)

let repo_spec = function
  | Some spec -> spec
  | None -> (
      match Local.repo () with
      | Some spec -> spec
      | None ->
          failwith
            "This directory is not a clone of a Tangled repository. Name one \
             with --repo.")

let resolve api ?knot spec = Api.resolve_repo api ?knot (repo_spec spec)

let user =
  optional [ "user"; "u" ] "USER" "Handle or DID (default: logged-in user)."

let did api = function
  | Some user -> Api.resolve_handle api user
  | None -> Api.get_did api

(* [handles api dids] pairs each DID with its verified handle, looked up a
   few at a time. *)
let handles api dids =
  Eio.Fiber.List.map ~max_fibers:8
    (fun did -> (did, Api.handle_of_did api did))
    dids

let email contact =
  Option.bind contact (fun c ->
      Option.map Tangled.Schema.text (Tangled.Schema.member "email" c))

let json_flag = Arg.(value & flag & info [ "json" ] ~doc:"Print JSON.")

let cursor =
  optional [ "cursor" ] "CURSOR" "Continuation cursor from the previous page."

let limit =
  Arg.(
    value & opt int 50 & info [ "limit"; "l" ] ~doc:"Maximum results per page.")

let strings names docv doc =
  Arg.(value & opt_all string [] & info names ~docv ~doc)

let workflows =
  strings [ "workflow"; "w" ] "NAME" "Select a workflow (repeatable)."

let nonempty = function [] -> None | values -> Some values

let object_ pairs =
  Jsont.Json.object'
    (List.map (fun (k, v) -> Jsont.Json.mem (k, Jsont.Meta.none) v) pairs)

let string = Jsont.Json.string

let pair text =
  match String.index_opt text '=' with
  | None -> invalid_arg "Expected KEY=VALUE"
  | Some i when i > 0 ->
      (String.sub text 0 i, String.sub text (i + 1) (String.length text - i - 1))
  | Some _ -> invalid_arg "Empty parameter name"

let read_file path =
  let read channel =
    let limit = 16 * 1024 * 1024 in
    let buffer = Buffer.create 4096 in
    let bytes = Bytes.create 8192 in
    let rec loop () =
      let count = input channel bytes 0 (Bytes.length bytes) in
      if count > 0 then begin
        if Buffer.length buffer + count > limit then
          invalid_arg "Input exceeds 16 MiB";
        Buffer.add_subbytes buffer bytes 0 count;
        loop ()
      end
    in
    loop ();
    Buffer.contents buffer
  in
  if path = "-" then read stdin else In_channel.with_open_bin path read

let read_json path =
  match Jsont_bytesrw.decode_string Jsont.json (read_file path) with
  | Ok value -> value
  | Error message -> invalid_arg message

(* [body] is a ticket or comment body, given inline or read from a file. *)
let body =
  let inline =
    Arg.(
      value
      & opt (some string) None
      & info [ "body"; "b" ] ~docv:"TEXT" ~doc:"Body text, in Markdown.")
  in
  let file =
    Arg.(
      value
      & opt (some string) None
      & info [ "body-file"; "F" ] ~docv:"FILE"
          ~doc:"Read the body from FILE, or from standard input for -.")
  in
  let choose inline file =
    match (inline, file) with
    | Some _, Some _ -> Error (`Msg "Give --body or --body-file, not both")
    | Some text, None -> Ok (Some text)
    | None, Some path -> (
        match read_file path with
        | text -> Ok (Some text)
        | exception (Sys_error m | Invalid_argument m) -> Error (`Msg m))
    | None, None -> Ok None
  in
  Term.(term_result ~usage:true (const choose $ inline $ file))

let spindle =
  optional [ "spindle"; "s" ] "SPINDLE"
    "Spindle hostname or DID (default: the repository's spindle)."

(* The repository DID and spindle origin for [repo]. A repository DID with an
   explicit spindle needs no knot or PDS lookup. *)
let spindle_target api ~repo ~spindle =
  let repo = repo_spec repo in
  match spindle with
  | Some s
    when String.starts_with ~prefix:"did:" repo
         && not (String.contains repo '/') ->
      (repo, Api.spindle_url api s)
  | _ ->
      let repository = Api.resolve_repo api repo in
      (repository.did, Api.spindle_of api ?spindle repository)
