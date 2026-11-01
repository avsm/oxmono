(* SPDX-License-Identifier: ISC *)
open Cmdliner
module Api = Tangled.Api

let create_api ~sw ~env ~app_name ~pds () =
  let http = Fetch_httpz.std env in
  Api.create ~sw ~env ~app_name ~pds ~http ()

let with_api ?(authenticated = true) env f =
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

let run f =
  try
    Eio_main.run f;
    Ok ()
  with
  | Failure message | Invalid_argument message | Sys_error message ->
      Error (`Msg message)
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

let audience =
  optional [ "audience" ] "DID"
    "Override the service token audience (for test gateways)."

let user =
  optional [ "user"; "u" ] "USER" "Handle or DID (default: logged-in user)."

let did api = function
  | Some user -> Api.resolve_handle api user
  | None -> Api.get_did api

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
