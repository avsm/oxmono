(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Type alias for convenience *)
module Knot = Atp_lexicon_tangled.Sh.Tangled.Knot

(* Helper to create API without requiring login for public queries *)
let with_public_api env ~knot f =
  Eio.Switch.run @@ fun sw ->
  (* Create a minimal client for the knot server directly *)
  let service = "https://" ^ knot in
  let client = Xrpc.Client.create ~sw ~env ~service () in
  f client

(* Knot version command *)

let knot_arg =
  let doc = "Knot server hostname." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"KNOT" ~doc)

let version_action ~knot env =
  with_public_api env ~knot @@ fun client ->
  let resp =
    Xrpc.Client.query client ~nsid:"sh.tangled.knot.version" ~params:[]
      ~decoder:Knot.Version.output_jsont
  in
  Fmt.pr "Knot: %s@." knot;
  Fmt.pr "Version: %s@." resp.version

let version_cmd =
  let doc = "Get knot server version." in
  let info = Cmd.info "version" ~doc in
  let version' knot = Eio_main.run @@ fun env -> version_action ~knot env in
  Cmd.v info Term.(const version' $ knot_arg)

(* Knot keys command *)

let limit_arg =
  let doc = "Maximum number of keys to return." in
  Arg.(value & opt (some int) None & info [ "limit"; "n" ] ~docv:"N" ~doc)

let pp_public_key ppf (k : Knot.ListKeys.public_key) =
  Fmt.pf ppf "@[<v>DID: %s@,Key: %s@,Created: %s@]" k.did
    (String.sub k.key 0 (min 50 (String.length k.key)) ^ "...")
    k.created_at

let keys_action ~knot ~limit env =
  with_public_api env ~knot @@ fun client ->
  let params =
    List.filter_map Fun.id
      [ Option.map (fun l -> ("limit", string_of_int l)) limit ]
  in
  let resp =
    Xrpc.Client.query client ~nsid:"sh.tangled.knot.listKeys" ~params
      ~decoder:Knot.ListKeys.output_jsont
  in
  if resp.keys = [] then Fmt.pr "No public keys found on %s.@." knot
  else begin
    Fmt.pr "Public keys on %s:@.@." knot;
    List.iter (fun k -> Fmt.pr "%a@.@." pp_public_key k) resp.keys
  end

let keys_cmd =
  let doc = "List public keys on a knot server." in
  let info = Cmd.info "keys" ~doc in
  let keys' knot limit =
    Eio_main.run @@ fun env -> keys_action ~knot ~limit env
  in
  Cmd.v info Term.(const keys' $ knot_arg $ limit_arg)

(* Knot command group *)

let cmd =
  let doc = "Knot server commands." in
  let info = Cmd.info "knot" ~doc in
  Cmd.group info [ version_cmd; keys_cmd ]
