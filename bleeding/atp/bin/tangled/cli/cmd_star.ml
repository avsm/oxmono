(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Type alias for convenience *)
module Star = Atp_lexicon_tangled.Sh.Tangled.Feed.Star

let app_name = "tangled"

(* Helper to load session and create API *)
let with_api env f =
  Eio.Switch.run @@ fun sw ->
  let fs = env#fs in
  match Xrpc_auth.Session.load fs ~app_name () with
  | None ->
      Fmt.epr "Not logged in. Use 'tangled auth login' first.@.";
      exit 1
  | Some session ->
      let api = Tangled.Api.create ~sw ~env ~app_name ~pds:session.pds () in
      Tangled.Api.resume api ~session;
      f api

(* Pretty printer for star *)
let pp_star ppf (_rkey, (s : Star.main)) =
  (* subject is an AT URI like at://did:plc:.../sh.tangled.repo/reponame *)
  let repo_display =
    match Tangled.Types.parse_at_uri s.subject with
    | Some uri -> Printf.sprintf "%s/%s" uri.did uri.rkey
    | None -> s.subject
  in
  Fmt.pf ppf "%s (starred %s)" repo_display s.created_at

(* Stars list command *)

let user_arg =
  let doc = "User handle or DID (default: logged-in user)." in
  Arg.(value & opt (some string) None & info [ "user"; "u" ] ~docv:"USER" ~doc)

let list_action ~user env =
  with_api env @@ fun api ->
  let did =
    match user with
    | Some u ->
        if String.starts_with ~prefix:"did:" u then u
        else Tangled.Api.resolve_handle api u
    | None -> Tangled.Api.get_did api
  in
  let stars = Tangled.Api.list_stars api ~did () in
  if stars = [] then Fmt.pr "No starred repositories.@."
  else begin
    Fmt.pr "Starred repositories:@.@.";
    List.iter (fun s -> Fmt.pr "  %a@." pp_star s) stars
  end

let list_cmd =
  let doc = "List starred repositories." in
  let info = Cmd.info "list" ~doc in
  let list' user = Eio_main.run @@ fun env -> list_action ~user env in
  Cmd.v info Term.(const list' $ user_arg)

(* Star command group *)

let cmd =
  let doc = "Star/unstar repositories." in
  let info = Cmd.info "star" ~doc in
  Cmd.group info [ list_cmd ]
