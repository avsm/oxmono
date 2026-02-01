(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Type aliases for convenience *)
module Profile = Atp_lexicon_tangled.Sh.Tangled.Actor.Profile
module PublicKey = Atp_lexicon_tangled.Sh.Tangled.PublicKey

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

(* Pretty printers *)

let pp_profile ppf (p : Profile.main) =
  Fmt.pf ppf "@[<v>%a%a%a%a%a@]"
    Fmt.(option (fmt "Description: %s@,"))
    p.description
    Fmt.(option (fmt "Location: %s@,"))
    p.location
    Fmt.(option (fmt "Pronouns: %s@,"))
    p.pronouns
    (fun ppf links ->
      match links with
      | Some l when l <> [] ->
          Fmt.pf ppf "Links:@,";
          List.iter (fun link -> Fmt.pf ppf "  - %s@," link) l
      | _ -> ())
    p.links
    (fun ppf -> function true -> Fmt.pf ppf "Bluesky: linked@," | false -> ())
    p.bluesky

let pp_public_key ppf (rkey, (k : PublicKey.main)) =
  Fmt.pf ppf "@[<v>%s (%s)@,  %s@,  Created: %s@]" k.name rkey
    (String.sub k.key 0 (min 60 (String.length k.key)) ^ "...")
    k.created_at

(* Profile view command *)

let user_arg =
  let doc = "User handle or DID (default: logged-in user)." in
  Arg.(value & opt (some string) None & info [ "user"; "u" ] ~docv:"USER" ~doc)

let view_action ~user env =
  with_api env @@ fun api ->
  let did =
    match user with
    | Some u ->
        if String.starts_with ~prefix:"did:" u then u
        else Tangled.Api.resolve_handle api u
    | None -> Tangled.Api.get_did api
  in
  let handle =
    match user with
    | Some u when not (String.starts_with ~prefix:"did:" u) -> u
    | _ -> did
  in
  Fmt.pr "Profile: %s@." handle;
  Fmt.pr "DID: %s@.@." did;
  match Tangled.Api.get_profile api ~did with
  | Some profile -> Fmt.pr "%a@." pp_profile profile
  | None -> Fmt.pr "(No Tangled profile set)@."

let view_cmd =
  let doc = "View a user's Tangled profile." in
  let info = Cmd.info "view" ~doc in
  let view' user = Eio_main.run @@ fun env -> view_action ~user env in
  Cmd.v info Term.(const view' $ user_arg)

(* Keys list command *)

let keys_action ~user env =
  with_api env @@ fun api ->
  let did =
    match user with
    | Some u ->
        if String.starts_with ~prefix:"did:" u then u
        else Tangled.Api.resolve_handle api u
    | None -> Tangled.Api.get_did api
  in
  let keys = Tangled.Api.list_public_keys api ~did () in
  if keys = [] then Fmt.pr "No SSH keys found.@."
  else begin
    Fmt.pr "SSH Keys for %s:@.@." did;
    List.iter (fun k -> Fmt.pr "%a@.@." pp_public_key k) keys
  end

let keys_cmd =
  let doc = "List a user's SSH public keys." in
  let info = Cmd.info "keys" ~doc in
  let keys' user = Eio_main.run @@ fun env -> keys_action ~user env in
  Cmd.v info Term.(const keys' $ user_arg)

(* Profile command group *)

let cmd =
  let doc = "User profile commands." in
  let info = Cmd.info "profile" ~doc in
  Cmd.group info [ view_cmd; keys_cmd ]
