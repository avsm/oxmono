(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Type aliases for convenience *)
module Profile = Tangled.Api.Lex.Actor.Profile
module SshKey = Tangled.Api.Lex.Key.SshKey

let with_api = Common.with_api

(* Pretty printers *)

let pp_profile ppf (p : Profile.main) =
  Fmt.pf ppf "@[<v>%a%a%a%a%a%a@]"
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
    Fmt.(option (fmt "Preferred handle: %s@,"))
    p.preferred_handle
    (fun ppf -> function
      | Some true -> Fmt.pf ppf "Organization@," | _ -> ())
    p.is_organization

let pp_public_key ppf (rkey, (k : SshKey.main)) =
  Fmt.pf ppf "@[<v>%s (%s)@,  %s@,  Usages: %s@,  Created: %s@]" k.name rkey
    k.public_key
    (String.concat ", " (Option.value ~default:[ "auth"; "sign" ] k.usages))
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
  let profile =
    Ui.busy "Reading profile" (fun () -> Tangled.Api.get_profile api ~did)
  in
  if Ui.tty () then
    let get f = Option.bind profile (fun (p : Profile.main) -> f p) in
    Ui.panel (Ui.title handle)
      (Ui.V.vcat ~gap:1
         [
           Ui.fields
             [
               ("DID", Some (Ui.accent did));
               ("Pronouns", Option.map Ui.text (get (fun p -> p.pronouns)));
               ("Location", Option.map Ui.text (get (fun p -> p.location)));
               ( "Handle",
                 Option.map Ui.text (get (fun p -> p.preferred_handle)) );
               ( "Kind",
                 if get (fun p -> p.is_organization) = Some true then
                   Some (Ui.text "organization")
                 else None );
               ( "Links",
                 match get (fun p -> p.links) with
                 | Some (_ :: _ as l) -> Some (Ui.V.vcat (List.map Ui.link l))
                 | _ -> None );
             ];
           (match (profile, get (fun p -> p.description)) with
           | None, _ -> Ui.muted "No Tangled profile set."
           | Some _, Some d -> Ui.para d
           | Some _, None -> Ui.V.empty);
         ])
  else begin
    Fmt.pr "Profile: %s@." handle;
    Fmt.pr "DID: %s@.@." did;
    match profile with
    | Some profile -> Fmt.pr "%a@." pp_profile profile
    | None -> Fmt.pr "(No Tangled profile set)@."
  end

let view_cmd =
  let doc = "View a user's Tangled profile." in
  let info = Cmd.info "view" ~doc in
  Common.cmd info
    Term.(
      term_result
        (const (fun user -> Common.run (fun env -> view_action ~user env))
        $ user_arg))

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
  let keys =
    Ui.busy "Reading SSH keys" (fun () -> Tangled.Api.list_ssh_keys api ~did ())
  in
  if Ui.tty () then Cmd_key.show_keys keys
  else if keys = [] then Fmt.pr "No SSH keys found.@."
  else begin
    Fmt.pr "SSH Keys for %s:@.@." did;
    List.iter (fun k -> Fmt.pr "%a@.@." pp_public_key k) keys
  end

let keys_cmd =
  let doc = "List a user's SSH public keys." in
  let info = Cmd.info "keys" ~doc in
  Common.cmd info
    Term.(
      term_result
        (const (fun user -> Common.run (fun env -> keys_action ~user env))
        $ user_arg))

(* Profile command group *)

let cmd =
  let doc = "User profile commands." in
  let info = Cmd.info "profile" ~doc in
  Cmd.group info [ view_cmd; keys_cmd ]
