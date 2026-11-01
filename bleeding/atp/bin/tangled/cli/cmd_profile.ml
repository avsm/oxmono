(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Type aliases for convenience *)
module Profile = Tangled.Api.Lex.Actor.Profile

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

(* Profile view command *)

let user_arg =
  Arg.(
    value
    & pos 0 (some string) None
    & info [] ~docv:"USER"
        ~doc:"The user, by handle. Without it, the logged-in user.")

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

let cmd =
  Common.cmd
    (Cmd.info "profile" ~doc:"Show a user's Tangled profile.")
    Term.(
      term_result
        (const (fun user -> Common.run (fun env -> view_action ~user env))
        $ user_arg))
