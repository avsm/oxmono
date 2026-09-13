(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

let connect ~sw env profile =
  match Jmap_eio.Profile.connect ~sw env profile with
  | Ok client -> client
  | Error _ ->
      fail "cannot connect to JMAP profile %s" (Jmap_eio.Profile.name profile)

let read client ~account ~email_id ~fetch_body =
  let account_id = get_ok (Jmap.Proto.Id.of_string_received account)
  and email_id = get_ok (Jmap.Proto.Id.of_string_received email_id) in
  let got =
    Jmap_eio.Client.call_exn client
      ~capabilities:[ Jmap.Proto.Capability.core; Jmap.Proto.Capability.mail ]
      Jmap.Chain.(
        email_get ~account_id ~ids:(ids [ email_id ])
          ~fetch_text_body_values:fetch_body ~max_body_value_bytes:262144L
          ~properties:
            ([ `Id; `Subject; `Thread_id; `Message_id ]
            @ if fetch_body then [ `Text_body; `Body_values ] else [])
          ())
  in
  if not (Jmap.Proto.Id.equal got.account_id account_id) then
    fail "JMAP returned a different account";
  match got.list with
  | [ m ] when m.id = Some email_id -> m
  | _ -> fail "JMAP source message not found or response identity differs"

let lookup ~sw env ~(config : Config.t) ?profile ?account ?service
    ?(fetch_body = false) email_id =
  let profile =
    match (profile, config.jmap_profile) with
    | Some p, _ | None, Some p -> p
    | _ -> fail "configure jmap.profile or pass --jmap-profile"
  in
  let store =
    match Jmap_eio.Profile.xdg_store env with
    | Ok s -> s
    | Error _ -> fail "cannot locate JMAP profiles"
  in
  let p =
    match Jmap_eio.Profile.load store profile with
    | Ok p -> p
    | Error _ -> fail "cannot load JMAP profile %s" profile
  in
  let binding =
    Link.service
      (Option.value config.jmap_service
         ~default:(Jmap_eio.Profile.session_url p))
  in
  let service =
    match service with
    | None -> binding
    | Some s ->
        let s = Link.service s in
        if s <> binding then
          fail "source service does not match this JMAP profile binding";
        s
  in
  let client = connect ~sw env p in
  let session = Jmap_eio.Client.session client in
  let account =
    match (account, config.jmap_account) with
    | Some a, _ | None, Some a -> a
    | _ -> (
        match
          Jmap.Proto.Session.primary_account_for Jmap.Proto.Capability.mail
            session
        with
        | Some a -> Jmap.Proto.Id.to_string a
        | None -> fail "JMAP has no primary mail account; pass --account")
  in
  let target = Link.email ~service ~account ~email:email_id in
  let message = read client ~account ~email_id ~fetch_body in
  let title =
    match message.subject with
    | Some s when String.trim s <> "" -> s
    | _ -> "Email task"
  in
  let hints =
    obj
      ([ ("subject", str title) ]
      @ (match message.thread_id with
        | None -> []
        | Some s -> [ ("thread_id", str (Jmap.Proto.Id.to_string s)) ])
      @
      match message.message_id with
      | None -> []
      | Some xs -> [ ("message_ids", arr (List.map str xs)) ])
  in
  let body =
    if not fetch_body then None
    else
      let values = Option.value message.body_values ~default:[] in
      let parts = Option.value message.text_body ~default:[] in
      Some
        (String.concat "\n"
           (List.filter_map
              (fun (part : Jmap.Proto.Email_body.Part.t) ->
                Option.bind part.part_id (fun id ->
                    Option.map
                      (fun (v : Jmap.Proto.Email_body.Value.t) ->
                        v.value
                        ^
                        if v.is_truncated then "\n[Message body truncated]\n"
                        else "")
                      (List.assoc_opt id values)))
              parts))
  in
  (target, title, hints, body)
