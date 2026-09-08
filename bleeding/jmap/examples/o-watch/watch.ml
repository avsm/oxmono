(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Proto = Jmap.Proto
module Client = Jmap_eio.Client
module Push = Jmap_eio.Push
module Sync = Jmap_eio.Sync

let date = Option.fold ~none:"?" ~some:Proto.Date.to_utc_string

let sender (e : Proto.Email.t) =
  match e.from with
  | Some (a :: _) -> Option.value a.Proto.Email_address.name ~default:a.email
  | _ -> "-"

let args =
  let open Cmdliner in
  let poll =
    let doc =
      "Close each event source connection after $(docv) seconds and reopen it, \
       rather than holding one open. 0 holds one open."
    in
    Arg.(value & opt float 2. & info [ "poll" ] ~docv:"SECONDS" ~doc)
  in
  let timeout =
    let doc = "Stop watching after $(docv) seconds." in
    Arg.(value & opt float 20. & info [ "timeout" ] ~docv:"SECONDS" ~doc)
  in
  let once =
    let doc = "Stop once a state change has been caught up with." in
    Arg.(value & flag & info [ "once" ] ~doc)
  in
  Term.(const (fun p t o -> (p, t, o)) $ poll $ timeout $ once)

let fetch client ~account_id ids =
  match
    Sync.get_all client ids (fun ~ids ->
        Chain.email_get ~account_id ~ids:(Chain.ids ids)
          ~properties:[ `Id; `Subject; `From; `Received_at ]
          ())
  with
  | Ok (emails, _) -> Proto.Method.in_ids_order ~id:Proto.Email.id ids emails
  | Error e -> Fmt.failwith "Email/get: %a" Sync.pp_error e

let () =
  Jmap_eio.Cli.main' "watch" ~doc:"Print a line for every message as it arrives"
    ~args
  @@ fun ctx (poll, timeout, once) ->
  let client = ctx.client and account_id = ctx.account_id in
  let clock = Eio.Stdenv.clock ctx.env in
  let state = ref (Client.call_exn client (Chain.email_state ~account_id)) in
  let sub =
    Push.subscribe ~sw:ctx.sw client ~types:[ "Email" ] ~ping:30
      ?poll:(if poll > 0. then Some poll else None)
      ~last_event_id:"1" ()
  in
  Fmt.pr "Watching Email in %a from state %s for %.0f s@." Proto.Id.pp
    account_id !state timeout;
  let deadline = Eio.Time.now clock +. timeout in
  let arrived = ref 0 in
  let rec loop ?(draining = false) () =
    let remaining = deadline -. Eio.Time.now clock in
    if remaining > 0. then
      match
        if draining then Some ()
        else
          Option.map
            (fun _ -> ())
            (Push.wait_for_state sub ~timeout:remaining ~since:!state
               ~type_:"Email" ~account_id ())
      with
      | None -> ()
      | Some () -> (
          match Sync.email_changes client ~account_id ~since:!state () with
          | Error e -> Fmt.failwith "Email/changes: %a" Sync.pp_error e
          | Ok `Cannot_calculate_changes ->
              Fmt.failwith
                "cannotCalculateChanges: the mail cache must be resynchronised"
          | Ok (`Changes c) ->
              List.iter
                (fun (e : Proto.Email.t) ->
                  incr arrived;
                  Fmt.pr "%-20s  %-24s  %s@." (date e.received_at) (sender e)
                    (Option.value e.subject ~default:"(no subject)"))
                (fetch client ~account_id c.created);
              let changed = List.length c.updated
              and removed = List.length c.destroyed in
              if changed + removed > 0 then
                Fmt.pr "%-20s  %d changed, %d removed@." "-" changed removed;
              state := c.new_state;
              if c.has_more then loop ~draining:true ()
              else if not once then loop ())
  in
  loop ();
  Push.close sub;
  Fmt.pr "@.%d new message(s). Email state %s, last event id %s@." !arrived
    !state
    (Option.value (Push.last_event_id sub) ~default:"-")
