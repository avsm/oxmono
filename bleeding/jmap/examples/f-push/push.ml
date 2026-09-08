(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Proto = Jmap.Proto
module Client = Jmap_eio.Client
module Push = Jmap_eio.Push
module Sync = Jmap_eio.Sync

let pp_ids ppf = function
  | [] -> Format.pp_print_string ppf "-"
  | l -> Fmt.(list ~sep:(any " ") Proto.Id.pp) ppf l

let poll_term =
  let open Cmdliner in
  let doc =
    "Close each event source connection after $(docv) seconds and reopen it, \
     rather than holding one open. 0 holds one open."
  in
  Arg.(value & opt float 2. & info [ "poll" ] ~docv:"SECONDS" ~doc)

let () =
  Jmap_eio.Cli.main' "push" ~doc:"Wait for a StateChange on the event source"
    ~args:poll_term
  @@ fun ctx poll ->
  let client = ctx.client and account_id = ctx.account_id in
  let state = Client.call_exn client (Chain.email_state ~account_id) in
  let poll = if poll > 0. then Some poll else None in
  let sub =
    Push.subscribe ~sw:ctx.sw client ~types:[ "Email" ] ~ping:30 ?poll
      ~last_event_id:"1" ()
  in
  Fmt.pr "Event source %s@."
    (Push.event_source_url client ~types:[ "Email" ]
       ~close_after:(if Option.is_some poll then `State else `No)
       ~ping:30 ());
  Fmt.pr "Waiting 20 s for an Email StateChange after %s@." state;
  let announced =
    Push.wait_for_state sub ~timeout:20. ~since:state ~type_:"Email" ~account_id
      ()
  in
  Push.close sub;
  match announced with
  | None -> Fmt.pr "No change within 20 s.@."
  | Some s -> (
      Fmt.pr "StateChange: Email is now at %s@." s;
      match Sync.email_changes client ~account_id ~since:state () with
      | Ok (`Changes c) ->
          Fmt.pr "  created %a  updated %a  destroyed %a@." pp_ids
            c.Sync.created pp_ids c.Sync.updated pp_ids c.Sync.destroyed
      | Ok `Cannot_calculate_changes -> Fmt.pr "  cannotCalculateChanges@."
      | Error e -> Fmt.failwith "Email/changes: %a" Sync.pp_error e)
