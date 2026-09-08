(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The event source of RFC 8620 Section 7.3, against the oracle.

    Cyrus polls for changes every [jmap_pushpoll] seconds, 60 by default, so a
    change made while a connection is open is not seen for up to a minute. It
    does, however, implement the reconnection path of Section 7.3: a request
    carrying [Last-Event-ID] is answered with the changes since that id straight
    away. These tests use that path, which is the one a real client takes after
    a dropped connection, and is quick.

    One further oracle quirk shapes them: an open event-source connection holds
    the account's conversations database, which blocks LMTP delivery and JMAP
    API calls for the same user until it closes. Nothing here delivers mail
    while listening. *)

module Client = Jmap_eio.Client
module Push = Jmap_eio.Push
module Proto = Jmap.Proto
module State_change = Proto.Push.State_change

let listen_timeout = 30.0

(* Every listen is bounded: a stalled event source would otherwise hang the
   suite rather than fail it. *)
let with_deadline t what f =
  let clock = Eio.Stdenv.clock t.Oracle_harness.env in
  match Eio.Time.with_timeout_exn clock listen_timeout f with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%s: %s" what (Client.error_to_string e)
  | exception Eio.Time.Timeout ->
      Alcotest.failf "%s: nothing arrived within %.0fs" what listen_timeout

(* The type states the change reports for the account under test. *)
let for_account t change =
  match
    List.assoc_opt t.Oracle_harness.account_id change.State_change.changed
  with
  | Some types -> types
  | None ->
      Alcotest.failf "StateChange names %d other accounts but not %s"
        (List.length change.State_change.changed)
        (Proto.Id.to_string t.Oracle_harness.account_id)

let state_of types name =
  match List.find_opt (fun ts -> ts.State_change.type_name = name) types with
  | Some ts -> ts.State_change.state
  | None ->
      Alcotest.failf "no %s in the StateChange, only %s" name
        (String.concat ", "
           (List.map (fun ts -> ts.State_change.type_name) types))

(* One event-source connection, driven at the [Fetch.Sse] level rather than
   through {!Push.listen}, because these tests check the event ids the server
   sends and a decoded {!Push.event} does not carry one. *)
let listen_events t ?types ?close_after ?ping ?last_event_id f =
  let client = t.Oracle_harness.client in
  let url = Push.event_source_url client ?types ?close_after ?ping () in
  Eio.Switch.run @@ fun sw ->
  match Fetch.Sse.connect ~sw ?last_event_id (Client.fetch client) url with
  | Error response ->
      Alcotest.failf "the event source answered %d" (Fetch.status response)
  | Ok events ->
      let rec loop events =
        match events () with
        | Seq.Nil -> Ok ()
        | Seq.Cons (event, rest) -> (
            match f event with `Stop -> Ok () | `Continue -> loop rest)
      in
      loop events

(* Connect asking to be told everything that changed since [last_event_id],
   and return the first StateChange with the id that came with it. RFC 8620
   Section 7.3: with closeafter=state the server ends the response after the
   state event, so this is one short request rather than a held connection. *)
let state_since t ~last_event_id what =
  let seen = ref None in
  with_deadline t what (fun () ->
      listen_events t ~types:[ "Email" ] ~close_after:`State ~ping:5
        ~last_event_id (fun event ->
          match Push.decode event with
          | Push.State_change change ->
              seen := Some (change, event.Fetch.Sse.id);
              `Stop
          | Push.Ping _ -> `Continue
          | Push.Unknown (name, data) ->
              Alcotest.failf "unexpected %s event: %s" name data));
  match !seen with
  | Some (change, id) -> (for_account t change, id)
  | None -> Alcotest.failf "%s: the stream ended without a state event" what

(* RFC 8620 Section 7.3: a ping event carries the interval the server settled
   on, and MUST NOT set a new event id. *)
let test_ping t =
  let pings = ref [] in
  with_deadline t "ping" (fun () ->
      listen_events t ~types:[ "Email" ] ~ping:2 (fun event ->
          match Push.decode event with
          | Push.Ping { interval } ->
              pings := (interval, event.Fetch.Sse.id) :: !pings;
              if List.length !pings >= 2 then `Stop else `Continue
          | _ -> `Continue));
  Alcotest.(check int) "two pings" 2 (List.length !pings);
  List.iter
    (fun (interval, id) ->
      Alcotest.(check int64) "the interval we asked for" 2L interval;
      Alcotest.(check (option string)) "a ping sets no event id" None id)
    !pings

(* Deliver a message, then reconnect with the id from before it: the server
   replays the change, naming Email for this account with a new state. *)
let test_state_change t =
  (* An id of "1" precedes anything this account has ever done, so the first
     connection reports the current state of Email whatever it is. *)
  let before, id_before = state_since t ~last_event_id:"1" "baseline" in
  let email_before = state_of before "Email" in
  let id_before =
    match id_before with
    | Some id -> id
    | None -> Alcotest.fail "the state event carried no id to reconnect with"
  in
  let _email_id, subject = Oracle_harness.deliver_and_wait t () in
  let after, _ =
    state_since t ~last_event_id:id_before ("change for " ^ subject)
  in
  let email_after = state_of after "Email" in
  Alcotest.(check bool)
    (Printf.sprintf "Email state moved on from %s" email_before)
    true
    (email_after <> email_before)

(* An account only ever asks about the types it named, so a StateChange from a
   types=Mailbox connection never mentions Email. *)
let test_types_filter t =
  let seen = ref [] in
  with_deadline t "types filter" (fun () ->
      Push.listen t.Oracle_harness.client ~types:[ "Mailbox" ]
        ~close_after:`State ~ping:5 ~last_event_id:"1" (fun event ->
          match event with
          | Push.State_change change ->
              seen :=
                List.map
                  (fun ts -> ts.State_change.type_name)
                  (for_account t change);
              `Stop
          | _ -> `Continue));
  Alcotest.(check (list string)) "only the type asked for" [ "Mailbox" ] !seen

(* {1 Subscriptions}

   Push.subscribe is the reconnecting loop: a fiber that connects, reads,
   remembers the last event id and comes back. Against Cyrus it has to run in
   its polling mode. An open event-source connection there holds the account's
   conversations database, and an LMTP delivery for that user is not
   acknowledged until the connection closes; a connection whose Last-Event-ID
   is already current is held open until the server's own change poll, 60
   seconds away by default. So [~poll:1.] - at most a second connected, then a
   second disconnected - is what lets these tests both watch and receive mail,
   and [~last_event_id:"1"] is what makes the first connection answer at once
   rather than hang: an id older than anything the account has done is
   replayed immediately, no id at all is not. *)

let subscribe t ~sw ?(poll = 1.) () =
  Push.subscribe ~sw t.Oracle_harness.client ~types:[ "Email" ] ~ping:5 ~poll
    ~last_event_id:"1" ~backoff_initial:0.5 ~backoff_max:5. ()

(* The next Email state for this account, ignoring a replay of one already
   seen, within [timeout] seconds overall. *)
let next_state t sub ~timeout ~different_from =
  let clock = Eio.Stdenv.clock t.Oracle_harness.env in
  let deadline = Eio.Time.now clock +. timeout in
  let remaining () = deadline -. Eio.Time.now clock in
  let rec loop () =
    if remaining () <= 0. then None
    else
      match
        Push.wait_for_state sub ~timeout:(remaining ()) ~type_:"Email"
          ~account_id:t.Oracle_harness.account_id ()
      with
      | Some state when Some state = different_from -> loop ()
      | answer -> answer
  in
  loop ()

(* Deliver between two polls and let the subscription tell us: the state it
   reports afterwards is not the one it reported before. *)
let test_subscribe_delivery t =
  Eio.Switch.run @@ fun sw ->
  let sub = subscribe t ~sw () in
  let before =
    match next_state t sub ~timeout:listen_timeout ~different_from:None with
    | Some state -> state
    | None -> Alcotest.failf "no baseline state within %.0fs" listen_timeout
  in
  let _subject, raw = Oracle_harness.message () in
  (* The delivery blocks while a connection happens to be open, and is
     acknowledged as soon as the poll cycle drops it. *)
  Oracle_harness.deliver t raw;
  (match
     next_state t sub ~timeout:listen_timeout ~different_from:(Some before)
   with
  | Some after ->
      Alcotest.(check bool)
        (Printf.sprintf "Email state moved on from %s (to %s)" before after)
        true (after <> before)
  | None ->
      Alcotest.failf "no state change within %.0fs of the delivery"
        listen_timeout);
  Alcotest.(check bool)
    "the subscription is still running" false
    (Eio.Promise.is_resolved (Push.result sub));
  Push.close sub

(* Closing gets rid of a subscription that is mid-connection, promptly. *)
let test_subscribe_close t =
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock t.Oracle_harness.env in
  let sub = subscribe t ~sw ~poll:30. () in
  (* Wait until it is connected, which the first state event proves. *)
  ignore
    (Push.wait_for_state sub ~timeout:listen_timeout ~type_:"Email"
       ~account_id:t.Oracle_harness.account_id ());
  Push.close sub;
  match
    Eio.Time.with_timeout_exn clock 5. (fun () ->
        let stopped = Eio.Promise.await (Push.result sub) in
        (stopped, Push.next sub))
  with
  | Ok (), `End -> ()
  | Error e, _ ->
      Alcotest.failf "the subscription failed rather than stopped: %s"
        (Client.error_to_string e)
  | Ok (), `Event e ->
      Alcotest.failf "expected the end of the stream, got %a" Push.pp_event e
  | exception Eio.Time.Timeout ->
      Alcotest.fail "close did not stop the subscription within 5s"

let () =
  Oracle_harness.run "oracle-push"
    [
      ( "eventsource",
        [
          Oracle_harness.test_case "ping" test_ping;
          Oracle_harness.test_case "state change after delivery"
            test_state_change;
          Oracle_harness.test_case "types filter" test_types_filter;
        ] );
      ( "subscribe",
        [
          Oracle_harness.test_case "delivery between polls"
            test_subscribe_delivery;
          Oracle_harness.test_case "close" test_subscribe_close;
        ] );
    ]
