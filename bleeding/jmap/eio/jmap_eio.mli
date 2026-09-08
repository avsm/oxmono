(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JMAP over Eio.

    A JMAP client is HTTP and nothing else
    ({{:https://www.rfc-editor.org/rfc/rfc8620#section-3}RFC 8620 §3}). It
    fetches a session resource, POSTs batches of method calls to the [apiUrl]
    the session names, and moves blobs through the [uploadUrl] and [downloadUrl]
    beside it. This library is that, on Eio. {!Auth} says who the client is,
    {!Transport} says how the HTTP is done, {!Client} holds the session and
    makes the requests, and {!Sync} and {!Push} are the two loops, poll and be
    told, that keep a local view up to date.

    {2 Quick start}

    {[
    let () =
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let open Jmap_eio in
      (* One call fetches the session and scopes the token to the origins
           it names. The switch owns anything that outlives a request. *)
      let client =
        Client.connect_env ~sw ~timeout:30.
          ~auth:(Auth.bearer "your-token")
          env "https://api.example.com/.well-known/jmap"
        |> Result.get_ok
      in
      let account_id =
        Option.get
          (Jmap.Proto.Session.primary_account_for Jmap.Proto.Capability.mail
             (Client.session client))
      in
      let capabilities =
        [ Jmap.Proto.Capability.core; Jmap.Proto.Capability.mail ]
      in
      (* A chain is one round trip: the /get refers to the /query's ids,
           and the properties it asks for are typed. *)
      let got =
        Client.call_exn client ~capabilities
          Jmap.Chain.(
            let* q = email_query ~account_id ~limit:10L () in
            email_get ~account_id ~ids:(from_query q)
              ~properties:[ `Id; `Subject ] ())
      in
      List.iter
        (fun (e : Jmap.Proto.Email.t) ->
          Fmt.pr "%s@." (Option.value e.subject ~default:""))
        got.list;
      (* Paging, change draining and /get batching obey the session's own
           limits. *)
      (match
         Sync.all_ids client ~capabilities ~max:500 (fun ~position ~limit ->
             Jmap.Chain.email_query ~account_id ~position ~limit ())
       with
      | Ok ids -> Fmt.pr "%d ids@." (List.length ids)
      | Error e -> Fmt.epr "%s@." (Sync.error_to_string e));
      (* Push: a fiber under [sw] keeps the event source connected and
           reconnects with Last-Event-ID. *)
      let sub = Push.subscribe ~sw client ~types:[ "Email" ] ~ping:30 () in
      match Push.next sub with
      | `Event ev -> Fmt.pr "%a@." Push.pp_event ev
      | `End -> ()
    ]}

    An attachment never has to be a [string]. {!Client.upload_flow} sends the
    bytes of an [Eio.Flow.source] and {!Client.download_to} writes a blob to an
    [Eio.Flow.sink] as it arrives
    ({{:https://www.rfc-editor.org/rfc/rfc8620#section-6}RFC 8620 §6}).

    {[
    Eio.Path.with_open_in path @@ fun file ->
    let length = Optint.Int63.to_int64 (Eio.File.size file) in
    Client.upload_flow client ~account_id ~content_type:"message/rfc822" ~length
      file
    ]}

    {2 The modules}

    {!Auth} is the credential, which is a bearer token
    ({{:https://www.rfc-editor.org/rfc/rfc6750#section-2.1}RFC 6750 §2.1}), a
    basic pair ({{:https://www.rfc-editor.org/rfc/rfc7617}RFC 7617}), a file
    read on demand, an OAuth thunk, or what the environment names. Nothing in it
    is HTTP specific, so a program can choose a scheme before it has an Eio
    environment.

    {!Transport} is the HTTP stack, an httpz backend with TLS against the system
    trust anchors, a cookie jar, retries and per-origin pacing, plus the clocks
    {!Client} times an exchange with. It opens no socket and needs no switch, so
    one may be shared. {!Transport.of_fetch} puts a mock or a recorded fixture
    underneath instead.

    {!Client} holds the session (RFC 8620 §2), makes the requests
    ({!Client.request}, {!Client.chain}, {!Client.call} for a chain ending in
    one handle and {!Client.run} for one ending in several), moves the blobs
    ({!Client.upload}, {!Client.upload_flow}, {!Client.download_to}) and runs
    the lifecycle. A response whose [sessionState] disagrees with the cached
    session refetches it once, one exchange however many fibers noticed and one
    a caller can still give up on, and tells {!Client.on_session_change}. Its
    [maxConcurrentRequests] and [maxConcurrentUpload] bound the fibers in
    flight.

    {!Sync} is the loops the server's limits force. {!Sync.pages} walks a
    [/query] by [position] (§5.5), {!Sync.val-changes} drains [hasMoreChanges]
    (§5.2), and {!Sync.get_all} splits an id list over [maxObjectsInGet] (§5.1).

    {!Push} is the event source of RFC 8620 §7.3, over [Fetch.Sse].
    {!Push.subscribe} forks a fiber that reconnects with [Last-Event-ID] and
    backoff and delivers into a bounded [Eio.Stream.t], and {!Push.listen} is
    the single connection under it.

    {!Chain} is {!Jmap.Chain}, method calls that refer to each other's results
    by back reference (RFC 8620 §3.7).

    {!Profile} is the shared XDG store for named session URLs and credentials.
    {!Profile.connect_name} is one call from a profile selected by a small tool
    to an authenticated client and its fetched session.

    {!Cli} is the [Cmdliner] terms for [--profile], [--url], [--api-key] and
    their neighbours, with {!Cli.connect} to turn them into a client.

    {!Codec} is JSON text for a JMAP value, for a program that reads or writes
    one outside an exchange.

    {2 Capabilities}

    A request names the capability URIs it uses in [using]
    ({{:https://www.rfc-editor.org/rfc/rfc8620#section-3.3}RFC 8620 §3.3}), and
    the session says which the server supports. They are constants in
    {!Jmap.Proto.Capability}, namely [core], [mail], [submission] and
    [vacation_response]. *)

module Codec = Codec
(** JSON text for JMAP values. *)

module Client = Client
(** The JMAP client. *)

module Cli = Cli
(** Command line configuration for a JMAP tool. *)

module Push = Push
(** JMAP push over the event source (RFC 8620 §7.3). *)

module Auth = Auth
(** How a JMAP client authenticates. *)

module Transport = Transport
(** The HTTP stack a JMAP client runs on. *)

module Sync = Sync
(** Paging, change draining and batching over the server's limits. *)

module Profile = Profile
(** Shared named connection profiles. *)

module Chain = Jmap.Chain
(** Requests whose method calls refer to each other's results. *)
