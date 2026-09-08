(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The loops RFC 8620 leaves to the client.

    {!Jmap_eio.Client} sends one request and hands back one response. Three
    things a JMAP client does are not one request.

    Paging a query. [Foo/query] answers at most [limit] ids from [position], and
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-5.5} RFC 8620
     Section 5.5} lets the server impose a smaller [limit] than the one asked
    for and report it back, so walking a result list is a loop that must follow
    the server's limit. See {!pages} and {!all_ids}.

    Draining [/changes].
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-5.2} RFC 8620
     Section 5.2} caps a delta at [maxChanges] and sets [hasMoreChanges]. The
    client calls again from the [newState] just returned until the flag clears,
    and folds the rounds together under the section's rules, so that a record
    created and then destroyed is not reported at all. A server may also answer
    [cannotCalculateChanges], which is an instruction to resync rather than a
    failure. See {!val-changes} and its typed wrappers.

    Batching a [/get].
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-5.1} RFC 8620
     Section 5.1} answers [requestTooLarge] when more than [maxObjectsInGet] ids
    are asked for in one call, so a long id list is several requests, which can
    run concurrently up to [maxConcurrentRequests]. See {!get_all}.

    Every function here takes the {!Jmap_eio.Client.t} used for a single
    request. The loops are ordinary fibers, so they run under [Eio.Fiber] beside
    anything else. *)

(** {1 Errors} *)

(** The type for the reasons a loop stopped early.

    A loop drives method calls rather than raw requests, so it fails in both of
    the ways RFC 8620 distinguishes. The request never happened, or it happened
    and one call in it failed. {!Jmap_eio.Client.error} covers both and
    {!Client_error} carries it unchanged.

    The one method error not reported here is [cannotCalculateChanges], which is
    an expected answer rather than a failure. {!val-changes} returns it as a
    {!type-delta}. The other constructors report consistency failures detected
    while joining several otherwise successful responses. *)
type error =
  | Client_error of Client.error
      (** The request did not complete, or it completed and the method call this
          loop drives did not answer with the response it should have. The first
          is a transport failure, a timeout or a request level error
          ({{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.6.1} RFC
            8620 Section 3.6.1}), the second a {!Client.Method_error} (Section
          3.6.2) or a {!Client.Json_error}. *)
  | No_mailbox_with_role of Jmap.Proto.Mailbox.role
      (** The account has no Mailbox with the role asked for. It is the one
          error a role lookup adds to the two above.
          {{:https://datatracker.ietf.org/doc/html/rfc8621#section-2} RFC 8621
           Section 2} gives a role to at most one Mailbox of an account, so the
          answer to a role lookup is at most one Mailbox and none at all is a
          failure rather than an empty result. See {!mailbox_id}. *)
  | Mailbox_missing_id of Jmap.Proto.Mailbox.role
      (** A Mailbox with the requested role was returned without its mandatory
          [id]. This is distinct from the account having no such Mailbox. *)
  | Query_state_changed of { previous : string; current : string }
      (** Two successive pages reported different [queryState] values, so they
          do not describe one stable ordered result. [previous] is the state of
          the first page and [current] the state that interrupted the walk. *)
  | Nonadvancing_query of { requested : int64; returned : int64 }
      (** A non-empty query page reported a [position] different from the one
          requested, or repeated the preceding page. Continuing could omit or
          duplicate results. *)
  | Page_fuel_exhausted of int
      (** Paging received a full page for every permitted request. The server
          may have an infinite or unexpectedly large result. No completion was
          inferred. *)
  | Nonadvancing_changes of string
      (** A [/changes] response said [hasMoreChanges] but returned a [newState]
          already seen in this drain. No state is available from which the
          remaining changes can be requested safely. *)
  | Mismatched_changes_state of { requested : string; returned : string }
      (** A [/changes] response reported an [oldState] other than the
          [sinceState] the round sent. [requested] is that [sinceState] and
          [returned] the [oldState] the server answered with. The delta covers
          some other interval, so folding it into the drain would report changes
          that did not happen since [requested]. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] prints a one line description of [e]. A
    {!No_mailbox_with_role} prints as [no mailbox has the role <role>], with the
    wire name of the role and any control characters escaped. *)

val error_to_string : error -> string
(** [error_to_string e] is {!pp_error} of [e] as a string. *)

exception Sync_error of error
(** The exception {!mailbox_id_exn} raises. {!Client.error} has no constructor
    for an account without the role asked for, so a loop of this module cannot
    report that as {!Client.Jmap_client_error} and raises this instead. It has a
    [Printexc] printer, so an uncaught one prints as {!pp_error} does. *)

(** {1 Server limits}

    The [urn:ietf:params:jmap:core] capability of the session object carries the
    limits a client must respect
    ({{:https://datatracker.ietf.org/doc/html/rfc8620#section-2} RFC 8620
      Section 2}). All of these read the session cached in the client, so they
    follow a session refresh. Session decoding requires a valid core capability;
    only a manually constructed {!Jmap.Proto.Session.t} can omit or malform it.
    The accessors expose the server's UnsignedInt values verbatim, including
    zero. *)

val core_limits : Client.t -> Jmap.Proto.Capability.Core.t option
(** [core_limits client] is the core capability object of the session [client]
    currently holds, or [None] if that session offers none. *)

val max_calls_in_request : Client.t -> int64 option
(** [max_calls_in_request client] is [maxCallsInRequest], "the maximum number of
    method calls the server will accept in a single request to the API endpoint"
    (RFC 8620 Section 2). Exceeding it is answered with the
    [urn:ietf:params:jmap:error:limit] request error of
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.6.1} Section
     3.6.1}.

    There is deliberately no [split_chain]. A chain is not a list of independent
    calls. {!Jmap.Chain} exists to let one call reference the result of an
    earlier one in the same request
    ({{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.7} RFC 8620
      Section 3.7}), and a back reference cannot cross a request boundary, so
    cutting a chain in two would silently change its meaning. {!Client.request}
    rejects an oversized chain before network access. {!calls_in_chain} and
    {!chain_fits} let an application choose a meaningful sequential fallback,
    and the loops in this module keep themselves to one method call per request.
*)

val max_objects_in_get : Client.t -> int64 option
(** [max_objects_in_get client] is [maxObjectsInGet], "the maximum number of
    objects that the client may request in a single [/get] type method call"
    (RFC 8620 Section 2). {!get_all} batches by it. *)

val max_objects_in_set : Client.t -> int64 option
(** [max_objects_in_set client] is [maxObjectsInSet], the maximum total number
    of objects a single [/set] may create, update and destroy (RFC 8620 Section
    2). Nothing here batches a [/set], since splitting one changes its
    atomicity, but a caller building a large [/set] needs the number. *)

val max_concurrent_requests : Client.t -> int64 option
(** [max_concurrent_requests client] is [maxConcurrentRequests], "the maximum
    number of concurrent requests the server will accept to the API endpoint"
    (RFC 8620 Section 2). {!get_all} runs no more batches at once than
    {!Client.concurrency_limits} derives from it. *)

val max_concurrent_upload : Client.t -> int64 option
(** [max_concurrent_upload client] is [maxConcurrentUpload], the maximum number
    of concurrent requests to the upload endpoint (RFC 8620 Section 2). *)

val default_max_objects_in_get : int64
(** [default_max_objects_in_get] is [4096L], the batch size {!get_all} uses when
    no valid core object is available in a manually constructed session. *)

val default_page_fuel : int
(** [default_page_fuel] is [1000], the maximum number of requests {!pages} and
    {!all_ids} make unless their caller selects another [fuel]. *)

val calls_in_chain : capabilities:string list -> 'a Jmap.Chain.t -> int
(** [calls_in_chain ~capabilities c] is the number of entries [c] would put in
    the [methodCalls] array of a request
    ({{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.2} RFC 8620
      Section 3.2}). It builds the chain and throws the request away. Chains are
    pure builders, so building one twice is free of consequence. *)

val chain_fits :
  Client.t -> ?capabilities:string list -> 'a Jmap.Chain.t -> bool
(** [chain_fits client ~capabilities c] is [true] when [c] has no more calls
    than the session's [maxCallsInRequest], and [true] when the server
    advertised no limit. [capabilities] defaults to
    {!Client.default_capabilities}. See {!max_calls_in_request} for why
    splitting is not offered. *)

(** {1 Paging a query}

    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-5.5} RFC 8620
     Section 5.5}: a [/query] takes a 0 based [position] and a [limit], and the
    response repeats the [position] it actually used and, "if the server set a
    limit or used a different limit than that given in the request", the [limit]
    it applied. A client that pages with its own limit rather than the server's
    skips results whenever the server is stricter than it asked for. *)

val pages :
  Client.t ->
  ?capabilities:string list ->
  ?page_size:int64 ->
  ?fuel:int ->
  (position:int64 ->
  limit:int64 ->
  ('q, Jmap.Proto.Method.query_response) Jmap.Chain.handle Jmap.Chain.t) ->
  (Jmap.Proto.Id.t list, error) result Seq.t
(** [pages client ~capabilities ~page_size ~fuel query] walks [query] page by
    page, lazily. Each time the sequence is advanced one request is sent.

    [query ~position ~limit] must build the [/query] to page, passing [position]
    and [limit] through to the builder with nothing else changing between pages.
    The filter and sort must be identical, since RFC 8620 Section 5.5 defines
    [position] as an offset into that sorted, filtered list. [page_size] is the
    limit asked for on the first page and defaults to [50L]. [capabilities] is
    the [using] array of every request and defaults to
    {!Client.default_capabilities}.

    [fuel] bounds the number of requests and defaults to {!default_page_fuel}.
    If every permitted request returns a full page, that page is yielded and the
    next sequence element is {!Page_fuel_exhausted}; no extra request is made. A
    caller that knows a larger result is legitimate may raise the bound
    explicitly.

    Every page is judged against the limit the server reported for it, which is
    [page_size] only until the server names one of its own. The walk stops when
    a page comes back shorter than that limit, or empty, so the last non empty
    page is yielded and the sequence ends without a further request.

    Every page must report the same [queryState] as the first. If it changes,
    the differing page is not yielded and the sequence ends with
    {!Query_state_changed}; joining pages from two ordered result snapshots
    could otherwise silently omit or duplicate records.

    A non-empty page must report the position requested and must not repeat the
    first id and length of the preceding page. Either case ends the sequence
    with {!Nonadvancing_query}; treating it as successful completion would
    silently return a truncated result. Ids are not deduplicated across
    otherwise valid pages, since RFC 8620 Section 5.5 offers no way to tell a
    moved record from a repeated one.

    The sequence ends after the first [Error], which is therefore always its
    last element. Earlier pages are still delivered.

    {[
    Sync.pages client ~page_size:100L (fun ~position ~limit ->
        Jmap.Chain.email_query ~account_id ~filter ~sort ~position ~limit ())
    |> Seq.iter (function
      | Ok ids -> List.iter index ids
      | Error e -> Fmt.epr "%s@." (Sync.error_to_string e))
    ]}
    The sequence is not persistent. It sends requests as it is forced, so
    forcing it twice queries twice, and the pages of the second walk may differ
    if the list changed underneath.

    @raise Invalid_argument
      if [page_size] is outside 1 to 2{^ 53}-1 or [fuel] is below 1. *)

val all_ids :
  Client.t ->
  ?capabilities:string list ->
  ?page_size:int64 ->
  ?fuel:int ->
  ?max:int ->
  (position:int64 ->
  limit:int64 ->
  ('q, Jmap.Proto.Method.query_response) Jmap.Chain.handle Jmap.Chain.t) ->
  (Jmap.Proto.Id.t list, error) result
(** [all_ids client ~capabilities ~page_size ~fuel ~max query] is {!pages} run
    to the end and concatenated, in query order. [capabilities], [page_size] and
    [fuel] are those of {!pages}.

    [max] caps the number of ids collected. Paging stops as soon as that many
    are in hand and the result is truncated to exactly [max]. It defaults to no
    cap, so the walk runs until the server stops returning results, which for an
    unfiltered [Email/query] is the whole account. Any error stops the walk and
    is returned in place of the ids collected so far.

    @raise Invalid_argument
      if [page_size] is outside 1 to 2{^ 53}-1, [fuel] is below 1, or [max] is
      negative. A zero [max] is valid and sends no request. *)

(** {1 Draining /changes}

    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-5.2} RFC 8620
     Section 5.2}: [Foo/changes] reports what happened between a [sinceState]
    and now as three id lists, capped at [maxChanges] with [hasMoreChanges] set
    when the delta was cut short. "The client should then call [Foo/changes]
    again, using the new state string" until the flag is false. *)

type changes = {
  created : Jmap.Proto.Id.t list;
      (** Ids created since [sinceState] and still existing. *)
  updated : Jmap.Proto.Id.t list;
      (** Ids that existed at [sinceState] and have changed since. *)
  destroyed : Jmap.Proto.Id.t list;
      (** Ids that existed at [sinceState] and no longer exist. *)
  new_state : string;
      (** The state reached, which is the [newState] of the last round and the
          [sinceState] to resume from next time. *)
  has_more : bool;
      (** [true] when the server still had changes to report when the drain
          stopped because the fuel ran out. A nonadvancing state is an error.
          The delta is then a prefix of the real one, and calling again with
          [~since:new_state] continues it. [false] means the drain reached the
          present, which is the only case in which a caller may wait for the
          next push before asking again. *)
}
(** The type for a whole delta between two states, with every round folded
    together.

    RFC 8620 Section 5.2 states the folding rules for a single response, and
    they hold just as well across the rounds of one drain, which is how this
    record is built. "If a record has been created AND updated since the old
    state, the server SHOULD just return the id in the created list, but MAY
    return it in the updated list as well." "If a record has been created AND
    destroyed since the old state, the server SHOULD remove the id from the
    response entirely", so an id in both cancels out and appears in none of the
    three lists. "If a record has been updated AND destroyed since the old
    state, the server SHOULD just return the id in the destroyed list."

    Each list is deduplicated and keeps the order in which the ids were first
    seen. *)

type mailbox_delta = {
  changes : changes;  (** The standard [/changes] lists. *)
  updated_properties : string list option;
      (** [updatedProperties] of
          {{:https://datatracker.ietf.org/doc/html/rfc8621#section-2.2} RFC 8621
           Section 2.2}. When only [totalEmails], [unreadEmails], [totalThreads]
          or [unreadThreads] changed it is the list of properties that may have
          changed, and [None] when the server cannot tell. Across a multi round
          drain it is the union of the rounds' lists, and [None] as soon as any
          one round said [null], a round that cannot rule out a full change not
          being narrowed by a later one. *)
}
(** The type for a [Mailbox/changes] delta, which RFC 8621 Section 2.2 gives one
    argument more than the standard response. Keeping it matters, since it is
    what makes the follow up [Mailbox/get] cheap. *)

type 'a delta =
  [ `Changes of 'a
  | `Cannot_calculate_changes
    (** RFC 8620 Section 5.2: "the server cannot calculate the changes from the
        state string given by the client, usually due to the client's state
        being too old". This is not an error. The client must "invalidate its
        cache and resync". Rebuild the cache and obtain its data state from
        [/get]. A [/query] returns a [queryState], which is only suitable for
        [/queryChanges] and cannot resume [/changes]. *) ]
(** The type for the answer to a drain, which is either a delta or the
    instruction to resync. *)

val changes :
  Client.t ->
  ?capabilities:string list ->
  since:string ->
  ?max_changes:int64 ->
  ?fuel:int ->
  (since_state:string ->
  max_changes:int64 ->
  ('c, Jmap.Proto.Method.changes_response) Jmap.Chain.handle Jmap.Chain.t) ->
  (changes delta, error) result
(** [changes client ~capabilities ~since ~max_changes ~fuel build] drains a
    [/changes] method from the state [since] to the present.

    [build ~since_state ~max_changes] builds one round. It is called once per
    round, with the [newState] of the previous round as [since_state], until the
    response's [hasMoreChanges] is false. [max_changes] is passed to every round
    and defaults to [256L]. [capabilities] is the [using] array of every request
    and defaults to {!Client.default_capabilities}.

    [fuel] bounds the number of rounds and defaults to [100]. A server is free
    to answer with one id at a time, so an unbounded drain is an unbounded
    number of requests. When the fuel runs out the changes gathered so far are
    returned with {!field-has_more} set, and since {!field-new_state} is the
    state they were gathered up to, calling again with [~since:new_state]
    resumes exactly where this stopped. A round that reports [hasMoreChanges]
    with a [newState] already seen in the drain cannot be resumed and is
    [Error (Nonadvancing_changes state)] rather than a partial success that
    would make the caller repeat the same loop for ever.

    {!field-has_more} is therefore the difference between the whole delta and as
    far as this got. A caller that loops drain, apply, wait for the next push
    must keep draining while it is set, or it lags by whatever was left behind.

    [`Cannot_calculate_changes] is returned as a value rather than an error. It
    is the documented way for a server to say the client's state is too old, and
    the client's answer is to resync rather than to retry.

    {[
    match
      Sync.changes client ~since:state (fun ~since_state ~max_changes ->
          Jmap.Chain.email_changes ~account_id ~since_state ~max_changes ())
    with
    | Ok (`Changes c) -> apply ~created:c.created ~destroyed:c.destroyed
    | Ok `Cannot_calculate_changes -> resync_from_scratch ()
    | Error e -> Fmt.epr "%s@." (Sync.error_to_string e)
    ]}

    @raise Invalid_argument
      if [max_changes] is outside 1 to 2{^ 53}-1, or if [fuel] is below 1. *)

val email_changes :
  Client.t ->
  ?capabilities:string list ->
  account_id:Jmap.Proto.Id.t ->
  since:string ->
  ?max_changes:int64 ->
  ?fuel:int ->
  unit ->
  (changes delta, error) result
(** [email_changes client ~capabilities ~account_id ~since ~max_changes ~fuel
     ()] drains [Email/changes]
    ({{:https://datatracker.ietf.org/doc/html/rfc8621#section-4.2} RFC 8621
      Section 4.2}) for [account_id]. [capabilities] defaults to core and mail,
    and the rest are those of {!val-changes}.

    @raise Invalid_argument as {!val-changes} does. *)

val thread_changes :
  Client.t ->
  ?capabilities:string list ->
  account_id:Jmap.Proto.Id.t ->
  since:string ->
  ?max_changes:int64 ->
  ?fuel:int ->
  unit ->
  (changes delta, error) result
(** [thread_changes client ~account_id ~since ()] drains [Thread/changes]
    ({{:https://datatracker.ietf.org/doc/html/rfc8621#section-3.2} RFC 8621
      Section 3.2}) on the terms of {!email_changes}.

    @raise Invalid_argument as {!val-changes} does. *)

val mailbox_changes :
  Client.t ->
  ?capabilities:string list ->
  account_id:Jmap.Proto.Id.t ->
  since:string ->
  ?max_changes:int64 ->
  ?fuel:int ->
  unit ->
  (mailbox_delta delta, error) result
(** [mailbox_changes client ~account_id ~since ()] drains [Mailbox/changes] on
    the terms of {!email_changes}, keeping the [updatedProperties] of RFC 8621
    Section 2.2. See {!mailbox_delta}.

    The one request form of this, feeding [updatedProperties] straight into a
    [Mailbox/get] in the same request as a [#properties] back reference, is
    {!Jmap.Chain.from_changes_updated_properties}, and is the better call when
    one round is enough. This is for the case where it is not.

    @raise Invalid_argument as {!val-changes} does. *)

(** {1 Batching a /get} *)

val get_all :
  Client.t ->
  ?capabilities:string list ->
  ?batch:int64 ->
  ?max_concurrent:int ->
  Jmap.Proto.Id.t list ->
  (ids:Jmap.Proto.Id.t list ->
  ('g, 'r Jmap.Proto.Method.get_response) Jmap.Chain.handle Jmap.Chain.t) ->
  ('r list * Jmap.Proto.Id.t list, error) result
(** [get_all client ~capabilities ~batch ~max_concurrent ids get] fetches every
    id in [ids], in as many [/get] calls as the server's limits require, and is
    the objects paired with the ids the server did not know.

    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-5.1} RFC 8620
     Section 5.1}: a [/get] answers [requestTooLarge] when "the number of ids
    requested by the client exceeds the maximum number the server is willing to
    process in a single method call". [batch] is the number of ids per call and
    defaults to the session's [maxObjectsInGet], or
    {!default_max_objects_in_get} only for a manually constructed session with
    no valid core object. An explicit [batch] may choose a smaller size but is
    capped at the advertised [maxObjectsInGet], so it cannot make the client
    violate the server limit. If the server advertises zero, an empty [ids] list
    still succeeds without a request, but a non-empty one returns a
    {!Client_error} carrying [Fetch.Invalid_request] before network access.

    [capabilities] is the [using] array of every request and defaults to
    {!Client.default_capabilities}.

    [max_concurrent] batches run at once and defaults to the API half of
    {!Client.concurrency_limits}, which is the session's [maxConcurrentRequests]
    (RFC 8620 Section 2) with that function's own default and clamp. A worker
    pool of that size assigns batches as earlier ones finish, so the client
    never has more requests in flight than the server said it would accept.

    Both returned lists are in input order, the objects of the first batch, then
    the second, and so on. Within a batch the order is the server's, since RFC
    8620 Section 5.1 does not promise a [/get] returns its objects in the order
    asked for. Ids the server did not know are collected from every batch's
    [notFound].

    An empty [ids] sends no request and is [Ok ([], [])]. The first batch to
    fail fails the whole call and no later batch is assigned. Batches already in
    flight are not cancelled, but their results are discarded.

    {[
    Sync.get_all client ids (fun ~ids ->
        Jmap.Chain.email_get ~account_id ~ids:(Jmap.Chain.ids ids)
          ~properties:[ `Id; `Subject; `Received_at ]
          ())
    ]}

    @raise Invalid_argument
      if [batch] is outside 1 to 2{^ 53}-1 or [max_concurrent] is not positive.
      Both are checked before [ids] is looked at. *)

(** {1 Finding a Mailbox by role} *)

val mailbox_with_role :
  Client.t ->
  ?capabilities:string list ->
  account_id:Jmap.Proto.Id.t ->
  Jmap.Proto.Mailbox.role ->
  (Jmap.Proto.Mailbox.t option, error) result
(** [mailbox_with_role client ~capabilities ~account_id role] is the Mailbox of
    [account_id] whose [role] is [role], or [None] if the account has none.
    [capabilities] defaults to core and mail.

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-2} RFC 8621 Section
     2} gives a role to at most one Mailbox of an account, so the answer is one
    object. Normally it is fetched in a single request, a [Mailbox/query]
    filtered on the role followed by a [Mailbox/get] naming the query's ids by
    the [#ids] back reference of
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.7} RFC 8620
     Section 3.7}. If the session's [maxCallsInRequest] is one, the same lookup
    uses two requests instead. Every property is asked for. A session limit of
    zero is returned as a local {!Client_error} without sending a request.

    {[
    match Sync.mailbox_with_role client ~account_id `Inbox with
    | Ok (Some inbox) -> index inbox
    | Ok None -> Fmt.epr "no Inbox@."
    | Error e -> Fmt.epr "%s@." (Sync.error_to_string e)
    ]}
    It costs one request, or two under [maxCallsInRequest=1]. A program that
    already holds the Mailboxes of the account finds the one in [role] by
    filtering that list on its [role] rather than by sending this. *)

val mailbox_id :
  Client.t ->
  ?capabilities:string list ->
  account_id:Jmap.Proto.Id.t ->
  Jmap.Proto.Mailbox.role ->
  (Jmap.Proto.Id.t, error) result
(** [mailbox_id client ~capabilities ~account_id role] is the id of the Mailbox
    of [account_id] whose [role] is [role], fetched by {!mailbox_with_role}.
    [capabilities] defaults to core and mail.

    RFC 8621 Section 2 gives a role to at most one Mailbox of an account, so the
    answer is one id. An account with no Mailbox in [role] is
    [Error (No_mailbox_with_role role)], which is where this parts company with
    {!mailbox_with_role}. A Mailbox returned without an [id] is instead
    [Error (Mailbox_missing_id role)], since the role exists but the response is
    malformed.

    {[
    match Sync.mailbox_id client ~account_id `Drafts with
    | Ok drafts -> file_message_in drafts
    | Error e -> Fmt.epr "%s@." (Sync.error_to_string e)
    ]}
    It costs one request, or two under [maxCallsInRequest=1]. A program that
    already holds the Mailboxes of the account finds the one in [role] by
    filtering that list on its [role] rather than by sending this. *)

val mailbox_id_exn :
  Client.t ->
  ?capabilities:string list ->
  account_id:Jmap.Proto.Id.t ->
  Jmap.Proto.Mailbox.role ->
  Jmap.Proto.Id.t
(** [mailbox_id_exn client ~account_id role] is {!mailbox_id} of [role]. It
    costs a request of its own, which a program that already holds the Mailboxes
    of the account spares itself by filtering that list on its [role].

    @raise Sync_error if the lookup fails. *)
