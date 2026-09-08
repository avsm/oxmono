(** Bot runtime resources and asynchronous sending.

    A context owns one sequential sender under a caller-supplied switch. Room
    values and send handles retain their context. The caller must keep the
    switch alive while using them. *)

type clock = float Eio.Time.clock_ty Eio.Resource.t
(** The type for wall clocks used by send retries and timeouts. *)

type identity = Context_runtime.identity = {
  user_id : Zulip.Id.User.t;
  email : string;
  full_name : string;
}
(** The type for the authenticated bot identity. Realm-user observations can
    update [email] and [full_name]. *)

type send =
  destination:Zulip.Message.destination ->
  content:string ->
  (Zulip.Id.Message.t, Zulip_eio.Error.t) result
(** The type for message send adapters. *)

type t = Context_runtime.t
(** The type for bot runtime contexts. *)

val v :
  sw:Eio.Switch.t ->
  client:Zulip_eio.Client.t ->
  identity:identity ->
  ?clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  ?plugin_store:Plugin_store.t ->
  ?is_bot:(Zulip.Id.User.t -> bool) ->
  ?send:send ->
  ?send_depth:int ->
  unit ->
  t
(** [v ~sw ~client ~identity ~clock ~plugin_store ~is_bot ~send ~send_depth ()]
    is a context for [client] and [identity]. It starts one sequential sender
    under [sw]. [clock] defaults to the client's transport clock. [plugin_store]
    defaults to a new in-memory store. [is_bot] defaults to a predicate that
    returns [false]. [send] defaults to sending through [client]. [send_depth]
    defaults to [128] and bounds requests waiting for the sender.

    Releasing [sw] cancels the sender. It settles queued handles as
    {!Sent.Cancelled} and sending handles as {!Sent.Indeterminate} with no
    error. Handles that are already terminal retain their outcomes.

    @raise Stdlib.exception-Invalid_argument
      if [send_depth] is not positive or if neither [clock] nor the client
      transport provides a clock. *)

val connect :
  sw:Eio.Switch.t ->
  env:
    < net : _ Eio.Net.t
    ; clock : float Eio.Time.clock_ty Eio.Resource.t
    ; mono_clock : _ Eio.Time.Mono.t
    ; secure_random : _ Eio.Flow.source
    ; fs : Eio.Fs.dir_ty Eio.Path.t
    ; .. > ->
  profile:string ->
  ?site:string ->
  ?email:string ->
  ?api_key:string ->
  ?allow_insecure:bool ->
  ?transport:Zulip_eio.Transport.t ->
  unit ->
  (t, Zulip_eio.Error.t) result
(** [connect ~sw ~env ~profile ~site ~email ~api_key ~allow_insecure ~transport
     ()] is a context authenticated from [profile]. [site], [email], and
    [api_key] override values resolved from the profile when supplied.
    [allow_insecure] defaults to [false]. [transport] defaults to one created
    from [env]. The context uses [env]'s clock and a private file-backed plugin
    store in the profile data directory.

    Profile resolution, client creation, identity lookup, and plugin-store
    failures are returned as [Error]. Fiber cancellation propagates. The caller
    owns [sw] and must keep it alive while using the result. *)

val client : t -> Zulip_eio.Client.t
(** [client context] is the Zulip client used by [context]. *)

val identity : t -> identity
(** [identity context] is the latest observed identity of [context]. *)

val user_id : t -> Zulip.Id.User.t
(** [user_id context] is the authenticated user identifier of [context]. *)

val clock : t -> clock
(** [clock context] is the wall clock used by [context]. *)

val plugin_store : t -> Plugin_store.t
(** [plugin_store context] is the plugin store used by [context]. *)

val is_bot : t -> Zulip.Id.User.t -> bool
(** [is_bot context user_id] is [true] if [user_id] is known as a bot. Cached
    realm-user observations take precedence over the adapter supplied to {!v}.
    The cache grows with observed users and has no eviction bound. *)

val apply_initial_state :
  t -> Zulip_eio.Initial_state.t -> (unit, Zulip_eio.Error.t) result
(** [apply_initial_state context state] adds user classifications from [state]
    to [context] and refreshes its identity when present. Event-queue adapters
    call it after each successful registration and before delivering live
    events. Missing optional user lists are ignored. All three optional user
    lists are decoded before any update is applied, so any decoding error leaves
    the context unchanged. *)

val observe_event : t -> Zulip.Event.t -> unit
(** [observe_event context event] applies a supported realm-user update to
    [context]. Event adapters call it in event order before exposing later
    events that depend on the updated identity or bot classification.
    Unsupported and malformed events have no effect. *)

val enqueue :
  t -> destination:Zulip.Message.destination -> content:string -> Sent.t
(** [enqueue context ~destination ~content] admits a request to the sequential
    sender of [context] and is its {!Sent.t} handle. It blocks while the bounded
    send queue is full and returns only after admission or context shutdown.
    Admission does not mean that Zulip has received the message. Fiber
    cancellation before admission propagates and cancels the new handle.

    The sender retries rate-limit responses up to three times. Releasing the
    context switch wakes blocked admissions and returns a handle settled as
    {!Sent.Cancelled}. *)
