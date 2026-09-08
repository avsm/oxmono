(** send_queue — a per-room queue of events waiting to be sent.

    A request is enqueued with a transaction id, echoed into the timeline at
    once, and retried until it lands or is rejected for good. Each room has one
    FIFO with one request in flight, so events arrive in the order the user
    wrote them, and a request that fails unrecoverably becomes {!Wedged} and
    blocks its room until it is cancelled or unwedged rather than letting the
    ones behind it overtake it. The transaction id is generated once and reused
    on every attempt, so a retry after a lost reply cannot duplicate the
    message.

    Nothing here blocks or sleeps. [Matrix_eio.Send_queue] is the fiber that
    walks a queue, calls {!send_one} and waits {!retry_delay} in between.

    @see <https://spec.matrix.org/v1.11/client-server-api/#transaction-identifiers>
      Transaction Identifiers *)

(** {1 Requests} *)

(** What a queued request asks the server to do. *)
type kind =
  | Event of { event_type : string; content : Jsont.json }
      (** Send [content] as an event of [event_type]. *)
  | Reaction of { relates_to : Matrix_proto.Id.Event_id.t; key : string }
      (** Send an [m.reaction] annotating [relates_to] with [key]. *)
  | Redaction of {
      event_id : Matrix_proto.Id.Event_id.t;
      reason : string option;
    }  (** Redact [event_id], optionally giving [reason]. *)
  | Upload_request of {
      role : [ `Original | `Thumbnail ];
      content_type : string;
      filename : string option;
      data : string;
      encrypted_metadata : Encrypted_attachment.metadata option;
    }
      (** Upload clear bytes, or ciphertext with validated encrypted metadata.
          This node is not itself a room event and has no event ID result. *)
  | Attachment of {
      content : Jsont.json;
      original_upload : int;
      thumbnail_upload : int option;
    }
      (** Send one visible room message after the required upload and optional
          thumbnail upload resolve. The base [content] is retained for the local
          echo; generated [url]/[file] and thumbnail fields win over vendor
          extras at send time. *)

type upload_result =
  | Clear_upload of { mxc : Media.Mxc.t }
  | Encrypted_upload of {
      mxc : Media.Mxc.t;
      metadata : Encrypted_attachment.metadata;
    }
      (** The distinct result of a clear or encrypted media upload. Encrypted
          metadata excludes the final MXC URL and has already been validated. *)

type dependency_result =
  | Event_id of Matrix_proto.Id.Event_id.t
  | Upload of upload_result
      (** A resolved dependency. Event dependencies retain their server event
          ID; media dependencies retain their MXC and, when encrypted, metadata.
      *)

type progress = { current_bytes : int64; total_bytes : int64 }
(** Monotonic byte progress for an upload attempt. The terminal total is emitted
    only after the upload result has been durably propagated. *)

type attachment_upload
(** The bytes and metadata for one attachment upload. Encrypted uploads must be
    made with {!attachment_upload_encrypted}, which stores ciphertext only. *)

type attachment_edit_result =
  | Updated
  | Deferred
  | Already_sent
      (** Result of changing a queued attachment caption. [Deferred] means that
          the parent send was already in flight and the edit was durably
          recorded for after it resolves. *)

(** Where a request has got to. *)
type status =
  | Pending  (** Waiting its turn. *)
  | Sending  (** An attempt is in flight. *)
  | Sent of Matrix_proto.Id.Event_id.t  (** The server assigned this id. *)
  | Uploaded of upload_result  (** A media upload resolved with its MXC. *)
  | Wedged
      (** Rejected for good. {!last_error} says why while the process lives. *)
  | Cancelled  (** Dropped before it was sent. *)

type request
(** The type for queued sends. A request is mutable, and {!send_one} advances
    its {!val-status}. *)

val id : request -> int
(** [id r] is the queue's identifier for [r]. It is stable across retries, and
    across a restart when the queue was created with a store. Two queues over
    different stores hand out the same identifiers. *)

val room_id : request -> Matrix_proto.Id.Room_id.t
(** [room_id r] is the room [r] will be sent to. *)

val kind : request -> kind
(** [kind r] is what [r] asks the server to do. *)

val txn_id : request -> string
(** [txn_id r] is the transaction id every attempt at [r] carries, which is what
    makes a retry idempotent and what identifies [r]'s echo when the event comes
    back from [/sync]. *)

val status : request -> status
(** [status r] is where [r] has got to. *)

val attempts : request -> int
(** [attempts r] is how many times [r] has been sent, including the attempt in
    flight. *)

val created_at : request -> Matrix_proto.Event.Timestamp.t
(** [created_at r] is when [r] was enqueued, by the local clock. *)

val last_error : request -> Error.t option
(** [last_error r] is the error of [r]'s last failed attempt, and [None] before
    the first failure, after a success and after {!unwedge}. It is not
    persisted, so a request restored from a store as {!Wedged} has none. *)

(** {1 The queue} *)

type t
(** The type for send queues. One queue serves every room of one account. *)

val create :
  random:Random.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?store:Store.t ->
  ?media_store:Media_store.t ->
  ?media_owner:string ->
  ?max_retries:int ->
  ?base_delay_ms:int ->
  ?max_delay_ms:int ->
  unit ->
  t
(** [create ~random ~user_id ()] is an empty queue.

    [random] supplies the transaction ids ({!Random.txn_id}). [user_id] is the
    sender {!local_echo} attributes an echo to. [store], when given, is both
    loaded from at creation, so requests left unsent by a crash come back, and
    written to by {!save}; it defaults to holding the queue in memory only.
    [media_store], when given, stores upload bytes under deterministic local MXC
    keys before the queue record is persisted. Cache-backed retries and restores
    read only that exact key; without it, uploads retain their historical inline
    payload. Legacy inline records are imported into the media store on restore
    when possible. A persistent store derives a stable per-directory media owner
    for orphan reconciliation; [media_owner] can provide an explicit stable
    identity. Without either identity, local-media reconciliation is disabled.
    Two queues sharing one persistent store directory are unsupported and must
    use the store's existing locking discipline. [max_retries] defaults to [5],
    [base_delay_ms] to [500] and [max_delay_ms] to [60_000]. *)

val user_id : t -> Matrix_proto.Id.User_id.t
(** [user_id t] is the account [t] sends as. *)

val store : t -> Store.t option
(** [store t] is the store [t] was created with, and [None] if it has none. *)

(** {2 Enqueueing} *)

val enqueue :
  ?depends_on:int list ->
  ?extra_content:Jsont.json ->
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  kind ->
  request
(** [enqueue t ~room_id kind] appends a request to [room_id]'s FIFO, allocating
    its transaction id, and is that request. Every [depends_on] ID is local to
    [t] and must name an earlier request in the same room; duplicates, missing
    IDs and cross-room edges raise [Invalid_argument]. The request becomes
    sendable only after every parent has resolved. Event and attachment content,
    and [extra_content] when present, must be JSON objects; invalid content
    raises [Invalid_argument] before the queue is changed. *)

val send_message :
  ?depends_on:int list ->
  ?extra_content:Jsont.json ->
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:string ->
  content:Jsont.json ->
  request
(** [send_message t ~room_id ~event_type ~content] enqueues an {!Event}.
    [content] must be a JSON object. *)

val upload :
  ?depends_on:int list ->
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  role:[ `Original | `Thumbnail ] ->
  content_type:string ->
  data:string ->
  ?filename:string ->
  unit ->
  request
(** [upload] queues a clear upload whose persisted payload is exactly [data]. *)

val upload_encrypted :
  ?depends_on:int list ->
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  role:[ `Original | `Thumbnail ] ->
  content_type:string ->
  encrypted:Encrypted_attachment.encrypted ->
  ?filename:string ->
  unit ->
  request
(** [upload_encrypted] queues the ciphertext and validated metadata from
    [encrypted]; plaintext is never stored in the request. *)

val attachment_upload :
  content_type:string ->
  data:string ->
  ?filename:string ->
  unit ->
  attachment_upload
(** [attachment_upload] describes a clear upload for {!send_attachment}. *)

val attachment_upload_encrypted :
  content_type:string ->
  encrypted:Encrypted_attachment.encrypted ->
  ?filename:string ->
  unit ->
  attachment_upload
(** [attachment_upload_encrypted] describes an encrypted upload using only its
    ciphertext and validated metadata. *)

val send_attachment :
  ?depends_on:int list ->
  ?extra_content:Jsont.json ->
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  base_content:Jsont.json ->
  original:attachment_upload ->
  ?thumbnail:attachment_upload ->
  unit ->
  request
(** [send_attachment] queues the original upload, optional thumbnail upload and
    one dependent visible attachment event. Upload nodes are hidden from the
    event cache; [base_content] is used for the single local echo.
    [base_content] and [extra_content], when present, must be JSON objects;
    invalid input raises before any upload node is queued. *)

val edit_attachment_caption :
  ?formatted_body:string ->
  ?format:string ->
  ?mentions:Jsont.json ->
  t ->
  request ->
  caption:string option ->
  (attachment_edit_result, Error.t) result
(** [edit_attachment_caption t request ~caption] updates a queued media caption
    without rebuilding its upload graph. The original transaction ID is retained
    while the attachment is local; an in-flight parent records one stable
    replacement transaction for after the parent resolves. [None] restores the
    logical filename as the body. Missing formatted body or mentions clears
    those fields. *)

val send_text :
  ?extra_content:Jsont.json ->
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  body:string ->
  request
(** [send_text t ~room_id ~body] enqueues an [m.room.message] with msgtype
    [m.text] and [body]. [extra_content] appends vendor fields without
    overriding typed fields and is persisted with the request. *)

val send_edit :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  new_body:string ->
  ?formatted_body:string ->
  ?format:string ->
  unit ->
  request
(** [send_edit t ~room_id ~event_id ~new_body ()] enqueues an [m.room.message]
    replacing [event_id]. Its fallback [body] is [new_body] prefixed by ["* "],
    and [m.new_content] carries the unprefixed replacement. [formatted_body]
    adds the formatted replacement and its similarly prefixed fallback; [format]
    defaults to ["org.matrix.custom.html"] and is ignored without a
    [formatted_body]. The request is an ordinary {!Event}, so it has the same
    optimistic echo, persistence and retry behaviour as other queued messages.
*)

val send_reaction :
  ?extra_content:Jsont.json ->
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  relates_to:Matrix_proto.Id.Event_id.t ->
  key:string ->
  request
(** [send_reaction t ~room_id ~relates_to ~key] enqueues a {!Reaction}.
    [extra_content] appends vendor fields without overriding the typed relation
    and is persisted with the request. *)

val send_redaction :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?reason:string ->
  unit ->
  request
(** [send_redaction t ~room_id ~event_id ()] enqueues a {!Redaction}. [reason]
    defaults to none. *)

(** {2 Inspecting} *)

val requests : t -> request list
(** [requests t] is everything still queued, across all rooms, in insertion
    order. A request leaves the queue when it is sent or cancelled. *)

val room_requests : t -> Matrix_proto.Id.Room_id.t -> request list
(** [room_requests t room_id] is one room's FIFO, oldest first. *)

val dependencies : request -> int list
(** Unresolved parent request IDs. *)

val resolved_dependencies : request -> (int * Matrix_proto.Id.Event_id.t) list
(** The event-only compatibility projection. Use {!dependency_results} for media
    upload results as well. *)

val dependency_results : request -> (int * dependency_result) list
(** [dependency_results r] includes every resolved dependency, retaining its
    typed event or media result. [resolved_dependencies] remains the event-only
    compatibility projection. *)

val rooms : t -> Matrix_proto.Id.Room_id.t list
(** [rooms t] is the rooms with at least one queued request. *)

val next : t -> Matrix_proto.Id.Room_id.t -> request option
(** [next t room_id] is the head of [room_id]'s queue if it can be sent now,
    which means the queue is enabled, the head is {!Pending} and all of its
    dependencies have resolved. It is [None] when the room is idle, disabled, or
    its head is blocked or {!Wedged}. *)

val pending_count : t -> int
(** [pending_count t] is how many requests are {!Pending} across all rooms. *)

val is_empty : t -> bool
(** [is_empty t] is [true] when nothing is queued in any room. *)

(** {2 Control} *)

val cancel : t -> request -> [ `Cancelled | `Already_sent | `In_flight ]
(** [cancel t r] drops [r] and all of its transitive dependants from the queue.
    It is [`Cancelled] when [r] was {!Pending}, {!Wedged} or already cancelled.
    It is [`Already_sent] when the server has given [r] an event id, and
    [`In_flight] when an attempt has not yet answered. Undoing a send that is
    still in flight but then lands is handled automatically by a durable
    compensating redaction; use {!cancel_with_reason} to supply its optional
    reason. Cancelling an attachment while one of its private uploads is in
    flight records cancellation on that upload and removes it when the attempt
    returns; the upload callback has no separate cancellation token. An
    already-sent request retains the [`Already_sent] result. *)

val cancel_with_reason :
  ?reason:string -> t -> request -> [ `Cancelled | `Already_sent | `In_flight ]
(** [cancel_with_reason ?reason t r] is [cancel t r], except that a request
    whose send is in flight records a durable intent to redact the event with
    [reason] if that attempt succeeds. Repeated calls are idempotent; a failed
    attempt drops the local request and its dependants without sending a
    redaction. *)

val forget_room : t -> Matrix_proto.Id.Room_id.t -> unit
(** [forget_room t room_id] removes every queued request for [room_id] and
    persists that deletion. Pending requests are cancelled and their local
    upload bytes are removed. A transport already in flight is detached until
    its callback returns; it is then cancelled without a compensating redaction,
    and its local upload bytes are removed. New requests enqueued after this
    call are independent and are not removed. *)

val unwedge : t -> request -> unit
(** [unwedge t r] returns a {!Wedged} request to {!Pending}, with its attempt
    count and last error cleared, so the queue moves again. A request in any
    other state is left alone. *)

val enabled : t -> bool
(** [enabled t] is [false] when every room's sending is held. *)

val set_enabled : t -> enabled:bool -> unit
(** [set_enabled t ~enabled] holds or releases sending for every room. A held
    queue still accepts new requests. *)

val room_enabled : t -> Matrix_proto.Id.Room_id.t -> bool
(** [room_enabled t room_id] is [true] when [t] is enabled and [room_id] has not
    been held on its own. *)

val set_room_enabled : t -> Matrix_proto.Id.Room_id.t -> enabled:bool -> unit
(** [set_room_enabled t room_id ~enabled] holds or releases sending for one
    room. *)

val on_change : t -> (request -> unit) -> unit
(** [on_change t f] registers [f] to run whenever a request's status changes.
    Callbacks run in the order they were registered and cannot be removed, so a
    caller whose lifetime is shorter than the queue's must guard [f] itself. *)

val on_progress : t -> (request -> progress -> unit) -> unit
(** [on_progress t f] registers [f] for monotonic upload byte progress. *)

(** {1 Sending} *)

(** What one attempt came to. *)
type outcome =
  | Sent_ok of Matrix_proto.Id.Event_id.t  (** The server assigned this id. *)
  | Uploaded_ok of upload_result  (** The media upload resolved with an MXC. *)
  | Retry_in of float  (** Seconds to wait before the next attempt. *)
  | Failed of Error.t  (** Unrecoverable, and the request is now {!Wedged}. *)

val send_one :
  t ->
  ?send:(Client.t -> request -> (Matrix_proto.Id.Event_id.t, Error.t) result) ->
  ?upload:
    (?on_progress:(int64 -> unit) ->
    Client.t ->
    request ->
    (upload_result, Error.t) result) ->
  ?on_progress:(request -> progress -> unit) ->
  Client.t ->
  request ->
  outcome
(** [send_one t client r] makes one attempt at [r] and does not block.

    An event success returns {!Sent_ok} and leaves [r] with status {!Sent}; an
    upload success returns {!Uploaded_ok} and resolves dependants with its typed
    {!upload_result} before removing [r] from the queue. On failure the error is
    classified by {!classify}, which either leaves [r] at the head of its queue
    as {!Pending} or wedges it.

    [send] replaces the HTTP call and defaults to the endpoint {!val-payload}
    describes. An encrypted room passes {!send_as} partially applied to the
    encrypted content, which keeps the queue's ordering, retry classification
    and transaction id unchanged. [upload] replaces the media call and receives
    an optional byte-count callback; it defaults to the streaming media
    implementation. The per-call [on_progress] callback and callbacks registered
    with {!on_progress} receive monotonic upload progress, with the terminal
    total emitted only after the result is persisted. *)

val classify : t -> request -> Error.t -> outcome
(** [classify t r e] is what {!send_one} does about [e].

    A transport failure, a [429] or [M_LIMIT_EXCEEDED] honouring its
    [retry_after_ms], an [M_UNKNOWN] on its first failed attempt, and any 5xx
    are {!Retry_in}, unless [r] has already used its [max_retries], which makes
    them {!Failed}. An explicit [retry_after_ms] is authoritative and is not
    jittered. Locally computed exponential delays have bounded random jitter in
    [[0.5, 1.5)]] and remain capped at [max_delay_ms]. Everything else,
    including [M_FORBIDDEN], [M_UNKNOWN_TOKEN], any other 4xx and a JSON
    failure, is {!Failed}. It never returns {!Sent_ok}. *)

val retry_delay : t -> request -> float
(** [retry_delay t r] is the exponential backoff before [r]'s next attempt, in
    seconds. It is [base_delay_ms * 2 ^ (attempts - 1)] with a random factor in
    [[0.5, 1.5)]], capped at [max_delay_ms]. *)

(** {2 The request on the wire} *)

(** What a request puts on the wire, and at which endpoint. *)
type payload =
  | Send of { event_type : string; content : Jsont.json }
      (** [PUT /rooms/{roomId}/send/{eventType}/{txnId}] with [content] as the
          body. *)
  | Redact of { event_id : Matrix_proto.Id.Event_id.t; reason : string option }
      (** [PUT /rooms/{roomId}/redact/{eventId}/{txnId}], whose body carries
          only [reason]. *)
  | Upload_payload of {
      role : [ `Original | `Thumbnail ];
      content_type : string;
      filename : string option;
      data : string;
      encrypted_metadata : Encrypted_attachment.metadata option;
    }  (** A media upload to [/_matrix/media/v3/upload]. *)

val payload : request -> payload
(** [payload r] is what [r] would send. An {!Event} sends the caller's own type
    and content, a {!Reaction} the [m.reaction] this module builds, and a
    {!Redaction} names its target in the path rather than the body, and an
    {!Upload_request} carries the payload for the media upload, and an
    {!constructor-Attachment} exposes its base event content until its upload
    results are substituted for sending. This is total for requests admitted by
    this module or restored from its persistence format.

    Only a {!Send} can be wrapped in [m.room.encrypted]. A {!Redact} is a
    request against a different endpoint and is never encrypted. *)

val content_for_send : request -> (string * Jsont.json, Error.t) result
(** [content_for_send r] prepares the network event. Attachment upload results
    are substituted here; missing or malformed results return an error while
    [local_echo] continues to expose the base content. *)

val send_as :
  event_type:string ->
  content:Jsont.json ->
  Client.t ->
  request ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_as ~event_type ~content client r] sends [content] as [event_type] to
    [r]'s room, under [r]'s transaction id, and is the event id the server gave
    it. Partially applied to the encrypted form of {!val-payload}, it is what
    {!send_one}'s [send] takes for an encrypted room. *)

(** {1 Local echo} *)

val local_echo : t -> request -> Matrix_proto.Event.Raw_event.t
(** [local_echo t r] is a synthetic event for [r], so a timeline can show it
    before the server has seen it. It has no [event_id], since only the server
    assigns one; its sender is [t]'s user, its timestamp is {!created_at}, its
    [unsigned] carries {!txn_id} and its content is what [r] will send.

    A redaction is the one request whose echo differs from the body it sends.
    [PUT /redact] names its target in the path, but the event the server makes
    carries it as [redacts] in the content, so the echo does too, or a reader
    could not tell what it redacts.

    The real event arrives from [/sync] with the same [unsigned.transaction_id],
    which is how a timeline recognises its own echo. See {!Timeline.add}. *)

(** {1 Persistence} *)

val save : t -> unit
(** [save t] writes the queue into the {!Store} given at {!create}, under the
    ["send_queue"] slot. It is a no-op with no store. Nothing reaches the
    filesystem until {!Store.flush}. Requests that have been sent or cancelled
    are not saved, and a {!Wedged} one is saved as wedged but without its error.
*)
