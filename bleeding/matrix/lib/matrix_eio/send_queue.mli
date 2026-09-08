(** send_queue — the fibers that drain the offline send queue.

    {!Matrix_client.Send_queue} is the state machine and the single-attempt
    step, and documents every type and value re-exported here. {!start} adds the
    fibers that walk each room's FIFO, send the head, and sleep for the backoff
    the classification asks for.

    {[
    Eio.Switch.run @@ fun sw ->
    let queue =
      Matrix_eio.Send_queue.create
        ~random:(Matrix_client.Client.random (Matrix_eio.Client.base client))
        ~user_id:(Matrix_eio.Client.user_id client)
        ~store ()
    in
    Matrix_eio.Send_queue.start ~sw ~clock client queue;
    ignore (Matrix_eio.Send_queue.send_text queue ~room_id ~body:"hi")
    ]} *)

(** {1 Requests} *)

(** What a queued request asks the server to do. *)
type kind = Matrix_client.Send_queue.kind =
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
      encrypted_metadata : Matrix_client.Encrypted_attachment.metadata option;
    }
  | Attachment of {
      content : Jsont.json;
      original_upload : int;
      thumbnail_upload : int option;
    }

type attachment_upload = Matrix_client.Send_queue.attachment_upload

type attachment_edit_result = Matrix_client.Send_queue.attachment_edit_result =
  | Updated
  | Deferred
  | Already_sent

type upload_result = Matrix_client.Send_queue.upload_result =
  | Clear_upload of { mxc : Matrix_client.Media.Mxc.t }
  | Encrypted_upload of {
      mxc : Matrix_client.Media.Mxc.t;
      metadata : Matrix_client.Encrypted_attachment.metadata;
    }

type dependency_result = Matrix_client.Send_queue.dependency_result =
  | Event_id of Matrix_proto.Id.Event_id.t
  | Upload of upload_result

type progress = Matrix_client.Send_queue.progress = {
  current_bytes : int64;
  total_bytes : int64;
}

(** Where a request has got to. *)
type status = Matrix_client.Send_queue.status =
  | Pending  (** Waiting its turn. *)
  | Sending  (** An attempt is in flight. *)
  | Sent of Matrix_proto.Id.Event_id.t  (** The server assigned this id. *)
  | Uploaded of upload_result  (** A media upload resolved with its MXC. *)
  | Wedged
      (** Rejected for good. {!last_error} says why while the process lives. *)
  | Cancelled  (** Dropped before it was sent. *)

type request = Matrix_client.Send_queue.request
(** The type for queued sends. A request is mutable, and {!send_one} advances
    its {!val-status}. *)

(** {1 The queue} *)

type t = Matrix_client.Send_queue.t
(** The type for send queues. One queue serves every room of one account. *)

val create :
  random:Matrix_client.Random.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?store:Matrix_client.Store.t ->
  ?media_store:Matrix_client.Media_store.t ->
  ?media_owner:string ->
  ?max_retries:int ->
  ?base_delay_ms:int ->
  ?max_delay_ms:int ->
  unit ->
  t
(** [create ~random ~user_id ()] is an empty queue. [store] is both loaded from
    at creation, so requests left unsent by a crash come back, and written to by
    {!save}; it defaults to holding the queue in memory only. [media_owner] can
    supply the stable identity used for local-media orphan reconciliation.
    [max_retries] defaults to [5], [base_delay_ms] to [500] and [max_delay_ms]
    to [60_000]. *)

(** {2 Enqueueing} *)

val enqueue :
  ?depends_on:int list ->
  ?extra_content:Jsont.json ->
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  kind ->
  request
(** [enqueue t ~room_id kind] appends a request to [room_id]'s FIFO, allocating
    its transaction id, and is that request. [depends_on] contains queue-local
    IDs of earlier requests in the same room. *)

val send_message :
  ?depends_on:int list ->
  ?extra_content:Jsont.json ->
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:string ->
  content:Jsont.json ->
  request
(** [send_message t ~room_id ~event_type ~content] enqueues an {!Event}. *)

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

val upload_encrypted :
  ?depends_on:int list ->
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  role:[ `Original | `Thumbnail ] ->
  content_type:string ->
  encrypted:Matrix_client.Encrypted_attachment.encrypted ->
  ?filename:string ->
  unit ->
  request

val attachment_upload :
  content_type:string ->
  data:string ->
  ?filename:string ->
  unit ->
  attachment_upload

val attachment_upload_encrypted :
  content_type:string ->
  encrypted:Matrix_client.Encrypted_attachment.encrypted ->
  ?filename:string ->
  unit ->
  attachment_upload

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

val edit_attachment_caption :
  ?formatted_body:string ->
  ?format:string ->
  ?mentions:Jsont.json ->
  t ->
  request ->
  caption:string option ->
  (attachment_edit_result, Matrix_client.Error.t) result

val send_text :
  ?extra_content:Jsont.json ->
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  body:string ->
  request
(** [send_text t ~room_id ~body] enqueues an [m.room.message] with msgtype
    [m.text] and [body]. *)

val send_edit :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  new_body:string ->
  ?formatted_body:string ->
  ?format:string ->
  unit ->
  request
(** [send_edit t ~room_id ~event_id ~new_body ()] is the queued counterpart of
    {!Matrix_client.Send_queue.send_edit}. *)

val send_reaction :
  ?extra_content:Jsont.json ->
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  relates_to:Matrix_proto.Id.Event_id.t ->
  key:string ->
  request
(** [send_reaction t ~room_id ~relates_to ~key] enqueues a {!Reaction} and
    persists [extra_content] without letting it override typed relation fields.
*)

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

val id : request -> int
(** [id r] is the queue's identifier for [r]. It is stable across retries, and
    across a restart when the queue was created with a store. *)

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
(** [attempts r] is how many times [r] has been sent, the attempt in flight
    included. *)

val last_error : request -> Matrix_client.Error.t option
(** [last_error r] is the error of [r]'s last failed attempt, and [None] before
    the first failure, after a success and after {!unwedge}. It is not
    persisted. *)

val requests : t -> request list
(** [requests t] is everything still queued, across all rooms, in insertion
    order. A request leaves the queue when it is sent or cancelled. *)

val room_requests : t -> Matrix_proto.Id.Room_id.t -> request list
(** [room_requests t room_id] is one room's FIFO, oldest first. *)

val dependencies : request -> int list
(** Unresolved parent request IDs, local to this queue instance. *)

val resolved_dependencies : request -> (int * Matrix_proto.Id.Event_id.t) list
(** The event-only compatibility projection. Use {!dependency_results} for media
    upload results as well. *)

val dependency_results : request -> (int * dependency_result) list
(** Every resolved dependency, retaining the typed event or media result. *)

val rooms : t -> Matrix_proto.Id.Room_id.t list
(** [rooms t] is the rooms with at least one queued request. *)

val next : t -> Matrix_proto.Id.Room_id.t -> request option
(** [next t room_id] is the head of [room_id]'s queue if it can be sent now,
    which means the queue is enabled and the head is {!Pending}. It is [None]
    when the room is idle, disabled, or its head is {!Wedged}. *)

val pending_count : t -> int
(** [pending_count t] is how many requests are {!Pending} across all rooms. *)

val is_empty : t -> bool
(** [is_empty t] is [true] when nothing is queued in any room. *)

val local_echo : t -> request -> Matrix_proto.Event.Raw_event.t
(** [local_echo t r] is a synthetic event for [r], so a timeline can show it
    before the server has seen it. See {!Matrix_client.Send_queue.local_echo}.
*)

(** {2 Control} *)

val cancel : t -> request -> [ `Cancelled | `Already_sent | `In_flight ]
(** [cancel t r] drops [r] from its queue. It is [`Cancelled] when [r] was
    {!Pending}, {!Wedged} or already cancelled, [`Already_sent] when the server
    has given [r] an event id, and [`In_flight] when an attempt has not yet
    answered. If an in-flight send lands, its event is automatically followed by
    a compensating redaction; {!cancel_with_reason} supplies its reason. *)

val cancel_with_reason :
  ?reason:string -> t -> request -> [ `Cancelled | `Already_sent | `In_flight ]
(** [cancel_with_reason ?reason t r] records a durable compensating redaction
    when [r] is being sent and that attempt succeeds. Repeated calls are
    idempotent; a failed attempt drops the local event instead. *)

val forget_room : t -> Matrix_proto.Id.Room_id.t -> unit
(** [forget_room t room_id] removes every queued request for [room_id].
    In-flight requests are detached until their transport callback returns and
    never produce a compensating redaction. *)

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
    caller whose lifetime is shorter than the queue's must guard [f] itself.
    {!start} registers one of its own to wake the sending fibers. *)

val on_progress : t -> (request -> progress -> unit) -> unit
(** [on_progress t f] registers [f] for monotonic upload byte progress. *)

val retry_delay : t -> request -> float
(** [retry_delay t r] is the exponential backoff before [r]'s next attempt, in
    seconds. *)

val save : t -> unit
(** [save t] writes the queue into the store given at {!create}. It is a no-op
    with no store, and nothing reaches the filesystem until
    {!Matrix_client.Store.flush}. *)

(** {1 Sending} *)

(** What one attempt came to. *)
type outcome = Matrix_client.Send_queue.outcome =
  | Sent_ok of Matrix_proto.Id.Event_id.t  (** The server assigned this id. *)
  | Uploaded_ok of upload_result  (** The media upload resolved with an MXC. *)
  | Retry_in of float  (** Seconds to wait before the next attempt. *)
  | Failed of Matrix_client.Error.t
      (** Unrecoverable, and the request is now {!Wedged}. *)

(** What a request puts on the wire, and at which endpoint. *)
type payload = Matrix_client.Send_queue.payload =
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
      encrypted_metadata : Matrix_client.Encrypted_attachment.metadata option;
    }

val payload : request -> payload
(** [payload r] is what [r] would send. Only a {!Send} can be wrapped in
    [m.room.encrypted]; a {!Redact} is a request against a different endpoint
    and is never encrypted. *)

val send_one :
  ?send:
    (Matrix_client.Client.t ->
    request ->
    (Matrix_proto.Id.Event_id.t, Matrix_client.Error.t) result) ->
  ?upload:
    (?on_progress:(int64 -> unit) ->
    Matrix_client.Client.t ->
    request ->
    (upload_result, Matrix_client.Error.t) result) ->
  ?on_progress:(request -> progress -> unit) ->
  t ->
  Client.t ->
  request ->
  outcome
(** [send_one queue client request] makes one attempt, in the calling fiber. It
    does not raise, a failure being classified into the {!type-outcome}.

    [send] replaces the HTTP call and defaults to the endpoint {!val-payload}
    describes. It is handed the result-returning client under [client], since it
    answers with a result rather than raising. {!sender_for} is what {!start}
    passes. *)

val sender_for :
  ?encryption:Encryption.t ->
  ?members:(Matrix_proto.Id.Room_id.t -> Matrix_proto.Id.User_id.t list) ->
  request ->
  (Matrix_client.Client.t ->
  request ->
  (Matrix_proto.Id.Event_id.t, Matrix_client.Error.t) result)
  option
(** [sender_for request] is the [send] to give {!send_one}. It is the encrypting
    sender when an encryption machine is attached, [members] is given and the
    room is encrypted, and [None] otherwise, which is the built-in plain send.

    [members] is everybody who should be able to read the room, and is normally
    {!Sync_service.members} applied to the running service. Leaving it out for
    an encrypted room sends in the clear, and logs a warning saying so. *)

(** {1 Running} *)

val start :
  sw:Eio.Switch.t ->
  clock:float Eio.Time.clock_ty Eio.Std.r ->
  ?encryption:Encryption.t ->
  ?members:(Matrix_proto.Id.Room_id.t -> Matrix_proto.Id.User_id.t list) ->
  Client.t ->
  t ->
  unit
(** [start ~sw ~clock client queue] forks the queue's fibers on [sw] and returns
    at once.

    One fiber supervises the set of rooms with queued requests and forks a
    sender fiber for each new one, so a room waiting out a rate limit does not
    hold up the others. Each sender sends its room's head, sleeps the backoff a
    {!Retry_in} asks for, and idles while the room is empty, disabled, or its
    head is {!Wedged}. Releasing [sw] stops all of them. The store, if the queue
    has one, is flushed after every attempt, so an unsent queue survives a
    crash; a store that will not write is logged and the sending goes on.

    [encryption] wraps every request for a room the machine knows to be
    encrypted, sending it as [m.room.encrypted]. It needs [members], as
    {!sender_for} describes. [members] is read at each attempt, so somebody who
    joined after the request was queued is included. *)
