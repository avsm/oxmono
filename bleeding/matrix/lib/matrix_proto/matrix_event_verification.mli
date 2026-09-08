@@ portable

(** The content of the key verification events.

    A verification flow runs as a sequence of events sharing one flow
    identifier. It is [request], [ready], [start], [accept], [key], [mac] and
    [done], or [cancel] at any point. A to-device flow carries the identifier as
    [transaction_id]. An in-room flow carries it as an [m.reference] relation to
    the request, in the [m.relates_to] member.

    @see <https://spec.matrix.org/v1.11/client-server-api/#key-verification-framework>
      Key verification framework *)

module Key_verification_request_content : sig
  (** The content of an [m.key.verification.request] to-device event.

      One request is sent to every device of the user being verified, all
      sharing a single [transaction_id]. The first device to answer with an
      [m.key.verification.ready] wins and the others are cancelled with
      [m.accepted]. The in-room form is
      {!Key_verification_request_message_content}.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mkeyverificationrequest>
        m.key.verification.request *)

  type t = {
    from_device : string;  (** Device initiating the request. *)
    methods : string list;  (** Verification methods the sender supports. *)
    transaction_id : Matrix_id.Transaction_id.t option;
        (** Required for to-device messages. *)
    timestamp : Matrix_event_core.Timestamp.t option;
        (** When the request was made. A request more than five minutes in the
            future or ten minutes in the past should be ignored. *)
  }

  val make :
    from_device:string ->
    methods:string list ->
    ?transaction_id:Matrix_id.Transaction_id.t ->
    ?timestamp:Matrix_event_core.Timestamp.t ->
    unit ->
    t
  (** [make ~from_device ~methods ()] is a request content. [transaction_id] and
      [timestamp] both default to absent. *)

  val from_device : t -> string
  (** [from_device t] is the device that opened the flow. *)

  val methods : t -> string list
  (** [methods t] is the verification methods the sender supports. *)

  val transaction_id : t -> Matrix_id.Transaction_id.t option
  (** [transaction_id t] is the flow identifier. *)

  val timestamp : t -> Matrix_event_core.Timestamp.t option
  (** [timestamp t] is when the request was made. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the device and the methods. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Key_verification_request_message_content : sig
  (** The content of an [m.room.message] with msgtype
      [m.key.verification.request], the in-room form of a verification request.
      The event id of this message is the flow identifier of everything that
      follows.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroommessagemkeyverificationrequest>
        m.room.message with msgtype m.key.verification.request *)

  type t = {
    body : string;  (** Fallback text for clients without verification. *)
    from_device : string;
    methods : string list;
    to_ : string;  (** The user the request is addressed to, the [to] member. *)
    format : string option;
    formatted_body : string option;
  }

  val msgtype : string
  (** [msgtype] is ["m.key.verification.request"]. *)

  val make :
    ?body:string ->
    from_device:string ->
    methods:string list ->
    to_:string ->
    ?format:string ->
    ?formatted_body:string ->
    unit ->
    t
  (** [make ~from_device ~methods ~to_ ()] is a request message content. [body]
      defaults to the empty string. [format] and [formatted_body] default to
      absent. *)

  val body : t -> string
  (** [body t] is the fallback text. *)

  val from_device : t -> string
  (** [from_device t] is the device that opened the flow. *)

  val methods : t -> string list
  (** [methods t] is the verification methods the sender supports. *)

  val to_ : t -> string
  (** [to_ t] is the user the request is addressed to. *)

  val format : t -> string option
  (** [format t] is the format of {!val-formatted_body}. *)

  val formatted_body : t -> string option
  (** [formatted_body t] is the rich form of the fallback text. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the recipient, the device and the methods. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Key_verification_ready_content : sig
  (** The content of an [m.key.verification.ready] event, the answer to a
      request, naming the methods both sides could use.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mkeyverificationready>
        m.key.verification.ready *)

  type t = {
    from_device : string;
    methods : string list;
    transaction_id : Matrix_id.Transaction_id.t option;
    relates_to : Matrix_event_core.Relates_to.t option;
  }

  val make :
    from_device:string ->
    methods:string list ->
    ?transaction_id:Matrix_id.Transaction_id.t ->
    ?relates_to:Matrix_event_core.Relates_to.t ->
    unit ->
    t
  (** [make ~from_device ~methods ()] is a ready content. [transaction_id] and
      [relates_to] both default to absent, and exactly one of them addresses the
      flow. *)

  val from_device : t -> string
  (** [from_device t] is the device that answered. *)

  val methods : t -> string list
  (** [methods t] is the methods the answering side supports. *)

  val transaction_id : t -> Matrix_id.Transaction_id.t option
  (** [transaction_id t] is the to-device flow identifier. *)

  val relates_to : t -> Matrix_event_core.Relates_to.t option
  (** [relates_to t] is the in-room flow identifier. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the device and the methods. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Key_verification_start_content : sig
  (** The content of an [m.key.verification.start] event.

      A single codec covers both defined methods. [m.sas.v1] fills in the
      negotiation lists, [m.reciprocate.v1] fills in {!field-secret} with the
      shared secret read out of a scanned QR code.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mkeyverificationstart>
        m.key.verification.start
      @see <https://spec.matrix.org/v1.11/client-server-api/#mkeyverificationstartmsasv1>
        m.key.verification.start with method m.sas.v1
      @see <https://spec.matrix.org/v1.11/client-server-api/#mkeyverificationstartmreciprocatev1>
        m.key.verification.start with method m.reciprocate.v1 *)

  type t = {
    from_device : string;
    method_ : string;  (** The [method] member. *)
    transaction_id : Matrix_id.Transaction_id.t option;
    next_method : string option;
    key_agreement_protocols : string list option;  (** [m.sas.v1] only. *)
    hashes : string list option;  (** [m.sas.v1] only. *)
    message_authentication_codes : string list option;  (** [m.sas.v1] only. *)
    short_authentication_string : string list option;  (** [m.sas.v1] only. *)
    secret : string option;  (** [m.reciprocate.v1] only, as unpadded base64. *)
    relates_to : Matrix_event_core.Relates_to.t option;
  }

  val make :
    from_device:string ->
    method_:string ->
    ?transaction_id:Matrix_id.Transaction_id.t ->
    ?next_method:string ->
    ?key_agreement_protocols:string list ->
    ?hashes:string list ->
    ?message_authentication_codes:string list ->
    ?short_authentication_string:string list ->
    ?secret:string ->
    ?relates_to:Matrix_event_core.Relates_to.t ->
    unit ->
    t
  (** [make ~from_device ~method_ ()] is a start content. Every optional
      argument defaults to absent. *)

  val from_device : t -> string
  (** [from_device t] is the device that started the method. *)

  val method_ : t -> string
  (** [method_ t] is the [method] member. *)

  val transaction_id : t -> Matrix_id.Transaction_id.t option
  (** [transaction_id t] is the to-device flow identifier. *)

  val next_method : t -> string option
  (** [next_method t] is the method to run once this one succeeds. *)

  val key_agreement_protocols : t -> string list option
  (** [key_agreement_protocols t] is the offered key agreement protocols. *)

  val hashes : t -> string list option
  (** [hashes t] is the offered hash functions. *)

  val message_authentication_codes : t -> string list option
  (** [message_authentication_codes t] is the offered MAC algorithms. *)

  val short_authentication_string : t -> string list option
  (** [short_authentication_string t] is the offered comparison formats. *)

  val secret : t -> string option
  (** [secret t] is the shared secret from a scanned QR code. *)

  val relates_to : t -> Matrix_event_core.Relates_to.t option
  (** [relates_to t] is the in-room flow identifier. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the device and the method. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Key_verification_accept_content : sig
  (** The content of an [m.key.verification.accept] event, the accepting side's
      choice of algorithms and its commitment to its ephemeral key.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mkeyverificationaccept>
        m.key.verification.accept *)

  type t = {
    transaction_id : Matrix_id.Transaction_id.t option;
    method_ : string;
    key_agreement_protocol : string;
    hash : string;
    message_authentication_code : string;
    short_authentication_string : string list;
    commitment : string;
        (** Unpadded base64 of the hash of the accepting device's ephemeral
            public key concatenated with the canonical JSON of the
            [m.key.verification.start] content. *)
    relates_to : Matrix_event_core.Relates_to.t option;
  }

  val make :
    ?transaction_id:Matrix_id.Transaction_id.t ->
    method_:string ->
    key_agreement_protocol:string ->
    hash:string ->
    message_authentication_code:string ->
    short_authentication_string:string list ->
    commitment:string ->
    ?relates_to:Matrix_event_core.Relates_to.t ->
    unit ->
    t
  (** [make ~method_ ~key_agreement_protocol ~hash ~message_authentication_code
       ~short_authentication_string ~commitment ()] is an accept content.
      [transaction_id] and [relates_to] both default to absent. *)

  val transaction_id : t -> Matrix_id.Transaction_id.t option
  (** [transaction_id t] is the to-device flow identifier. *)

  val method_ : t -> string
  (** [method_ t] is the method being accepted. *)

  val key_agreement_protocol : t -> string
  (** [key_agreement_protocol t] is the chosen key agreement protocol. *)

  val hash : t -> string
  (** [hash t] is the chosen hash function. *)

  val message_authentication_code : t -> string
  (** [message_authentication_code t] is the chosen MAC algorithm. *)

  val short_authentication_string : t -> string list
  (** [short_authentication_string t] is the chosen comparison formats. *)

  val commitment : t -> string
  (** [commitment t] is the accepting side's commitment. *)

  val relates_to : t -> Matrix_event_core.Relates_to.t option
  (** [relates_to t] is the in-room flow identifier. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the three chosen algorithms. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Key_verification_key_content : sig
  (** The content of an [m.key.verification.key] event, one side's ephemeral
      Curve25519 public key as unpadded base64.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mkeyverificationkey>
        m.key.verification.key *)

  type t = {
    transaction_id : Matrix_id.Transaction_id.t option;
    key : string;
    relates_to : Matrix_event_core.Relates_to.t option;
  }

  val make :
    ?transaction_id:Matrix_id.Transaction_id.t ->
    key:string ->
    ?relates_to:Matrix_event_core.Relates_to.t ->
    unit ->
    t
  (** [make ~key ()] is a key content. [transaction_id] and [relates_to] both
      default to absent. *)

  val transaction_id : t -> Matrix_id.Transaction_id.t option
  (** [transaction_id t] is the to-device flow identifier. *)

  val key : t -> string
  (** [key t] is the ephemeral public key. *)

  val relates_to : t -> Matrix_event_core.Relates_to.t option
  (** [relates_to t] is the in-room flow identifier. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the key. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Key_verification_mac_content : sig
  (** The content of an [m.key.verification.mac] event, each side's MAC over the
      keys it is asserting, computed with the algorithm the accept chose. A
      mismatch means the flow must be cancelled.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mkeyverificationmac>
        m.key.verification.mac *)

  type t = {
    transaction_id : Matrix_id.Transaction_id.t option;
    mac : (string * string) list;
        (** Key id to unpadded base64 MAC of that key. *)
    keys : string;
        (** MAC of the sorted, comma-separated list of key ids in {!field-mac}.
        *)
    relates_to : Matrix_event_core.Relates_to.t option;
  }

  val make :
    ?transaction_id:Matrix_id.Transaction_id.t ->
    mac:(string * string) list ->
    keys:string ->
    ?relates_to:Matrix_event_core.Relates_to.t ->
    unit ->
    t
  (** [make ~mac ~keys ()] is a MAC content. [transaction_id] and [relates_to]
      both default to absent. *)

  val transaction_id : t -> Matrix_id.Transaction_id.t option
  (** [transaction_id t] is the to-device flow identifier. *)

  val mac : t -> (string * string) list
  (** [mac t] is the MAC of each asserted key. *)

  val keys : t -> string
  (** [keys t] is the MAC over the key ids. *)

  val relates_to : t -> Matrix_event_core.Relates_to.t option
  (** [relates_to t] is the in-room flow identifier. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the key ids. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Key_verification_cancel_content : sig
  (** The content of an [m.key.verification.cancel] event. Either side may send
      one at any point, and the flow is over.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mkeyverificationcancel>
        m.key.verification.cancel *)

  type t = {
    transaction_id : Matrix_id.Transaction_id.t option;
    code : string;  (** A machine-readable code such as ["m.user"]. *)
    reason : string;  (** A human-readable description of {!field-code}. *)
    relates_to : Matrix_event_core.Relates_to.t option;
  }

  val make :
    ?transaction_id:Matrix_id.Transaction_id.t ->
    code:string ->
    reason:string ->
    ?relates_to:Matrix_event_core.Relates_to.t ->
    unit ->
    t
  (** [make ~code ~reason ()] is a cancel content. [transaction_id] and
      [relates_to] both default to absent. *)

  val transaction_id : t -> Matrix_id.Transaction_id.t option
  (** [transaction_id t] is the to-device flow identifier. *)

  val code : t -> string
  (** [code t] is the machine-readable cancellation code. *)

  val reason : t -> string
  (** [reason t] is the human-readable description. *)

  val relates_to : t -> Matrix_event_core.Relates_to.t option
  (** [relates_to t] is the in-room flow identifier. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the code and the reason. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Key_verification_done_content : sig
  (** The content of an [m.key.verification.done] event. Both sides send one
      once the MACs check out, and only then is the verification complete.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mkeyverificationdone>
        m.key.verification.done *)

  type t = {
    transaction_id : Matrix_id.Transaction_id.t option;
    relates_to : Matrix_event_core.Relates_to.t option;
  }

  val make :
    ?transaction_id:Matrix_id.Transaction_id.t ->
    ?relates_to:Matrix_event_core.Relates_to.t ->
    unit ->
    t
  (** [make ()] is a done content. [transaction_id] and [relates_to] both
      default to absent. *)

  val transaction_id : t -> Matrix_id.Transaction_id.t option
  (** [transaction_id t] is the to-device flow identifier. *)

  val relates_to : t -> Matrix_event_core.Relates_to.t option
  (** [relates_to t] is the in-room flow identifier. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints a fixed word. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end
