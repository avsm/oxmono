@@ portable

(** The content of the VoIP call events.

    A one-to-one call is signalled by events in the room, all sharing a
    [call_id] and each naming the sending client with a [party_id]. A group call
    is instead announced in room state by [m.call.member]. The media never
    passes through Matrix.

    @see <https://spec.matrix.org/v1.11/client-server-api/#voice-over-ip>
      Voice over IP *)

module Sdp : sig
  (** A WebRTC session description. *)

  type t = {
    type_ : string;  (** The [type] member, such as ["offer"]. *)
    sdp : string;  (** The session description, opaque here. *)
  }

  val v : type_:string -> sdp:string -> t
  (** [v ~type_ ~sdp] is the session description [sdp] of kind [type_]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the kind and the length of the description. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Hangup_reason : sig
  (** Why a call ended. *)

  type t =
    | Ice_failed  (** No media path could be negotiated. *)
    | Invite_timeout  (** The offer expired unanswered. *)
    | User_hangup  (** Someone hung up. *)
    | User_media_failed  (** No camera or microphone. *)
    | User_busy  (** The callee is already in a call. *)
    | Unknown_error  (** Anything else. *)

  val to_string : t -> string
  (** [to_string t] is the wire form of [t]. *)

  val of_string : string -> (t, [> `Msg of string ]) result
  (** [of_string s] is the reason [s] names. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same reason. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. Decoding fails on a reason this type
      does not name. *)
end

module Call_invite_content : sig
  (** The content of [m.call.invite], an offer to start a call.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mcallinvite>
        m.call.invite *)

  type t = {
    call_id : string;
        (** Identifies the call, chosen by the caller and echoed by every event
            of that call. *)
    party_id : string option;
        (** Identifies the sending client, so that a user answering from two
            devices can be told apart. *)
    version : int;  (** The call protocol version. *)
    lifetime : int;  (** Milliseconds the offer stays valid. *)
    offer : Sdp.t;
    invitee : string option;
        (** Restricts the invitation to one user, in a room with more than two.
        *)
  }

  val make :
    call_id:string ->
    ?party_id:string ->
    ?version:int ->
    lifetime:int ->
    offer:Sdp.t ->
    ?invitee:string ->
    unit ->
    t
  (** [make ~call_id ~lifetime ~offer ()] is an [m.call.invite] content.
      [party_id] defaults to absent. [version] defaults to [0], the original
      call protocol. [invitee] defaults to absent. *)

  val call_id : t -> string
  (** [call_id t] is the call this event belongs to. *)

  val party_id : t -> string option
  (** [party_id t] is the sending client. *)

  val version : t -> int
  (** [version t] is the call protocol version. *)

  val lifetime : t -> int
  (** [lifetime t] is how long the offer stays valid, in milliseconds. *)

  val offer : t -> Sdp.t
  (** [offer t] is the caller's session description. *)

  val invitee : t -> string option
  (** [invitee t] is the one user the invitation is meant for. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the call id, version and lifetime. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Call_answer_content : sig
  (** The content of [m.call.answer], with which the callee accepts and sends
      its own session description.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mcallanswer>
        m.call.answer *)

  type t = {
    call_id : string;
    party_id : string option;
    version : int;
    answer : Sdp.t;  (** The callee's description, of type ["answer"]. *)
  }

  val make :
    call_id:string ->
    ?party_id:string ->
    ?version:int ->
    answer:Sdp.t ->
    unit ->
    t
  (** [make ~call_id ~answer ()] is an [m.call.answer] content. [party_id]
      defaults to absent. [version] defaults to [0]. *)

  val call_id : t -> string
  (** [call_id t] is the call this event belongs to. *)

  val party_id : t -> string option
  (** [party_id t] is the sending client. *)

  val version : t -> int
  (** [version t] is the call protocol version. *)

  val answer : t -> Sdp.t
  (** [answer t] is the callee's session description. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the call id and version. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Call_hangup_content : sig
  (** The content of [m.call.hangup], which says the call is over or never
      started. [m.call.reject] carries the same content.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mcallhangup>
        m.call.hangup *)

  type t = {
    call_id : string;
    party_id : string option;
    version : int;
    reason : Hangup_reason.t option;
        (** Absent from the event means {!Hangup_reason.User_hangup}. *)
  }

  val make :
    call_id:string ->
    ?party_id:string ->
    ?version:int ->
    ?reason:Hangup_reason.t ->
    unit ->
    t
  (** [make ~call_id ()] is an [m.call.hangup] content. [party_id] defaults to
      absent. [version] defaults to [0]. [reason] defaults to absent, which is
      read as {!Hangup_reason.User_hangup}. *)

  val call_id : t -> string
  (** [call_id t] is the call this event belongs to. *)

  val party_id : t -> string option
  (** [party_id t] is the sending client. *)

  val version : t -> int
  (** [version t] is the call protocol version. *)

  val reason : t -> Hangup_reason.t option
  (** [reason t] is why the call ended. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the call id, version and reason. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Call_candidates_content : sig
  (** The content of [m.call.candidates], further ICE candidates sent as they
      are discovered rather than waiting for the offer.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mcallcandidates>
        m.call.candidates *)

  type candidate = {
    candidate : string;  (** The SDP [a=candidate] line. *)
    sdp_mid : string;  (** The media stream this candidate belongs to. *)
    sdp_m_line_index : int;  (** That stream's index in the SDP. *)
  }
  (** The type for one ICE candidate. *)

  val make_candidate :
    candidate:string -> sdp_mid:string -> sdp_m_line_index:int -> candidate
  (** [make_candidate ~candidate ~sdp_mid ~sdp_m_line_index] is one candidate.
  *)

  type t = {
    call_id : string;
    party_id : string option;
    version : int;
    candidates : candidate list;
  }

  val make :
    call_id:string ->
    ?party_id:string ->
    ?version:int ->
    ?candidates:candidate list ->
    unit ->
    t
  (** [make ~call_id ()] is an [m.call.candidates] content. [party_id] defaults
      to absent. [version] defaults to [0]. [candidates] defaults to the empty
      list. *)

  val call_id : t -> string
  (** [call_id t] is the call this event belongs to. *)

  val party_id : t -> string option
  (** [party_id t] is the sending client. *)

  val version : t -> int
  (** [version t] is the call protocol version. *)

  val candidates : t -> candidate list
  (** [candidates t] is the candidates carried by this event. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the call id, version and candidate count. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Call_member_content : sig
  (** The content of [m.call.member], who is currently in a group call, keyed by
      user id as the event's state key. Unlike the one-to-one call events this
      is room state, so a joining client learns the call by reading it.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mcallmember>
        m.call.member *)

  type focus = {
    type_ : string;  (** The [type] member, such as ["livekit"]. *)
    livekit_service_url : string option;
    livekit_alias : string option;
  }
  (** The type for a media server a participant is reachable through. *)

  val make_focus :
    type_:string ->
    ?livekit_service_url:string ->
    ?livekit_alias:string ->
    unit ->
    focus
  (** [make_focus ~type_ ()] is a focus of kind [type_]. [livekit_service_url]
      and [livekit_alias] both default to absent. *)

  type membership = {
    call_id : string;  (** Empty for the room's single unnamed call. *)
    scope : string;  (** ["m.room"] for a call the whole room may join. *)
    application : string;  (** ["m.call"] for audio and video. *)
    device_id : string;
    expires : int64;
        (** Milliseconds from the event's timestamp after which the membership
            is stale, so that a client that vanished does not appear to be in
            the call for ever. *)
    foci_active : focus list option;
    membership_id : string option;
        (** Distinguishes successive memberships of one device, so a rejoin is
            not mistaken for the previous session. *)
  }
  (** The type for one device's participation in one call. *)

  val make_membership :
    call_id:string ->
    ?scope:string ->
    application:string ->
    device_id:string ->
    expires:int64 ->
    ?foci_active:focus list ->
    ?membership_id:string ->
    unit ->
    membership
  (** [make_membership ~call_id ~application ~device_id ~expires ()] is one
      participation. [scope] defaults to ["m.room"]. [foci_active] and
      [membership_id] default to absent. *)

  type t = { memberships : membership list }

  val make : ?memberships:membership list -> unit -> t
  (** [make ()] is an [m.call.member] content. [memberships] defaults to the
      empty list. *)

  val memberships : t -> membership list
  (** [memberships t] is the calls the user is in. Empty means the user has left
      every call in the room. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints how many memberships are declared. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end
