(** presentation — typed projections of Matrix timeline events.

    {!of_event} turns a wire event into a {!t} a renderer can match on, with
    HTML already sanitized and a state event already classified. It never fails.
    An event this library does not model becomes {!Custom}, and one it cannot
    read becomes {!Malformed}. *)

(** {1 Formatted bodies} *)

module Html : sig
  (** A message's [formatted_body] is rendered in the specification's subset of
      HTML. *)

  val sanitize : ?resolve_mxc:(string -> string option) -> string -> string
  (** [sanitize html] is [html] with only the Matrix-safe subset kept, unsafe
      attributes and URLs dropped, and the result serialized as a balanced
      fragment. The result is still HTML, so it must be rendered in a component
      that does not execute scripts.

      [<img>] survives with [alt], [title], positive decimal [width] and
      [height] values no greater than 16384, and a [src] that is an [mxc://]
      URI. Dimension values are canonicalized to decimal integers. Any other
      scheme is not a Matrix image and the element is dropped whole rather than
      left as an empty box. [resolve_mxc] rewrites a surviving [src], and
      answering [None] from it drops the image; {!Matrix_client.Media.Mxc} is
      where such a resolver comes from. Without [resolve_mxc] the [mxc://] URI
      is kept verbatim, which no browser can load, so a renderer either passes a
      resolver or handles [mxc://] itself.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroommessage-msgtypes>
        the permitted tags and attributes *)

  val to_plain : string -> string
  (** [to_plain html] is the readable text of [html], omitting the fallback
      [mx-reply] block. *)
end

(** {1 Content} *)

(** The type for what one event says about another. *)
type relation_kind =
  | Reply
  | Replacement
  | Annotation of string  (** The reaction key. *)
  | Thread
  | Reference
  | Custom_relation of string  (** An unrecognised [rel_type]. *)

type relation = { target : Matrix_proto.Id.Event_id.t; kind : relation_kind }
(** The type for an [m.relates_to], as the event it acts on and how. *)

(** The type for a message's [msgtype]. *)
type message_kind =
  | Text
  | Notice
  | Emote
  | Image
  | File
  | Audio
  | Video
  | Location
  | Verification_request
  | Custom_message of string  (** An unrecognised [msgtype]. *)

type formatted_body = { html : string;  (** Sanitized HTML. *) plain : string }
(** The type for a message's markup, as HTML and as the text of that HTML. *)

type message = {
  kind : message_kind;
  body : string;
  formatted : formatted_body option;
  filename : string option;
  url : string option;
  info : Jsont.json option;
      (** The [info] object of a media message, passed through unread. *)
}
(** The type for an [m.room.message] content. *)

(** The type for what an [m.room.member] event did, classified from its content
    and its [unsigned.prev_content]. An absent [prev_content] counts as [leave],
    and whether the sender is the subject is what separates {!Left} from
    {!Kicked}, {!Invitation_rejected} from {!Invitation_revoked}, and
    {!Knock_retracted} from {!Knock_denied}.

    A member changing their own display name or avatar is not here. That is
    {!Profile}. *)
type membership_change =
  | Joined
  | Left
  | Kicked
  | Banned
  | Kicked_and_banned
  | Unbanned
  | Invited
  | Invitation_accepted
  | Invitation_rejected
  | Invitation_revoked
  | Knocked
  | Knock_accepted
  | Knock_denied
  | Knock_retracted
  | No_change  (** The state did not change. *)
  | Invalid  (** A transition the specification forbids. *)
  | Unknown_membership  (** An unknown membership on either side. *)

type 'a change = { previous : 'a; current : 'a }
(** The type for an old and a new value. One is only ever built where the two
    differ. *)

type profile_change = {
  displayname : string option change option;
  avatar_url : string option change option;
}
(** The type for a change to a member's profile. [None] means the field did not
    change, and [Some { previous; current }] carries the two values, either of
    which may be [None] for absent. *)

(** The type for the state events this library models explicitly. Membership and
    profile changes are not here; they are {!Membership} and {!Profile}. *)
type other_state =
  | Room_create
  | Room_name of string option
  | Room_topic of string option
  | Room_avatar of string option
  | Room_canonical_alias of string option
  | Room_encryption
  | Room_pinned_events
  | Room_tombstone of string option  (** The replacement room. *)
  | Room_power_levels
  | Room_join_rules
  | Room_history_visibility
  | Room_guest_access
  | Room_server_acl
  | Room_third_party_invite of string option
  | Policy_rule of string  (** The [m.policy.rule.*] type. *)
  | Space_child
  | Space_parent
  | Beacon_info of Matrix_proto.Event.Beacon_info_content.t
      (** A live-location sharing start or stop ([MSC3672]). *)
  | Other_state_type of string  (** Anything else, by its event type. *)

(** The type for an event's content, once classified. *)
type content =
  | Message of message
  | Sticker of { body : string; url : string option; info : Jsont.json option }
      (** [info] is passed through unread. *)
  | Reaction of { key : string; target : Matrix_proto.Id.Event_id.t }
  | Redaction of {
      target : Matrix_proto.Id.Event_id.t option;
      reason : string option;
    }
  | Poll of { text : string }
  | Membership of {
      user : Matrix_proto.Id.User_id.t;  (** The subject, from the state key. *)
      change : membership_change;
      reason : string option;
    }
  | Profile of { user : Matrix_proto.Id.User_id.t; change : profile_change }
  | State of { event_type : string; state_key : string; state : other_state }
      (** A state event that is neither a membership nor a profile change. *)
  | Unable_to_decrypt  (** An [m.room.encrypted] event with no plaintext. *)
  | Custom of { event_type : string; content : Jsont.json }
      (** An event type this library does not model. [content] is passed through
          unread. *)
  | Malformed of { event_type : string; reason : string }

type t = {
  event_id : Matrix_proto.Id.Event_id.t option;
      (** [None] while the event is a local echo. *)
  sender : Matrix_proto.Id.User_id.t;
  timestamp : Matrix_proto.Event.Timestamp.t;
  relation : relation option;
  content : content;
  raw : Matrix_proto.Event.Raw_event.t;  (** The event this was read from. *)
}
(** The type for projected events. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] were read from equal events and
    classified alike. *)

(** {1 Projecting} *)

val of_event :
  ?resolve_mxc:(string -> string option) -> Matrix_proto.Event.Raw_event.t -> t
(** [of_event event] is the projection of a wire event. [resolve_mxc] is handed
    to {!Html.sanitize} for the [formatted_body] of an [org.matrix.custom.html]
    message, so that a toolkit which can mint an authenticated media URL gets
    [<img src>] rewritten to one. Without it an [mxc://] [src] is kept verbatim.
*)

(** {1 Edits} *)

val new_content : Matrix_proto.Event.Raw_event.t -> Jsont.json option
(** [new_content event] is the [m.new_content] an [m.replace] carries, which is
    the whole content of the message it puts in place of its target. *)

val replacement : ?resolve_mxc:(string -> string option) -> t -> t option
(** [replacement edit] is the message an [m.replace] puts in place of its
    target, read through its {!new_content} with the [m.replace] relation
    removed so that {!is_preview_worthy} judges it on its own merits. It is
    [None] for an event that is not an edit or that carries no [m.new_content].
    The [event_id], [sender] and [timestamp] stay the edit's. [resolve_mxc] is
    as in {!of_event}. *)

val is_valid_replacement : original:t -> replacement:t -> bool
(** [is_valid_replacement ~original ~replacement] is the specification's
    validity rules for a replacement. The two must have the same sender and the
    same type, neither may be a state event, [original] may not itself be an
    edit, and the [m.replace] of [replacement] must name [original]. The
    specification's two encryption clauses are not checked, because {!t} does
    not carry the encryption information they read.

    @see <https://spec.matrix.org/v1.11/client-server-api/#validity-of-replacement-events>
      Validity of replacement events *)

val is_valid_replacement_with_encryption :
  original:t ->
  original_encrypted:bool ->
  replacement:t ->
  replacement_encrypted:bool ->
  bool
(** [is_valid_replacement_with_encryption] applies {!is_valid_replacement} and
    the specification's two encryption clauses. [original_encrypted] and
    [replacement_encrypted] say that the corresponding event arrived encrypted
    and was successfully decrypted. An edit of an encrypted original must itself
    have arrived encrypted, and a decrypted encrypted edit must carry
    [m.new_content]. *)

(** {1 Previews} *)

val preview : t -> string option
(** [preview event] is a short plain-text representation of the event, for a
    room list or a notification. A state event reads as a sentence about who did
    what, such as ["@alice:localhost joined"] or
    ["@alice:localhost changed the room name to Kitchen"]. The subject of a
    membership or profile change is the state key, and of everything else the
    sender. *)

val is_preview_worthy :
  ?own_user:Matrix_proto.Id.User_id.t ->
  ?can_accept_knock:(Matrix_proto.Id.User_id.t -> bool) ->
  t ->
  bool
(** [is_preview_worthy event] is whether the event may stand as a room's latest
    event in a room list. A room message of any type but
    [m.key.verification.request] may, and so may a sticker, a poll start, an
    [m.call.invite] and an [m.rtc.notification], but in no case an edit. A
    reaction may not, nor a redaction, nor a redacted event, which has no
    [msgtype] left and so reads as {!Malformed}, nor one still
    {!Unable_to_decrypt}. A decrypted event is judged by its plaintext, so an
    encrypted room previews as its last message.

    [own_user] is the account the list belongs to. Given it, one kind of state
    event is accepted too, an [m.room.member] whose {i subject} is that user,
    the state key rather than the sender, and whose change is {!Joined},
    {!Invited}, {!Invitation_accepted} or {!Knock_accepted}, so that a freshly
    joined or invited room has something to show. A {!Knocked} membership is
    accepted when [own_user] is present and [can_accept_knock] says that user
    can be invited or kicked; the callback defaults to refusing every knock. A
    valid [org.matrix.msc3672.beacon_info] state event is also accepted (whether
    it starts or stops sharing); malformed beacon events and events whose state
    key is not a user ID are refused. Without [own_user], all other state events
    are refused. *)
