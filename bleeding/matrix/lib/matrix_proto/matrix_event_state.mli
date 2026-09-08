@@ portable

(** The content of the room state events.

    Room state is the set of events keyed by a type and a state key. These are
    the content objects of the types the specification defines for it, together
    with the enumerations their members range over.

    @see <https://spec.matrix.org/v1.11/client-server-api/#room-events>
      Room Events *)

(** {1 Enumerations} *)

module Membership : sig
  (** The states of a user's membership of a room, the value of an
      [m.room.member] event's [membership] member.

      @see <https://spec.matrix.org/v1.11/client-server-api/#room-membership>
        Room Membership *)

  type t = Join | Invite | Leave | Ban | Knock

  val to_string : t -> string
  (** [to_string t] is the wire form of [t]. *)

  val of_string : string -> (t, [> `Msg of string ]) result
  (** [of_string s] is the membership [s] names. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same membership. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. Decoding fails on a membership this
      type does not name. *)
end

module Join_rule : sig
  (** Who may join a room, the value of an [m.room.join_rules] event's
      [join_rule] member.

      {!Restricted} and {!Knock_restricted} delegate the decision to the rooms
      named in {!Room_join_rules_content.allow}. {!Private} is reserved and
      unused.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroomjoin_rules>
        m.room.join_rules *)

  type t = Public | Invite | Knock | Restricted | Knock_restricted | Private

  val to_string : t -> string
  (** [to_string t] is the wire form of [t]. *)

  val of_string : string -> (t, [> `Msg of string ]) result
  (** [of_string s] is the rule [s] names. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same rule. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. Decoding fails on a rule this type
      does not name. *)
end

module History_visibility : sig
  (** How far back a member may read a room's history. A member reads from their
      invite under {!Invited}, from their join under {!Joined}, from whichever
      came first under {!Shared}, and anyone reads from the beginning under
      {!World_readable}.

      @see <https://spec.matrix.org/v1.11/client-server-api/#room-history-visibility>
        Room History Visibility *)

  type t = Invited | Joined | Shared | World_readable

  val to_string : t -> string
  (** [to_string t] is the wire form of [t]. *)

  val of_string : string -> (t, [> `Msg of string ]) result
  (** [of_string s] is the visibility [s] names. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same visibility. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. Decoding fails on a visibility this
      type does not name. *)
end

module Guest_access : sig
  (** Whether guest accounts may join a room, the value of an
      [m.room.guest_access] event's [guest_access] member.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroomguest_access>
        m.room.guest_access *)

  type t = Can_join | Forbidden

  val to_string : t -> string
  (** [to_string t] is the wire form of [t]. *)

  val of_string : string -> (t, [> `Msg of string ]) result
  (** [of_string s] is the access [s] names. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same access. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. Decoding fails on an access this type
      does not name. *)
end

(** {1 Room state contents} *)

module Room_create_content : sig
  (** The content of [m.room.create], the first event in a room, which fixes its
      version and, for a space, its type.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroomcreate>
        m.room.create *)

  module Predecessor : sig
    (** The room this one replaced, set when a room was upgraded. *)

    type t
    (** The type for predecessor references. *)

    val make : room_id:Matrix_id.Room_id.t -> event_id:Matrix_id.Event_id.t -> t
    (** [make ~room_id ~event_id] is a reference to [event_id] in [room_id]. *)

    val room_id : t -> Matrix_id.Room_id.t
    (** [room_id t] is the room that was upgraded. *)

    val event_id : t -> Matrix_id.Event_id.t
    (** [event_id t] is the tombstone that closed it. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf t] prints the room and event. *)

    val jsont : t Jsont.t
    (** [jsont] is the JSON codec for {!t}. *)
  end

  type t
  (** The type for [m.room.create] contents. *)

  val make :
    ?creator:Matrix_id.User_id.t ->
    ?room_version:string ->
    ?predecessor:Predecessor.t ->
    ?type_:string ->
    unit ->
    t
  (** [make ()] is an [m.room.create] content. [creator] defaults to absent.
      [room_version] defaults to absent, which the server reads as ["1"].
      [predecessor] defaults to absent, marking a room that is not an upgrade.
      [type_] defaults to absent, marking an ordinary room. *)

  val creator : t -> Matrix_id.User_id.t option
  (** [creator t] is the user who made the room. It is absent from room version
      11 on, where the event's [sender] is the creator. *)

  val room_version : t -> string option
  (** [room_version t] is the room's version. Absent means ["1"]. *)

  val predecessor : t -> Predecessor.t option
  (** [predecessor t] is the room this one replaced. *)

  val room_type : t -> string option
  (** [room_type t] is the [type] member. It is ["m.space"] for a space and
      absent for an ordinary room. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the members that are present. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_name_content : sig
  (** The content of [m.room.name], the room's human-readable name. A room
      without one is named from its alias or its members instead.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroomname>
        m.room.name *)

  type t
  (** The type for [m.room.name] contents. *)

  val make : name:string -> t
  (** [make ~name] is an [m.room.name] content naming the room [name]. *)

  val name : t -> string
  (** [name t] is the room's name. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the name. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_topic_content : sig
  (** The content of [m.room.topic], a line describing what the room is for.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroomtopic>
        m.room.topic *)

  type t
  (** The type for [m.room.topic] contents. *)

  val make : topic:string -> t
  (** [make ~topic] is an [m.room.topic] content carrying [topic]. *)

  val topic : t -> string
  (** [topic t] is the room's topic. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the topic. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_avatar_content : sig
  (** The content of [m.room.avatar], the room's picture.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroomavatar>
        m.room.avatar *)

  type t
  (** The type for [m.room.avatar] contents. *)

  val make : ?url:string -> ?info:Matrix_event_core.Image_info.t -> unit -> t
  (** [make ()] is an [m.room.avatar] content. [url] defaults to absent, which
      means the room has no avatar. [info] defaults to absent. *)

  val url : t -> string option
  (** [url t] is an [mxc://] URI. Absent means the room has no avatar. *)

  val info : t -> Matrix_event_core.Image_info.t option
  (** [info t] is what the sender claims about the picture. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the members that are present. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_member_content : sig
  (** The content of [m.room.member], one user's membership of the room, keyed
      by their user id as the event's state key.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroommember>
        m.room.member *)

  type t
  (** The type for [m.room.member] contents. *)

  val make :
    membership:Membership.t ->
    ?displayname:string ->
    ?avatar_url:string ->
    ?is_direct:bool ->
    ?reason:string ->
    unit ->
    t
  (** [make ~membership ()] is an [m.room.member] content declaring
      [membership]. [displayname], [avatar_url], [is_direct] and [reason] all
      default to absent. *)

  val membership : t -> Membership.t
  (** [membership t] is the state the user is in. *)

  val displayname : t -> string option
  (** [displayname t] is the name to show for this user {e in this room}, which
      need not be their profile name. *)

  val avatar_url : t -> string option
  (** [avatar_url t] is an [mxc://] URI for the user's picture in this room. *)

  val is_direct : t -> bool option
  (** [is_direct t] is set on an invite to say the room is meant as a direct
      chat. *)

  val reason : t -> string option
  (** [reason t] is why the membership changed, for a kick, ban, leave or knock.
  *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the members that are present. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_join_rules_content : sig
  (** The content of [m.room.join_rules], which says who may join.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroomjoin_rules>
        m.room.join_rules *)

  module Allow_condition : sig
    (** One way in, under a {!Join_rule.Restricted} or
        {!Join_rule.Knock_restricted} rule. *)

    type t
    (** The type for allow conditions. *)

    val make : type_:string -> ?room_id:Matrix_id.Room_id.t -> unit -> t
    (** [make ~type_ ()] is a condition of kind [type_], which is
        ["m.room_membership"], the only kind defined. [room_id] defaults to
        absent and otherwise names the room whose members may join. *)

    val condition_type : t -> string
    (** [condition_type t] is the [type] member. *)

    val room_id : t -> Matrix_id.Room_id.t option
    (** [room_id t] is the room whose members the condition admits. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf t] prints the kind and the room. *)

    val jsont : t Jsont.t
    (** [jsont] is the JSON codec for {!t}. *)
  end

  type t
  (** The type for [m.room.join_rules] contents. *)

  val make : join_rule:Join_rule.t -> ?allow:Allow_condition.t list -> unit -> t
  (** [make ~join_rule ()] is an [m.room.join_rules] content. [allow] defaults
      to absent. *)

  val join_rule : t -> Join_rule.t
  (** [join_rule t] is the rule in force. *)

  val allow : t -> Allow_condition.t list option
  (** [allow t] is meaningful only under a restricted join rule, where a member
      of any listed room may join. An empty list admits nobody. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the rule and any conditions. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_history_visibility_content : sig
  (** The content of [m.room.history_visibility], which says how much of the
      past a member may read.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroomhistory_visibility>
        m.room.history_visibility *)

  type t
  (** The type for [m.room.history_visibility] contents. *)

  val make : history_visibility:History_visibility.t -> t
  (** [make ~history_visibility] is a content declaring [history_visibility]. *)

  val history_visibility : t -> History_visibility.t
  (** [history_visibility t] is the visibility in force. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the visibility. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_canonical_alias_content : sig
  (** The content of [m.room.canonical_alias], the alias to show for the room
      and any others that also point at it.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroomcanonical_alias>
        m.room.canonical_alias *)

  type t
  (** The type for [m.room.canonical_alias] contents. *)

  val make :
    ?alias:Matrix_id.Room_alias.t ->
    ?alt_aliases:Matrix_id.Room_alias.t list ->
    unit ->
    t
  (** [make ()] is an [m.room.canonical_alias] content. [alias] and
      [alt_aliases] both default to absent. *)

  val alias : t -> Matrix_id.Room_alias.t option
  (** [alias t] is the alias a client should show. *)

  val alt_aliases : t -> Matrix_id.Room_alias.t list option
  (** [alt_aliases t] is the other aliases that point at the room. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the aliases that are present. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_power_levels_content : sig
  (** The content of [m.room.power_levels], the level each action needs and the
      level each user has.

      Every member is at {!users_default} unless {!users} says otherwise, and an
      absent member takes the specification's default, which is 50 for {!ban},
      {!kick}, {!redact} and {!state_default} and 0 for {!invite},
      {!events_default} and {!users_default}.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroompower_levels>
        m.room.power_levels *)

  type t
  (** The type for [m.room.power_levels] contents. *)

  val make :
    ?ban:int ->
    ?events:(string * int) list ->
    ?events_default:int ->
    ?invite:int ->
    ?kick:int ->
    ?redact:int ->
    ?state_default:int ->
    ?users:(string * int) list ->
    ?users_default:int ->
    ?notifications:(string * int) list ->
    unit ->
    t
  (** [make ()] is an [m.room.power_levels] content. Every argument defaults to
      absent, leaving the specification's default in force for that member. *)

  val ban : t -> int option
  (** [ban t] is the level needed to ban a user. *)

  val events : t -> (string * int) list option
  (** [events t] is the level needed per event type, overriding
      {!events_default} and {!state_default}. *)

  val events_default : t -> int option
  (** [events_default t] is the level needed to send a message event whose type
      {!events} does not name. *)

  val invite : t -> int option
  (** [invite t] is the level needed to invite a user. *)

  val kick : t -> int option
  (** [kick t] is the level needed to remove a user. *)

  val redact : t -> int option
  (** [redact t] is the level needed to redact another user's event. Redacting
      one's own needs nothing. *)

  val state_default : t -> int option
  (** [state_default t] is the level needed to send a state event whose type
      {!events} does not name. *)

  val users : t -> (string * int) list option
  (** [users t] maps a user id to that user's level. *)

  val users_default : t -> int option
  (** [users_default t] is the level of a member {!users} does not name. *)

  val notifications : t -> (string * int) list option
  (** [notifications t] is the level needed per room-wide notification key. The
      key ["room"] is the one the specification defines. *)

  val user_level : t -> Matrix_id.User_id.t -> int
  (** [user_level t user] is the level [user] holds. A user {!users} does not
      name is at {!users_default}, and an absent {!users_default} is 0. *)

  val with_user_level : t -> Matrix_id.User_id.t -> int -> t
  (** [with_user_level t user level] is [t] with [user] at [level], replacing
      the entry {!users} held for them. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the scalar levels that are present. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_retention_content : sig
  (** The content of a room retention policy. Lifetimes are integer
      milliseconds; either limit may be absent. The stable event type is
      [m.room.retention], while MSC1763 peers may use
      [org.matrix.msc1763.retention]. *)

  type t
  (** The type for room retention content. *)

  val make : ?min_lifetime:int64 -> ?max_lifetime:int64 -> unit -> t
  (** [make ()] is a retention policy. Both limits default to absent. *)

  val min_lifetime : t -> int64 option
  (** [min_lifetime t] is the minimum lifetime in milliseconds, if present. *)

  val max_lifetime : t -> int64 option
  (** [max_lifetime t] is the maximum lifetime in milliseconds, if present. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the limits that are present. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_member_hints_content : sig
  (** The content of [m.room.member_hints], naming service members that should
      not affect a room's human-facing member name or avatar. *)

  type t
  (** The type for member hints content. *)

  val make : ?service_members:Matrix_id.User_id.t list -> unit -> t
  (** [make ()] is member hints with no service members. *)

  val service_members : t -> Matrix_id.User_id.t list
  (** [service_members t] is the typed service-member user IDs. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the service members. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}; an absent [service_members] member is
      decoded as the empty list. *)
end

module Io_element_functional_members_content : sig
  (** The legacy [io.element.functional_members] content. Its [service_members]
      member is retained and decoded as typed user IDs. *)

  type t
  (** The type for legacy functional-member content. *)

  val make : ?service_members:Matrix_id.User_id.t list -> unit -> t
  (** [make ()] is legacy functional-member content with no service members. *)

  val service_members : t -> Matrix_id.User_id.t list
  (** [service_members t] is the typed service-member user IDs. *)

  val functional_members : t -> Matrix_id.User_id.t list
  (** [functional_members t] is an alias for {!service_members}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the service members. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}; an absent [service_members] member is
      decoded as the empty list. *)
end

module Room_encryption_content : sig
  (** The content of [m.room.encryption], which declares the room encrypted
      under an algorithm.

      The event is not reversible. Once it is in a room's state, every message
      event must be encrypted.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroomencryption>
        m.room.encryption *)

  type t
  (** The type for [m.room.encryption] contents. *)

  val make :
    algorithm:string ->
    ?rotation_period_ms:int64 ->
    ?rotation_period_msgs:int ->
    unit ->
    t
  (** [make ~algorithm ()] is an [m.room.encryption] content.
      [rotation_period_ms] defaults to absent, which the specification reads as
      a week. [rotation_period_msgs] defaults to absent, which it reads as 100.
  *)

  val algorithm : t -> string
  (** [algorithm t] is the encryption algorithm. ["m.megolm.v1.aes-sha2"] is the
      only value defined. *)

  val rotation_period_ms : t -> int64 option
  (** [rotation_period_ms t] is how long a Megolm session may be used, in
      milliseconds. Absent means a week. *)

  val rotation_period_msgs : t -> int option
  (** [rotation_period_msgs t] is how many messages a Megolm session may carry.
      Absent means 100. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the members that are present. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_pinned_events_content : sig
  (** The content of [m.room.pinned_events], the events a client should
      highlight.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroompinned_events>
        m.room.pinned_events *)

  type t
  (** The type for [m.room.pinned_events] contents. *)

  val make : ?pinned:string list -> unit -> t
  (** [make ()] is an [m.room.pinned_events] content. [pinned] defaults to the
      empty list. *)

  val pinned : t -> string list
  (** [pinned t] is the pinned event ids, most recently pinned last. It is empty
      when nothing is pinned. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the pinned ids. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_server_acl_content : sig
  (** The content of [m.room.server_acl], which says which servers may take part
      in the room.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroomserver_acl>
        m.room.server_acl *)

  type t
  (** The type for [m.room.server_acl] contents. *)

  val make :
    ?allow:string list ->
    ?allow_ip_literals:bool ->
    ?deny:string list ->
    unit ->
    t
  (** [make ()] is an [m.room.server_acl] content. [allow] defaults to the empty
      list. [allow_ip_literals] defaults to [true]. [deny] defaults to the empty
      list. *)

  val allow : t -> string list
  (** [allow t] is the server-name globs that may participate. An empty list
      bars every server, including the room's own, which is why a client should
      refuse to send one. *)

  val allow_ip_literals : t -> bool
  (** [allow_ip_literals t] is whether a server named by bare IP address may
      participate. *)

  val deny : t -> string list
  (** [deny t] is the server-name globs that may not participate, checked after
      {!allow}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the three members. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_tombstone_content : sig
  (** The content of [m.room.tombstone], which says the room is closed and
      continues elsewhere.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroomtombstone>
        m.room.tombstone *)

  type t
  (** The type for [m.room.tombstone] contents. *)

  val make : body:string -> replacement_room:Matrix_id.Room_id.t -> t
  (** [make ~body ~replacement_room] is an [m.room.tombstone] content pointing
      at [replacement_room]. *)

  val body : t -> string
  (** [body t] is text to show in place of the room's timeline. *)

  val replacement_room : t -> Matrix_id.Room_id.t
  (** [replacement_room t] is the room that continues this one. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the body and the replacement room. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Room_guest_access_content : sig
  (** The content of [m.room.guest_access], which says whether guest accounts
      may join. It is consulted only when the join rule would otherwise let them
      in.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroomguest_access>
        m.room.guest_access *)

  type t
  (** The type for [m.room.guest_access] contents. *)

  val make : guest_access:Guest_access.t -> t
  (** [make ~guest_access] is a content declaring [guest_access]. *)

  val guest_access : t -> Guest_access.t
  (** [guest_access t] is the access in force. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the access. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end
