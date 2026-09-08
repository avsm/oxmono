@@ portable

(** Push rules.

    A push rule is a test applied to an event to decide whether it deserves a
    notification and what that notification carries. A ruleset holds the five
    kinds of rule a user has, in the order they are consulted. Nothing here
    evaluates a rule against an event.

    @see <https://spec.matrix.org/v1.11/client-server-api/#push-rules>
      Push Rules *)

(** {1 Rule kinds} *)

module Kind : sig
  (** The kind a rule is filed under.

      The kind fixes when a rule is consulted and what its identifier means. *)

  (** The type for rule kinds. *)
  type t = Override | Content | Room | Sender | Underride

  val all : t list
  (** [all] is every kind, in the order the kinds are consulted. *)

  val to_string : t -> string
  (** [to_string t] is the wire spelling of [t], such as ["override"]. *)

  val of_string : string -> (t, [> `Msg of string ]) result
  (** [of_string s] is the kind [s] spells. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same kind. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Rule_id : sig
  (** A rule's identifier together with the kind it is filed under.

      A room rule is identified by the room it applies to and a sender rule by
      the sender it applies to. The other three kinds take a name, which is the
      dotted form such as [".m.rule.message"] for a rule the server supplies. A
      kind and an identifier are together the path the rule sits at under
      [/pushrules/global]. *)

  type t
  (** The type for rule identifiers. *)

  val override : string -> t
  (** [override name] is the override rule called [name]. *)

  val content : string -> t
  (** [content name] is the content rule called [name]. *)

  val room : Matrix_id.Room_id.t -> t
  (** [room id] is the room rule that applies to [id]. *)

  val sender : Matrix_id.User_id.t -> t
  (** [sender id] is the sender rule that applies to events [id] sent. *)

  val underride : string -> t
  (** [underride name] is the underride rule called [name]. *)

  val v : kind:Kind.t -> string -> t
  (** [v ~kind name] is the [kind] rule called [name]. Nothing is validated, so
      a room or sender rule whose [name] is not a well-formed identifier applies
      to no event. *)

  val kind : t -> Kind.t
  (** [kind t] is the kind [t] is filed under. *)

  val id : t -> string
  (** [id t] is the identifier as it appears on the wire. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same kind and identifier.
  *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the kind and identifier of [t] on [ppf]. *)
end

(** {1 Actions} *)

module Tweak : sig
  (** A property of the notification a rule asks for.

      @see <https://spec.matrix.org/v1.11/client-server-api/#tweaks> Tweaks *)

  (** The type for tweaks. *)
  type t =
    | Sound of string
        (** The sound to play. ["default"] asks for the client's own. *)
    | Highlight of bool  (** Whether to mark the event as needing attention. *)
    | Custom of string * Jsont.json
        (** A tweak this library does not model, as its name and its value. The
            value is JSON null when the rule gave none. *)

  val name : t -> string
  (** [name t] is the wire name of [t], such as ["sound"]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same tweak with the same
      value. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the name and value of [t] on [ppf]. *)
end

module Action : sig
  (** One entry of a rule's [actions] array.

      @see <https://spec.matrix.org/v1.11/client-server-api/#actions> Actions *)

  (** The type for actions. *)
  type t =
    | Notify  (** Deliver a notification. *)
    | Dont_notify
        (** Deprecated. A rule that does not {!Notify} has the same effect. *)
    | Coalesce  (** Deprecated. Asks that notifications be grouped. *)
    | Set_tweak of Tweak.t
        (** Sets a display tweak, such as a sound or highlight. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same action. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [t] in its wire form on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. It reads both forms an action takes on
      the wire, the bare string and the [set_tweak] object. An action it does
      not recognise decodes to {!Dont_notify}. *)
end

(** {1 Conditions} *)

module Condition : sig
  (** A test an override or underride rule applies to an event.

      Every condition of a rule must apply for the rule to match. A rule with no
      conditions matches every event.

      @see <https://spec.matrix.org/v1.11/client-server-api/#conditions-1>
        Conditions *)

  module Comparison : sig
    (** The relation a member count is required to stand in. *)

    (** The type for comparisons. *)
    type t = Eq | Lt | Gt | Le | Ge

    val to_string : t -> string
    (** [to_string t] is the operator [t] is written with, such as ["<="]. *)

    val of_string : string -> (t, [> `Msg of string ]) result
    (** [of_string s] is the comparison the operator [s] writes. The empty
        string is {!Eq}, which is how a bare count is read. *)

    val equal : t -> t -> bool
    (** [equal a b] is [true] when [a] and [b] are the same comparison. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf t] prints [to_string t] on [ppf]. *)
  end

  (** The type for conditions. *)
  type t =
    | Event_match of { key : string; pattern : string }
        (** [key] is a
            {{:https://spec.matrix.org/v1.11/appendices/#dot-separated-property-paths}
             dot-separated property path} into the event, with a [.] inside a
            member name written [\\.]. [pattern] is a glob. *)
    | Event_property_is of { key : string; value : Jsont.json }
        (** The property at [key] equals [value]. *)
    | Event_property_contains of { key : string; value : Jsont.json }
        (** The array at [key] has [value] among its elements. *)
    | Contains_display_name
        (** The event body contains the user's display name as a word. *)
    | Room_member_count of { comparison : Comparison.t; count : int }
        (** The number of joined members stands in [comparison] to [count]. *)
    | Sender_notification_permission of { key : string }
        (** The sender's power level reaches the room's [notifications.<key>].
        *)
    | Other of Jsont.json
        (** A condition this library does not model, kept as it arrived. It
            applies to no event, so the rule holding it matches nothing. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same condition. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [t] in its wire form on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. A condition whose [kind] is unknown,
      and a [room_member_count] whose [is] member is not an operator followed by
      a number, decode to {!Other}. *)
end

(** {1 Rules} *)

module Rule : sig
  (** A single push rule. *)

  type t = {
    rule_id : Rule_id.t;
    default : bool;  (** The rule is one the server supplies. *)
    enabled : bool;
    actions : Action.t list;
    conditions : Condition.t list;
        (** Empty for content, room and sender rules, which carry their test in
            [pattern] and [rule_id]. *)
    pattern : string option;  (** Content rules only. The glob to match. *)
  }
  (** The type for push rules. *)

  val v :
    ?default:bool ->
    ?enabled:bool ->
    ?conditions:Condition.t list ->
    ?pattern:string ->
    rule_id:Rule_id.t ->
    Action.t list ->
    t
  (** [v ~rule_id actions] is the rule [rule_id] producing [actions]. [default]
      defaults to [false]. [enabled] defaults to [true]. [conditions] defaults
      to the empty list. [pattern] defaults to absent. *)

  val kind : t -> Kind.t
  (** [kind t] is the kind [t] is filed under. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] agree in every field. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [t] in its wire form on [ppf]. *)

  val jsont : Kind.t -> t Jsont.t
  (** [jsont kind] is the JSON codec for a rule of [kind]. The wire form has no
      member naming the kind, so it comes from the list the rule was found in.
  *)
end

module Ruleset : sig
  (** The rules of one scope, by kind. *)

  type t = {
    override : Rule.t list;
    content : Rule.t list;
    room : Rule.t list;
    sender : Rule.t list;
    underride : Rule.t list;
  }
  (** The type for rulesets. Within each list, earlier rules win. *)

  val empty : t
  (** [empty] is the ruleset with no rules of any kind. *)

  val rules : t -> Kind.t -> Rule.t list
  (** [rules t kind] is the rules of [kind] in [t], in the order they are
      consulted. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] hold the same rules in the same
      order. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [t] in its wire form on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}, the object a scope is described by. A
      kind the object omits decodes to the empty list. *)

  val global_jsont : t Jsont.t
  (** [global_jsont] is the codec for the object that wraps a ruleset in its
      scope, whose [global] member is the ruleset. [GET /pushrules/] and the
      [m.push_rules] account-data event both use it. *)
end

val default_ruleset : user_id:Matrix_id.User_id.t -> Ruleset.t
(** [default_ruleset ~user_id] is the predefined ruleset of Matrix 1.11, the
    override rules [.m.rule.master], [.m.rule.suppress_notices],
    [.m.rule.invite_for_me], [.m.rule.member_event], [.m.rule.is_user_mention],
    [.m.rule.is_room_mention], [.m.rule.tombstone], [.m.rule.reaction],
    [.m.rule.room.server_acl] and [.m.rule.suppress_edits], and the underride
    rules [.m.rule.call], [.m.rule.encrypted_room_one_to_one],
    [.m.rule.room_one_to_one], [.m.rule.message] and [.m.rule.encrypted]. The
    rules that name the user, such as [.m.rule.invite_for_me], name [user_id].

    Use it until the homeserver's own ruleset has been read.

    @see <https://spec.matrix.org/v1.11/client-server-api/#predefined-rules>
      Predefined Rules *)
