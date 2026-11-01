@@ portable

(** Zulip organization users.

    User values contain identity, account state, optional bot metadata, and
    custom profile fields. Decoded users preserve unrecognized object members.
*)

module Role : sig
  type t =
    | Owner
    | Administrator
    | Moderator
    | Member
    | Guest
    | Other of int
        (** The type for organization roles. The known constructors have wire
            values [100], [200], [300], [400], and [600]. [Other n] preserves
            the unknown wire value [n]. *)

  val of_int : int -> t
  (** [of_int n] is the role represented by [n]. Unknown values produce
      [Other n]. *)

  val to_int : t -> int
  (** [to_int role] is the wire value of [role]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have equal wire values. *)

  val compare : t -> t -> int
  (** [compare a b] orders [a] and [b] by their wire values. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf role] writes the decimal wire value of [role] to [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is a codec for exactly representable integer roles. Unknown
      integers decode as [Other n] and encode unchanged. *)
end

module Bot_type : sig
  type t =
    | Generic
    | Incoming_webhook
    | Outgoing_webhook
    | Embedded
    | Other of int
        (** The type for bot integrations. The known constructors have wire
            values [1] through [4]. [Other n] preserves the unknown wire value
            [n]. *)

  val of_int : int -> t
  (** [of_int n] is the bot type represented by [n]. Unknown values produce
      [Other n]. *)

  val to_int : t -> int
  (** [to_int bot_type] is the wire value of [bot_type]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have equal wire values. *)

  val compare : t -> t -> int
  (** [compare a b] orders [a] and [b] by their wire values. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf bot_type] writes the decimal wire value of [bot_type] to [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is a codec for exactly representable integer bot types. Unknown
      integers decode as [Other n] and encode unchanged. *)
end

type profile_field_value = { value : string; rendered_value : string option }
(** The type for a custom profile field value. [rendered_value] is [None] when
    the wire object omits its rendered form. *)

type t
(** The type for organization users. *)

val create :
  user_id:Id.User.t ->
  email:string ->
  full_name:string ->
  ?delivery_email:string ->
  ?is_active:bool ->
  ?is_admin:bool ->
  ?is_owner:bool ->
  ?is_guest:bool ->
  ?is_billing_admin:bool ->
  ?is_bot:bool ->
  ?bot_type:Bot_type.t ->
  ?bot_owner_id:Id.User.t ->
  ?avatar_url:string ->
  ?avatar_version:int ->
  ?timezone:string ->
  ?date_joined:string ->
  ?role:Role.t ->
  ?is_imported_stub:bool ->
  ?is_deleted:bool ->
  ?profile_data:(Id.Profile_field.t * profile_field_value) list ->
  unit ->
  t
(** [create ~user_id ~email ~full_name ()] is a user identified by [user_id]
    with [email] and [full_name]. [is_active] defaults to [true]. [is_admin],
    [is_owner], [is_guest], [is_billing_admin], [is_bot], [is_imported_stub],
    and [is_deleted] default to [false]. All other optional arguments default to
    absence. *)

val user_id : t -> Id.User.t
(** [user_id user] is the identifier of [user]. *)

val email : t -> string
(** [email user] is the API email address of [user]. *)

val full_name : t -> string
(** [full_name user] is the display name of [user]. *)

val delivery_email : t -> string option
(** [delivery_email user] is the email delivery address, or [None] if the wire
    member is absent or null. *)

val is_active : t -> bool
(** [is_active user] is [true] if the account is active. A missing wire member
    decodes as [true]. *)

val is_admin : t -> bool
(** [is_admin user] is [true] if the account has administrator privileges. A
    missing wire member decodes as [false]. *)

val is_owner : t -> bool
(** [is_owner user] is [true] if the account has organization owner privileges.
    A missing wire member decodes as [false]. *)

val is_guest : t -> bool
(** [is_guest user] is [true] if the account is a guest account. A missing wire
    member decodes as [false]. *)

val is_billing_admin : t -> bool
(** [is_billing_admin user] is the legacy billing administrator state. A missing
    wire member, including responses from Zulip 10.0 and later, decodes as
    [false]. *)

val is_bot : t -> bool
(** [is_bot user] is [true] if the account is a bot. A missing wire member
    decodes as [false]. *)

val bot_type : t -> Bot_type.t option
(** [bot_type user] is the integration type of the bot account, or [None] if the
    wire member is absent or null. *)

val bot_owner_id : t -> Id.User.t option
(** [bot_owner_id user] is the identifier of the bot's owner, or [None] if the
    wire member is absent or null. *)

val avatar_url : t -> string option
(** [avatar_url user] is the avatar URL, or [None] if the wire member is absent
    or null. *)

val avatar_version : t -> int option
(** [avatar_version user] is the avatar version, or [None] if the wire member is
    absent or null. *)

val timezone : t -> string option
(** [timezone user] is the user's time zone name, or [None] if it is absent. *)

val date_joined : t -> string option
(** [date_joined user] is the server-supplied account creation timestamp, or
    [None] if it is absent. *)

val role : t -> Role.t option
(** [role user] is the organization role, or [None] if it is absent. *)

val is_imported_stub : t -> bool
(** [is_imported_stub user] is [true] if the account is an imported stub. A
    missing wire member decodes as [false]. *)

val is_deleted : t -> bool
(** [is_deleted user] is [true] if the account has been deleted. A missing wire
    member decodes as [false]. *)

val profile_data : t -> (Id.Profile_field.t * profile_field_value) list option
(** [profile_data user] is the custom profile data keyed by nominal field
    identifier, or [None] if it is absent. *)

val raw : t -> Jsont.json
(** [raw user] is the original JSON object for a decoded [user]. For a user made
    by {!create}, it is an encoded object containing the modeled fields. *)

val profile_field_value_jsont : profile_field_value Jsont.t
(** [profile_field_value_jsont] is a codec for custom profile field value
    objects. The [value] member is required and [rendered_value] is optional. *)

val jsont : t Jsont.t
(** [jsont] is a codec for user objects. It preserves the complete input object
    when decoding and emits that object unchanged when encoding the result.
    Custom profile field keys must be decimal nonnegative identifiers in the
    exactly representable JSON integer range. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf user] writes the identifier, email address, and full name of [user]
    to [ppf]. *)
