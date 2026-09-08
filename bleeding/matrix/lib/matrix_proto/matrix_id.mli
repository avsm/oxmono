@@ portable

(** Matrix identifiers, parsed and validated.

    Every identifier here satisfies {!S}. [of_string] validates and reports the
    fault as [`Msg], [of_string_exn] raises [Invalid_argument] instead,
    [to_string] renders the canonical form, and [equal] and [compare] order
    values by that form. Identifiers are case-sensitive, and
    [to_string (of_string_exn s) = s] holds for every [s] that parses.

    @see <https://spec.matrix.org/v1.11/appendices/#identifier-grammar>
      Identifier Grammar *)

(** {1 The common identifier interface} *)

(** The operations every Matrix identifier provides. *)
module type S = sig
  @@ portable
  type t : immutable_data
  (** The type for identifiers. *)

  val of_string : string -> (t, [> `Msg of string ]) result
  (** [of_string s] is the identifier [s] denotes. The error message names the
      fault. *)

  val of_string_exn : string -> t
  (** [of_string_exn s] is [of_string s].

      Raises [Invalid_argument] if [s] is not an identifier of this kind. *)

  val to_string : t -> string
  (** [to_string t] is the canonical rendering of [t]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] render the same. *)

  val compare : t -> t -> int
  (** [compare a b] orders [a] and [b] by their rendering. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. It reads and writes a string. *)
end

(** {1 Identifiers} *)

module Server_name : sig
  (** A homeserver name. It is a hostname, an IPv4 address or a bracketed IPv6
      address, with an optional numeric [:port]. The original spelling and case
      are preserved.

      @see <https://spec.matrix.org/v1.11/appendices/#server-name> Server Name
  *)

  include S
end

module User_id : sig
  (** A user identifier, [@localpart:server_name].

      The localpart is normally non-empty. The specification permits the
      lowercase letters, the digits and [._=-/+]. Historical identifiers
      containing any valid Unicode scalar other than [:] and NUL, including an
      empty localpart, are accepted too. [of_string] admits both forms and
      {!is_spec_conformant} tells them apart.

      @see <https://spec.matrix.org/v1.11/appendices/#user-identifiers>
        User Identifiers *)

  include S

  val localpart : t -> string
  (** [localpart t] is the part of [t] between the sigil and the colon. *)

  val server_name : t -> Server_name.t
  (** [server_name t] is the homeserver [t] is registered on. *)

  val is_spec_conformant : t -> bool
  (** [is_spec_conformant t] is [true] if the localpart of [t] uses only the
      characters the specification permits for new identifiers. *)
end

module Room_id : sig
  (** A room identifier, [!opaque_id:server_name] or a domainless [!opaque_id].

      The opaque part is assigned by the server that created the room and is not
      interpreted here. The server name records only the room's origin. It says
      nothing about where the room is now reachable.

      @see <https://spec.matrix.org/v1.11/appendices/#room-ids> Room IDs *)

  include S

  val opaque_id : t -> string
  (** [opaque_id t] is the part of [t] after the sigil, before the optional
      colon and server name. *)

  val server_name : t -> Server_name.t option
  (** [server_name t] is the server the room was created on, when this is a
      legacy domain-bearing room ID. Domainless room IDs return [None]. *)
end

module Event_id : sig
  (** An event identifier. It is [$opaque_id:server_name] in room versions 1 to
      3 and [$base64_opaque_id] from room version 4 on.

      [of_string] tells the two apart by whether a colon follows the sigil, and
      both render back to what was parsed.

      @see <https://spec.matrix.org/v1.11/appendices/#event-ids> Event IDs *)

  include S
end

module Room_alias : sig
  (** A room alias, [#alias:server_name].

      An alias is a mutable pointer to a room, managed by the server in its
      domain. Resolve it through the directory before using it where a
      {!Room_id.t} is wanted.

      @see <https://spec.matrix.org/v1.11/appendices/#room-aliases> Room Aliases
  *)

  include S

  val alias : t -> string
  (** [alias t] is the part of [t] between the sigil and the colon. *)

  val server_name : t -> Server_name.t
  (** [server_name t] is the server that publishes the alias. *)
end

module Device_id : sig
  (** A device identifier. It is an opaque non-empty string chosen by the client
      at login or assigned by the server. *)

  include S
end

module Session_id : sig
  (** A Megolm session identifier. It is an opaque non-empty string naming the
      ratchet a room event was encrypted with.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mmegolmv1aes-sha2>
        m.megolm.v1.aes-sha2 *)

  include S
end

module Transaction_id : sig
  (** A client-chosen identifier that makes a request idempotent. A server that
      sees the same one twice from the same device answers with the first
      outcome instead of acting again.

      The specification puts no shape on the value beyond its being non-empty.
      What matters is that it is unpredictable and reused across every retry of
      one logical request.

      @see <https://spec.matrix.org/v1.11/client-server-api/#transaction-identifiers>
        Transaction Identifiers *)

  include S

  val v : string -> t
  (** [v s] is [s] as a transaction identifier, without validation. It is for an
      identifier that already exists, such as one read back from a persisted
      send queue. *)

  val of_bytes : string -> t
  (** [of_bytes b] is the lowercase hexadecimal encoding of [b]. [b] must come
      from a cryptographically secure source. *)
end
