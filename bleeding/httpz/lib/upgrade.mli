(** HTTP Upgrade protocol identifiers and offer lists.

    Upgrade syntax is defined by
    {{:https://www.rfc-editor.org/rfc/rfc9110.html#section-7.8}RFC 9110, Section
    7.8}. Protocol names compare without regard to ASCII case; protocol
    versions compare exactly. *)

val[@zero_alloc] valid_protocol : local_ string -> bool @@ portable
(** [valid_protocol value] is [true] when [value] is one protocol name with an
    optional [/version], where both components are nonempty HTTP tokens. *)

val[@zero_alloc] valid_protocol_list : local_ string -> bool @@ portable
(** [valid_protocol_list value] is [true] when [value] is a nonempty,
    comma-separated list of protocols. Optional whitespace around members is
    accepted; empty or malformed members are rejected. *)

val[@zero_alloc] matches_offer :
  offer:local_ string -> selected:local_ string -> bool @@ portable
(** [matches_offer ~offer ~selected] is [true] when [offer] is a valid protocol
    list containing [selected]. The selected value must itself be one valid
    protocol. Empty members in [offer] are ignored as required for received HTTP
    lists. Protocol names compare without case and versions exactly. *)
