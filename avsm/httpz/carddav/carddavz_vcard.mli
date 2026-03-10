(** vCard 4.0 parser and serializer.

    Parses and serializes vCard data per
    {{:https://datatracker.ietf.org/doc/html/rfc6350}RFC 6350}. *)

(** {1 Types} *)

type param_value =
  | Ptext of string
  | Pquoted of string

type param = {
  pname : string;
  pvalues : param_value list;
}

type property = {
  group : string option;
  name : string;
  params : param list;
  value : string;
}
(** A vCard content line: [group.name;param1=v1;param2=v2:value]. *)

type t = {
  properties : property list;
}
(** A single vCard (BEGIN:VCARD ... END:VCARD). *)

(** {1 Parsing} *)

val parse : string -> (t, string) result
(** [parse s] parses a vCard string.
    Handles line unfolding (CRLF + WSP continuation per RFC 6350 Section 3.2). *)

(** {1 Serialization} *)

val to_string : t -> string
(** [to_string vcard] serializes a vCard to a string with CRLF line endings. *)

(** {1 Accessors} *)

val uid : t -> string option
(** [uid vcard] returns the UID property value. *)

val fn : t -> string option
(** [fn vcard] returns the FN (formatted name) property value. *)

val emails : t -> string list
(** [emails vcard] returns all EMAIL property values. *)

val tels : t -> string list
(** [tels vcard] returns all TEL property values. *)

val version : t -> string option
(** [version vcard] returns the VERSION property value. *)
