(** Structured IMAP client failures. *)
type t =
  | Closed
  | Protocol of string
  | Transport of string
  | Rejected of { tag : string; status : [ `No | `Bad ];
      code : Imap.Response.code option; text : string }
  | State of string
  | Missing_uid of int64
  | Limit of string
  | Uncertain of string

(** [Rejected] retains the tagged response code separately from explanatory
    text. Known codes are typed; extension codes use [Imap.Response.Other_code].
    No code is represented by [None]. A rejection does not itself authorize
    retry: partial mutations may instead return [Uncertain]. Authentication
    failures retain only a whitelist of standard codes without payloads and
    replace server text with a fixed diagnostic, so echoed credentials cannot
    enter the public error through arbitrary code parameters or text. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf e] prints [e] for diagnostics. *)

val to_string : t -> string
(** [to_string e] is [e] printed by {!pp}. *)
