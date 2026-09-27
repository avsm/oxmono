@@ portable

(** IMAP client failures, documented in [Imap_eio.Error]. *)

type t =
  | Closed
  | Protocol of string
  | Transport of string
  | Rejected of { tag : string; status : [ `No | `Bad ];
      code : Imap.Response.code option; text : string }
  | State of string
  | Missing_uid of Imap.Uid.t
  | Limit of string
  | Uncertain of string
  | Unsupported of Imap.Capability.t
  | Not_enabled of Imap.Capability.t

val pp : Format.formatter -> t -> unit
val to_string : t -> string
