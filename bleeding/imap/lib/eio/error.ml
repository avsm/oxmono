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
