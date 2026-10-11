(** Values shared by the worker and frontend. *)

type source = { service : string; account : string; id : string }
[@@deriving sexp]

type dimension = Area | Triage | Delivery | Attention [@@deriving sexp]
type origin = Model | Rule | Admin | Server [@@deriving sexp]

type message = {
  source : source;
  thread : string;
  date : string;
  sender : string;
  recipients : string list;
  subject : string;
  original : string;
  text : string;
  headers : (string * string) list;
  message_ids : string list;
  reply_ids : string list;
  truncated : bool;
  outside_interval : bool;
  revision : string;
}
[@@deriving sexp]

type area = {
  id : string;
  name : string;
  definition : string;
  exclusions : string;
  parent : string option;
  status : string;
  revision : int;
}
[@@deriving sexp]

type assignment = {
  target : source;
  conversation : bool;
  dimension : dimension;
  value : string;
  origin : origin;
  version : string;
  source_revision : string;
  explanation : string;
  evidence : source list;
  score : float option;
}
[@@deriving sexp]

type snapshot = {
  revision : int;
  messages : message list;
  areas : area list;
  assignments : assignment list;
  jobs : string list;
  discussion : string;
}
[@@deriving sexp]

type edit =
  | Approve of string
  | Rename of string * string
  | Archive of string
  | Merge of string * string
  | Define of string * string * string
  | Assign of assignment
  | Undo
[@@deriving sexp]

type request =
  | Inspect of string
  | Discuss of string * source option
  | Edit of int * edit * string * string
[@@deriving sexp]

type response = (snapshot, string) result [@@deriving sexp]

val empty : snapshot
(** [empty] is an empty corpus view. *)

val key : source -> string
(** [key source] encodes all three identity components without collisions. *)

val dimension : dimension -> string
(** [dimension d] is the stable storage name of [d]. *)

val valid_value : dimension -> string -> bool
(** [valid_value d value] validates a label in [d]. Area IDs need store checks. *)

val sanitize : string -> string
(** [sanitize text] removes terminal controls and bidi formatting. *)

val source_jsont : source Jsont.t
val message_jsont : message Jsont.t

type ui = {
  snapshot : snapshot;
  selected : source option;
  scroll : int;
  pending : int option;
  serial : int;
  error : string option;
}
[@@deriving sexp]

val initial : ui

val begin_request : ui -> (ui * int) option
(** [begin_request ui] starts one numbered operation unless already busy. *)

val receive : ui -> int -> response -> ui
(** [receive ui id response] ignores stale requests and preserves selection. *)
