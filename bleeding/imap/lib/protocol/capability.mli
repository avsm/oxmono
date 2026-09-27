(** IMAP capability tokens as advertised in CAPABILITY and confirmed in
    ENABLED responses. Token names compare case-insensitively (RFC 9051
    section 7.2.2). *)

type thread_algorithm = Thread.algorithm
(** RFC 5256 THREAD algorithms, the same type as {!Thread.algorithm}. *)

type t =
  | Imap4rev1
  | Imap4rev2
  | Auth of string
      (** [AUTH=m], with the SASL mechanism name [m] in uppercase. *)
  | Login_disabled
  | Starttls
  | Sasl_ir
  | Enable
  | Condstore
  | Qresync
  | Uidplus
  | Move
  | Binary
  | Idle
  | Namespace
  | Unselect
  | Literal_plus
  | Literal_minus
  | Multiappend
  | Searchres
  | Esearch
  | Sort
  | Sort_display
  | Esort
  | Context of [ `Search | `Sort ]
  | Thread of Thread.algorithm
  | Partial
  | Preview
  | Objectid
  | Objectid_plus
      (** The draft [OBJECTID+] token, distinct from RFC 8474 [OBJECTID]. *)
  | Uidonly
  | Uidbatches
  | Messagelimit of int64
  | Savelimit of int64
  | Utf8 of [ `Accept | `Only ]
  | Compress of [ `Deflate ]
  | Acl
  | Quota
  | Quota_res of string
      (** [QUOTA=RES-r], with the resource name [r] in uppercase. *)
  | Quotaset
  | Metadata
  | Metadata_server
  | Notify
  | List_extended
  | List_status
  | Special_use
  | Status_size
  | Jmapaccess
  | Id
  | Children
  | Language
  | Other of string
      (** A token this library does not know, in its received spelling. *)

val of_wire : string -> t
(** [of_wire s] is the capability named by the token [s], read
    case-insensitively. It never fails. An unknown token, and a known name
    whose parameter is malformed such as [MESSAGELIMIT=x], is [Other s]. The
    parameters of [MESSAGELIMIT] and [SAVELIMIT] must be decimal numbers in
    1..4294967295 without leading zeros. *)

val to_wire : t -> string
(** [to_wire c] is the canonical uppercase token for [c], or the received
    spelling of an [Other] token. [of_wire (to_wire c) = c] for every [c]
    that {!of_wire} returns. *)

val equal : t -> t -> bool
(** [equal a b] holds when [a] and [b] have the same token, ignoring case.
    [Other "idle"] equals [Idle]. *)

val compare : t -> t -> int
(** [compare a b] is a total order consistent with {!equal}. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints {!to_wire}. *)

val implied_by_rev2 : t -> bool
(** [implied_by_rev2 c] holds when IMAP4rev2 folds [c] into the base
    protocol (RFC 9051 Appendix E items 2 and 3). These are [Enable], [Idle],
    [Namespace], [Uidplus], [Move], [Searchres], [Esearch], [List_extended],
    [List_status], [Unselect], [Sasl_ir], [Literal_minus] and [Status_size].
    [Binary] is not among them, since IMAP4rev2 folds in only its FETCH
    side. *)

val malformed_limit : t -> bool
(** [malformed_limit c] holds when [c] is an [Other] token spelled
    [MESSAGELIMIT=v] or [SAVELIMIT=v] whose [v] {!of_wire} rejected. *)

module Set : sig
  type elt = t
  type t
  (** A set of capabilities under {!equal}. [of_list] and [add] store an
      [Other] token as {!of_wire} reads it, so [Other "idle"] is stored as
      [Idle]. *)

  val empty : t
  val is_empty : t -> bool
  val of_list : elt list -> t
  val to_list : t -> elt list
  (** [to_list s] is the elements of [s] in {!compare} order. *)

  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val union : t -> t -> t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  (** [pp] prints the tokens separated by spaces. *)
end

val messagelimit : Set.t -> int64 option
(** [messagelimit s] is the smallest [MESSAGELIMIT] in [s], if any. *)

val savelimit : Set.t -> int64 option
(** [savelimit s] is the smallest [SAVELIMIT] in [s], if any. *)

val auth_mechanisms : Set.t -> string list
(** [auth_mechanisms s] is the uppercase SASL mechanism of every [Auth] in
    [s]. *)

val thread_algorithms : Set.t -> thread_algorithm list
(** [thread_algorithms s] is the algorithm of every [Thread] in [s]. *)

val quota_resources : Set.t -> string list
(** [quota_resources s] is the uppercase resource of every [Quota_res] in
    [s]. *)
