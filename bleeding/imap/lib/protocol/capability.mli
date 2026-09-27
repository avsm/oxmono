(** IMAP capability tokens and sets.

    A capability is a token a server advertises in a CAPABILITY response or
    confirms in an ENABLED response. Token names compare
    case-insensitively, RFC 9051 §7.2.2. Parsing never fails. A token this
    library does not know stays available as [Other] in its received
    spelling. *)

type thread_algorithm = Thread.algorithm
(** The type for RFC 5256 THREAD algorithms, equal to
    {!Thread.algorithm}. *)

type t =
  | Imap4rev1
  | Imap4rev2
  | Auth of string
      (** [AUTH=m], with the SASL mechanism name [m] in uppercase. *)
  | Login_disabled  (** [LOGINDISABLED]. *)
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
  | Literal_plus  (** [LITERAL+]. *)
  | Literal_minus  (** [LITERAL-]. *)
  | Multiappend
  | Searchres
  | Esearch
  | Sort
  | Sort_display  (** [SORT=DISPLAY]. *)
  | Esort
  | Context of [ `Search | `Sort ]  (** [CONTEXT=SEARCH] or [CONTEXT=SORT]. *)
  | Thread of Thread.algorithm  (** [THREAD=a]. *)
  | Partial
  | Preview
  | Objectid
  | Objectid_plus
      (** The draft [OBJECTID+] token, distinct from RFC 8474 [OBJECTID]. *)
  | Uidonly
  | Uidbatches
  | Messagelimit of int64
      (** [MESSAGELIMIT=n], with [n] from 1 to 4294967295. *)
  | Savelimit of int64  (** [SAVELIMIT=n], with [n] from 1 to 4294967295. *)
  | Utf8 of [ `Accept | `Only ]  (** [UTF8=ACCEPT] or [UTF8=ONLY]. *)
  | Compress of [ `Deflate ]  (** [COMPRESS=DEFLATE]. *)
  | Acl
  | Quota
  | Quota_res of string
      (** [QUOTA=RES-r], with the resource name [r] in uppercase. *)
  | Quotaset
  | Metadata
  | Metadata_server  (** [METADATA-SERVER]. *)
  | Notify
  | List_extended
  | List_status
  | Special_use
  | Status_size  (** [STATUS=SIZE]. *)
  | Jmapaccess
  | Id
  | Children
  | Language
  | Other of string
      (** A token this library does not know, in its received spelling. *)
(** The type for capabilities. A constructor without a doc stands for the
    token its name spells in uppercase with [_] written as [-], such as
    [SASL-IR] for [Sasl_ir]. *)

val of_wire : string -> t
(** [of_wire s] is the capability named by the token [s], read
    case-insensitively. An unknown token is [Other s]. So is a known name
    with a malformed parameter, such as [MESSAGELIMIT=x] or an empty
    [AUTH=]. The parameters of [MESSAGELIMIT] and [SAVELIMIT] must be
    decimal numbers from 1 to 4294967295 without leading zeros. *)

val to_wire : t -> string
(** [to_wire c] is the canonical uppercase token for [c], or the received
    spelling of an [Other] token. [of_wire (to_wire c) = c] for every [c]
    that {!of_wire} returns. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] spell the same token ignoring
    case. [Other "idle"] equals [Idle]. *)

val compare : t -> t -> int
(** [compare a b] is a total order on capabilities, compatible with
    {!equal}. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf c] prints [to_wire c] on [ppf]. *)

val implied_by_rev2 : t -> bool
(** [implied_by_rev2 c] is [true] if IMAP4rev2 folds [c] into the base
    protocol, RFC 9051 Appendix E items 2 and 3. These are [Enable],
    [Idle], [Namespace], [Uidplus], [Move], [Searchres], [Esearch],
    [List_extended], [List_status], [Unselect], [Sasl_ir], [Literal_minus]
    and [Status_size], and an [Other] token that spells one of them.
    [Binary] is not among them, since IMAP4rev2 folds in only its FETCH
    side. *)

val malformed_limit : t -> bool
(** [malformed_limit c] is [true] if [c] is an [Other] token spelled
    [MESSAGELIMIT=v] or [SAVELIMIT=v] whose [v] {!of_wire} rejected. *)

(** Capability sets. *)
module Set : sig
  type elt = t
  (** The type for set elements. *)

  type t
  (** The type for sets of capabilities under {!equal}. An [Other] token is
      stored as {!of_wire} reads it, so [Other "idle"] is stored as
      [Idle]. *)

  val empty : t
  (** [empty] is the set with no capabilities. *)

  val is_empty : t -> bool
  (** [is_empty s] is [true] if [s] has no capabilities. *)

  val of_list : elt list -> t
  (** [of_list l] is the set of the capabilities of [l]. *)

  val to_list : t -> elt list
  (** [to_list s] is the elements of [s] in {!compare} order. *)

  val mem : elt -> t -> bool
  (** [mem c s] is [true] if [s] holds a capability equal to [c]. *)

  val add : elt -> t -> t
  (** [add c s] is [s] with [c] added. *)

  val union : t -> t -> t
  (** [union a b] is the capabilities in [a] or in [b]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] hold the same capabilities. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf s] prints the tokens of [s] on [ppf], separated by
      spaces. *)
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
