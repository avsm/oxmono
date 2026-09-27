(** Dovecot filename keyword mappings and Maildir flag letters. *)

type t
(** The type of a mapping from the 26 keyword letters [a] to [z] to keywords. *)

val fail : string -> 'a
(** [fail message] raises [Failure] with [message] under the library prefix. *)

val max_size : int
(** [max_size] is the largest accepted [dovecot-keywords] file, 64 KiB. *)

val empty : t
(** [empty] is the mapping with no keywords. *)

val validate_flags : Mail_flag.Imap_flag.t list -> unit
(** [validate_flags flags] raises [Failure] if [flags] contains [\Recent] or a
    system flag Maildir cannot store. *)

val parse : string -> t
(** [parse raw] is the mapping in the [dovecot-keywords] contents [raw]. Blank
    lines and a missing final newline are accepted. A malformed line, an index
    outside 0 to 25 and a duplicate index or name raise [Failure]. *)

val encode : t -> string
(** [encode mapping] is the [dovecot-keywords] contents for [mapping]. It
    raises [Failure] if the result exceeds {!max_size}. *)

val add : t -> Mail_flag.Imap_flag.t list -> t
(** [add mapping flags] is [mapping] with a free slot assigned to each keyword
    in [flags] it lacks. Existing slots are unchanged. It raises [Failure] if
    [flags] is invalid, the slots run out or the encoding is too large. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] map every slot to the same keyword. *)

val letters : ?passed:bool -> t -> Mail_flag.Imap_flag.t list -> string
(** [letters mapping flags] is the sorted, duplicate-free Maildir info letters
    for [flags]. [passed] adds the Passed letter [P] and defaults to [false].
    A keyword without a slot in [mapping] raises [Failure]. *)

val flags : t -> file:string -> string -> Mail_flag.Imap_flag.t list
(** [flags mapping ~file letters] is the durable flag list that the info
    letters [letters] of entry [file] denote. [P] is ignored. An unmapped
    keyword letter or an unknown letter raises [Failure] naming [file]. *)
