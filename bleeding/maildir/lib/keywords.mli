(** Dovecot filename keyword mappings and Maildir flag letters. *)

type t
(** The type of a mapping from the 26 keyword letters [a] to [z] to keywords. *)

type error = Maildir_error.t
(** The type of errors, which {!Maildir.error} re-exports. *)

val max_size : int
(** [max_size] is the largest accepted [dovecot-keywords] file, 64 KiB. *)

val empty : t
(** [empty] is the mapping with no keywords. *)

val validate_flags : Mail_flag.Imap_flag.t list -> (unit, error) result
(** [validate_flags flags] is [Error (Unsupported_flag f)] for the first
    flag [f] in [flags] that is [\Recent] or a system flag Maildir cannot
    store. *)

val parse : string -> (t, error) result
(** [parse raw] is the mapping in the [dovecot-keywords] contents [raw]. Blank
    lines and a missing final newline are accepted. A malformed line, an index
    outside 0 to 25 and a duplicate index or name are [Keyword_map]
    errors. *)

val encode : t -> (string, error) result
(** [encode mapping] is the [dovecot-keywords] contents for [mapping]. A
    result larger than {!max_size} is a [Keyword_map] error. *)

val add : t -> Mail_flag.Imap_flag.t list -> (t, error) result
(** [add mapping flags] is [mapping] with a free slot assigned to each keyword
    in [flags] it lacks. Existing slots are unchanged. An invalid flag is
    [Unsupported_flag], a keyword with no free slot is [Too_many_keywords],
    and an encoding larger than {!max_size} is [Keyword_map]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] map every slot to the same keyword. *)

val letters : ?passed:bool -> t -> Mail_flag.Imap_flag.t list -> string
(** [letters mapping flags] is the sorted, duplicate-free Maildir info letters
    for [flags]. [passed] adds the Passed letter [P] and defaults to [false].

    @raise Invalid_argument if a keyword in [flags] has no slot in
    [mapping]. *)

val flags : t -> file:string -> string ->
  (Mail_flag.Imap_flag.t list, error) result
(** [flags mapping ~file letters] is the durable flag list that the info
    letters [letters] of entry [file] denote. [P] is ignored. An unmapped
    keyword letter or an unknown letter is an [Unknown_letter] error naming
    [file] and the letter. *)
