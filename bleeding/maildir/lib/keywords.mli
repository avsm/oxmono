@@ portable

(** Dovecot keyword maps and Maildir flag letters.

    A Maildir filename carries its flags as letters after [:2,]. [D], [F],
    [R], [S] and [T] stand for Draft, Flagged, Answered, Seen and Deleted,
    and [P] for Passed, which has no IMAP flag. The lowercase letters [a]
    to [z] stand for the keywords that [dovecot-keywords] maps to slots 0
    to 25. *)

type t : immutable_data
(** The type for maps from the 26 keyword letters to keywords. A map is
    immutable data, so it may be shared between domains. *)

type error = Maildir_error.t
(** The type for errors, which {!Maildir.error} re-exports. *)

val max_size : int
(** [max_size] is the size of the largest accepted [dovecot-keywords]
    file, 65,536 bytes. *)

val empty : t
(** [empty] is the map with no keywords. *)

val validate_flags : Mail_flag.Imap_flag.t list -> (unit, error) result
(** [validate_flags flags] is [Ok ()] if Maildir can store every flag of
    [flags]. Otherwise it is [Error (Unsupported_flag f)] for the first
    flag [f] of [flags] that is [\Recent] or a system flag Maildir cannot
    store. *)

val parse : string -> (t, error) result
(** [parse raw] is the map in the [dovecot-keywords] contents [raw], one
    line [index name] per keyword. Blank lines and a missing final newline
    are accepted. The [Keyword_map] error covers a line without a space, an
    index outside 0 to 25 or not in canonical decimal form, an invalid
    keyword name and a repeated index or name. *)

val encode : t -> (string, error) result
(** [encode m] is the [dovecot-keywords] contents for [m], one line per
    keyword in slot order. A result larger than {!max_size} is a
    [Keyword_map] error. *)

val add : t -> Mail_flag.Imap_flag.t list -> (t, error) result
(** [add m flags] is [m] with the lowest free slot assigned to each keyword
    of [flags] that [m] lacks. Existing slots are unchanged. The error is
    [Unsupported_flag] for a flag {!validate_flags} rejects,
    [Too_many_keywords] for a keyword with no free slot and [Keyword_map]
    for a map whose encoding is larger than {!max_size}. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] map every slot to the same
    keyword. *)

val letters : ?passed:bool -> t -> Mail_flag.Imap_flag.t list -> string
(** [letters ~passed m flags] is the filename letters for [flags] under
    [m], in ASCII order without repeats. [\Recent] and system flags Maildir
    cannot store are left out. [passed] adds the Passed letter [P] and
    defaults to [false].

    @raise Invalid_argument if a keyword of [flags] has no slot in [m]. *)

val flags : t -> file:string -> string ->
  (Mail_flag.Imap_flag.t list, error) result
(** [flags m ~file letters] is the flags that the filename letters
    [letters] of the entry [file] denote under [m], normalised by
    [Mail_flag.Imap_flag.durable]. [P] is ignored. An unmapped keyword
    letter or any other unknown letter is an [Unknown_letter] error naming
    [file] and the letter. *)
