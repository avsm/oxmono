(** matching — caseless, diacritic-insensitive text matching.

    Every function here compares through {!search_key}, so matching ignores
    case, canonical and compatibility spelling and diacritics, and an empty
    pattern matches everything. *)

val search_key : string -> string
(** [search_key s] is a normalized, case-folded, diacritic-free key for [s]. Two
    strings share a key when they differ only by case, by canonical or
    compatibility normalization, or by combining marks, so ["cafe"], ["Café"]
    and ["CAFE\u{0301}"] all key alike, and so do ["STRASSE"] and ["Straße"].
    Keying is lossy for scripts that write vowels as marks. *)

val contains : haystack:string -> needle:string -> bool
(** [contains ~haystack ~needle] is [true] when the {!search_key} of [needle]
    occurs in the {!search_key} of [haystack] as a substring. *)

val fuzzy_score : haystack:string -> needle:string -> int option
(** [fuzzy_score ~haystack ~needle] is [None] when the characters of [needle] do
    not all appear in [haystack] in order, and otherwise a score, the higher the
    better the match. So ["mtx"] matches ["matrix"], ["mxt"] does not, and
    ["mat"] scores higher on ["matrix"] than on ["mxaxtrix"].

    A match earns points, a character starting a word earns more, a run of
    adjacent matches earns more again, and a gap between matches costs. Only the
    ordering the scores induce is meaningful, and only among haystacks judged by
    one needle. *)

val truncate_graphemes : max:int -> string -> string
(** [truncate_graphemes ~max s] is the first [max] extended grapheme clusters of
    [s], so that an emoji or a combining sequence is never split.

    Raises [Invalid_argument] if [max] is negative. *)
