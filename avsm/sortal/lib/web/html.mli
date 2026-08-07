(** HTML fragments built from the stdlib alone.

    Handlers are portable, so nothing here may reach outside [Buffer],
    [Printf] and [String]. Every value a page interpolates goes through
    {!escape}, and every path segment through {!pct_segment}. *)

val escape : string -> string @@ portable
(** [escape s] is [s] with ['&'], ['<'], ['>'], ['"'] and ['\''] replaced by
    their character references, so it is safe in element content and in a
    double-quoted attribute alike. *)

val pct_segment : string -> string @@ portable
(** [pct_segment s] is [s] percent-encoded for one path segment. Everything
    outside the unreserved set of RFC 3986 is encoded, so a handle containing
    ['/'], ['?'] or ['#'] cannot break out of its segment. *)

val add_escaped : Buffer.t -> string -> unit @@ portable
(** [add_escaped b s] appends {!escape}[ s] to [b] without the intermediate
    string. *)

val page : title:string -> query:string -> string -> string @@ portable
(** [page ~title ~query body] is a complete document with [body] as the
    contents of [main]. [title] is the browser title, shown unescaped nowhere.
    [query] prefills the header search box, which is empty when it is [""]. *)

val css : string @@ portable
(** [css] is the whole stylesheet, served at [/static/style.css]. It is the
    only asset, and it is embedded so a running server needs no data files. *)
