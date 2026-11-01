(** Checked Matrix endpoint-path templates.

    This is a narrow wrapper around [Httpz_uri.Template]. Each placeholder is
    one path segment: values are encoded with Matrix/Ruma path semantics before
    expansion, so reserved identifier characters such as [!] and [:] remain
    readable while [/], [?], [#], [%] and invalid delimiters cannot change the
    route. *)

type t
(** A pre-parsed absolute endpoint-path template. *)

val v : string -> t
(** [v source] parses [source]. Placeholders use the simple [{name}] spelling.
    Literal query and fragment delimiters and non-simple URI-template
    expressions are rejected.

    Raises [Invalid_argument] if [source] is not a valid absolute path template.
*)

val variables : t -> string list
(** [variables route] is the unique placeholder names in first-use order. *)

val expand : t -> (string * string) list -> (string, string) result
(** [expand route bindings] expands every placeholder as one encoded path
    segment. Missing, duplicate, unknown and invalid-UTF-8 bindings are
    rejected. Empty strings remain defined, which preserves trailing empty state
    keys. *)

val expand_exn : t -> (string * string) list -> string
(** Raising form of {!expand}. Raises [Invalid_argument] if expansion fails. *)
