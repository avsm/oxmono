val documents : (string * string) list
(** Offline discovery and input checks for every vendored Tangled lexicon.
    Checks wire types, references, required/nullable fields, numeric and byte
    length limits, enums, and common AT Protocol identifier formats. Server-side
    validation remains authoritative, including grapheme and blob size limits.
*)

val document : string -> Jsont.json
val main : string -> Jsont.json
val kind : string -> string
val member : string -> Jsont.json -> Jsont.json option
val field : string -> Jsont.json -> Jsont.json
val text : Jsont.json -> string
val strings : Jsont.json -> string list
val validate : ?depth:int -> nsid:string -> Jsont.json -> Jsont.json -> unit
val validate_input : string -> Jsont.json -> unit
val params : string -> (string * string) list -> unit
