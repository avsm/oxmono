(** JSON values as the spindle reads and writes them: building, reading with
    errors that answer [InvalidRequest], and bounded decoding. *)

exception Invalid of string
(** [Invalid reason] is raised by every reader below for a value of the wrong
    shape. The spindle answers it as a 400 [InvalidRequest]. *)

val invalid : string -> 'a
(** [invalid reason] raises {!Invalid}. *)

(** {1 Building} *)

val str : string -> Jsont.json
val int : int -> Jsont.json
val bool : bool -> Jsont.json
val arr : Jsont.json list -> Jsont.json
val obj : (string * Jsont.json) list -> Jsont.json

val optional : string -> ('a -> 'b) -> 'a option -> (string * 'b) list
(** [optional k f v] is [[(k, f x)]] for [Some x], and [[]] for [None], to
    append to an object's members. *)

val pairs : (string * string) list -> Jsont.json
(** [pairs l] is [l] as an object of strings. *)

(** {1 Reading} *)

val members : Jsont.json -> (string * Jsont.json) list
val field : string -> Jsont.json -> Jsont.json option

val required : string -> Jsont.json -> Jsont.json
(** [required k j] is member [k] of [j], which must be present. *)

val string : Jsont.json -> string

val number : Jsont.json -> float
(** [number j] is [j], which must be a finite number. *)

val integer : Jsont.json -> int
(** [integer j] is [j], which must be a whole number of at most 2{^ 53} in
    magnitude. *)

val list : Jsont.json -> Jsont.json list

val get : string -> Jsont.json -> string
(** [get k j] is [string (required k j)]. *)

val strings : string -> Jsont.json -> string list
(** [strings k j] is member [k] of [j] as strings, [[]] when absent. *)

val unpairs : Jsont.json -> (string * string) list
(** [unpairs j] is the object of strings [j] as pairs. *)

(** {1 Text} *)

val decode : string -> Jsont.json
(** [decode s] is the JSON text [s], nested at most 32 deep and without
    duplicate members. It raises {!Invalid} otherwise. *)

val encode : Jsont.json -> string

(** {1 Identifiers} *)

val sha : string -> string
(** [sha s] is [s], which must be a lowercase 40- or 64-digit Git object ID, as
    knots use SHA-1 and SHA-256. *)

val zero_sha : string -> bool
(** [zero_sha s] holds when [s] is 40 or 64 zeros, the ID of a missing ref. *)

val did : string -> string
(** [did s] is [s], which must be a valid DID. *)
