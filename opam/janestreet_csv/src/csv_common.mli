open! Core

type t =
  { header : string list
  ; lines : string list list
  }
[@@deriving compare, sexp]

type csv := t

val empty : t
val print_csv : ?separator:char -> t -> unit
val load : ?separator:char -> string -> t
val load_all : ?separator:char -> string list -> t list

module Or_file : sig
  type t =
    | Csv of csv
    | File of Filename.t
    | Stdin
  [@@deriving compare, sexp_of]

  (** Anonymous arg *)
  val anon : t Command.Param.t

  (** [-file] flag *)
  val flag : t Command.Param.t

  (** [with_lines] calls header_f on the first line, and passes the return value as the
      ~header and ~state args to f, which is called on every following line *)
  val with_lines
    :  ?separator:char
    -> t
    -> header_f:(string list -> string list * 'a)
    -> f:(header:string list -> state:'a -> string list -> string list)
    -> unit

  val with_all : ?separator:char -> t -> f:(csv -> unit) -> no_header:bool -> unit
end

(** [of_csvlib_csv rows] assumes the first row is a header if [no_header = false].
    Regardless, it raises if [rows = []]. *)
val of_csvlib_csv : string list list -> no_header:bool -> t
