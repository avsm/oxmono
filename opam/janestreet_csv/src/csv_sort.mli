open! Core

module Csv_kind : sig
  type t

  val has_header : t
  val no_header : t
end

module Order : sig
  type t =
    | Ascending
    | Descending
  [@@deriving compare, enumerate, sexp_of]
end

module Sort_type : sig
  type t =
    | Bytes
    | Float
    | Infer
    | Int
    | Natsort (** https://en.wikipedia.org/wiki/Natural_sort_order *)
    | Span
    | String
    | Time
  [@@deriving compare, enumerate, sexp_of]
end

module Sort_column : sig
  type t =
    { field : string
    ; order : Order.t
    ; sort_type : Sort_type.t
    }
  [@@deriving sexp_of]
end

module Sort_columns : sig
  type t

  val param : t Command.Param.t
end

val sort_on_fields : Csv_kind.t -> Sort_column.t list -> Csv_common.t -> Csv_common.t

val run
  :  ?separator:char
  -> Sort_columns.t
  -> Csv_common.Or_file.t
  -> no_header:bool
  -> unit
