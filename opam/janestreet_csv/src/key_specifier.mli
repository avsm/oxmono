open! Core

type t

val to_string_set : t -> String.Set.t
val int_specifier_of_string : string -> t
val specifier_of_string : string -> t
