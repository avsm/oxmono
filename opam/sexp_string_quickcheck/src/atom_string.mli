open! Core
open! Import

type t = string [@@deriving quickcheck, sexp_of]
