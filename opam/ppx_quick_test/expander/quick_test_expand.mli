open! Base
open Ppxlib

(** [expand ~loc value_binding maybe_rec rest] expands
    {[
      let%quick_test (* maybe_rec *) value_binding in
      rest
    ]} *)
val expand : loc:location -> value_binding -> rec_flag -> expression -> expression
