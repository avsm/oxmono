open! Core
open Ppxlib

(** These functions handle the geration of the functions that ppx_html calls. *)

(*=[modul] determines whether an html_syntax was passed. E.g.:

   [%html {||}]     -> modul is [None]
   [%html.Foo {||}] -> modul is [Some Foo] *)
type modul := longident loc option

(* Primitives are functions that should be provided by all Html_syntax implementations,
   they're used to build the tree.

   [primitive] determines whether [Html_syntax.Node.Primitive.f] or [Html_syntax.Node.f]
   is used. *)

val node_fn
  :  loc:location
  -> html_syntax_module:modul
  -> primitive:bool
  -> label
  -> expression

val attr_fn
  :  loc:location
  -> html_syntax_module:modul
  -> primitive:bool
  -> label
  -> expression

val attr_fn_with_create : loc:location -> html_syntax_module:modul -> label -> expression
val attr_t_type : loc:location -> core_type
val node_t_type : loc:location -> core_type
