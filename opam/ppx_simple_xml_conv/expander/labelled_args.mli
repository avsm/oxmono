open! Base
open! Ppxlib

include module type of struct
  include Ast_pattern
end

(** This module allows you to build an attribute which takes an apply expression of the
    form [expression] [a bunch of labelled arguments], where the arguments can be
    specified in any order. It includes Ast_pattern for ease of use.

    Example usage: To parse a field attribute where the function of the form:
    [@foobar 1 ~foo:"foo" ~bar] [@foobar 2 ~bar ~foo:"bar"] [@foobar 3 ~foo:"foobar"]

    Notice how foo and bar can be reordered, the function called is actually an integer,
    and bar can be dropped. You would write the following code:
    {v
    let attribute =
      let open Labelled_args in
      (* Parse the arguments [~foo:string -> ?bar -> _] *)
      let args =
        ("foo", (estring __), (fun foo -> `Foo foo))
        ^-> just_label "bar"
        ^?-> ret
      in
      (* Parses integers *)
      let func = eint __ in
      declare
        "foobar"
        Label_declaration
        func
        args
        (fun location_of_attribute (func : int) (foo : [ `Foo of string]) (bar : unit option) ->
          <something>)
    ;;
    v} *)

type ('func, 'result) t

type ('build_argument, 'argument) argument :=
  label * (expression, 'build_argument, 'argument) Ast_pattern.t * 'build_argument

val declare
  :  ?allow_no_args:
       bool
       (* [allow_no_args] also parses the function as is without any arguments. There is a
          potential conflict here, if the applied function is also an apply, it will catch
          everything. The target needs to not catch an apply. *)
  -> label
  -> 'context Attribute.Context.t
  -> (expression, 'apply -> 'apply, 'apply) Ast_pattern.t
     (** This is the pattern that matches the function to apply. *)
  -> ('func, 'result) t
  -> (Location.t -> 'apply -> 'func)
  -> ('context, 'result) Attribute.t

(** In [arguments = foo ^-> rest_of_arguments] creates a new argument list [arguments]
    which matches all the arguments of [rest_of_arguments] and also requires the argument
    [foo]. Order does not matter.

    See the example above for more details. *)
val ( ^-> )
  :  (_, 'argument) argument
  -> ('func, 'result) t
  -> ('argument -> 'func, 'result) t

(** In [arguments = foo ^?-> rest_of_arguments] creates a new argument list [arguments]
    which matches all the arguments of [rest_of_arguments] and also optionally matches the
    argument [foo] if it exists. Order does not matter.

    See the example above for more details. *)
val ( ^?-> )
  :  (_, 'argument) argument
  -> ('func, 'result) t
  -> ('argument option -> 'func, 'result) t

(** An empty list of arguments. To be used at the end of the argument chain. *)
val ret : ('result, 'result) t

(* If you want an optional argument that is just the label, (e.g. [allow_extra_blah]), you
   can use this helper. *)
val just_label : string -> (unit -> unit, unit) argument
