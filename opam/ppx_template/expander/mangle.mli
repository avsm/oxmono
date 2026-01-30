open! Stdppx
open! Import
open Language.Typed

module Suffix : sig
  type t

  val create : Value.Basic.packed list Maybe_explicit.t Axis.Map.t -> t
end

module Result : sig
  type t

  val did_mangle : t -> bool
end

(** We piggyback on [Ast_traverse] because it gives us a lot of AST traversal code for
    free. However, only the methods we currently need are implemented - if adding support
    for new kinds of bindings in [Monomorphize], be sure to update [Mangle].

    An alternative implementation for this could be
    [Suffix.t -> location Ast_traverse.map_with_context], with [Suffix.t = string list],
    rather than [string list loc] as it secretly is now. That is, we don't modify the
    [string list] part of the suffix as we traverse. The [location] is updated as we
    traverse to the nearest enclosing location, to be used in error reporting.

    However, passing the [string list] via [map_with_context] allows us to cheaply reuse a
    global singleton [Mangle] object for every identifier we need to mangle, calling pure
    functions, rather than creating a fresh object with many methods and potentially some
    closures each time. *)
val t : (Suffix.t, Result.t) Ast_traverse.lift_map_with_context

(** Apply name mangling to the item, using the given attribute expressions and
    environment. *)
val mangle
  :  'a Attributes.Context.mono
  -> 'a
  -> Expression.Basic.packed Loc.t list Maybe_explicit.t Axis.Map.t
  -> env:Env.t
  -> 'a
