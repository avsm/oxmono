open! Base
open! Ppxlib
open! Ast_builder.Default

module type S = sig
  type input
  type t

  (** [payload] defines the form of the input in the expander: [[%anon input]]. *)
  val payload : (payload, input -> t, t) Ast_pattern.t

  (** [expand] converts a [t] of the record variety to a [t] of the polymorphic-variant
      tuple variety. *)
  val expand : input -> loc:location -> t
end

module type Expander = sig
  module type S = S
end
