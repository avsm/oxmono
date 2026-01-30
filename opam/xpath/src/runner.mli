open! Core
module Xml := Simple_xml

module Context : sig
  (** This defines the context and configuration of the query. *)
  type t [@@deriving sexp_of]

  val create_exn
    :  ?prefixes:string String.Map.t
         (** Mapping from namespace prefix in the query to namespace URI. *)
    -> ?variables:Value.t Expanded_name.Map.t
         (** Mapping from variable name in the query to variable value. *)
    -> ?functions:Function.t Expanded_name.Map.t
         (** Mapping from function name in the query to function implementation. Cannot
             conflict with predefined functions, will raise if it does. *)
    -> ?non_standard_qname_name_test_ignores_namespace:bool
         (** If true, the name test will ignore namespaces when comparing qualified names. *)
    -> unit
    -> t
end

val run_exn : Context.t -> Xml.element -> Types.Expression.t -> Value.t

module Private : sig
  val evaluate_axis_in_document_order
    :  Axis.t
    -> Trail.t
    -> Node.t
    -> (Trail.t * Node.t) Sequence.t
end
