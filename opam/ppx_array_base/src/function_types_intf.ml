open! Ppxlib

(** [overwrite_output_kinds] will default to [How_to_vary_kinds.base_layouts]. *)
type 'a create_binding =
  location -> Context.t -> overwrite_output_kinds:expression option -> 'a

module type S = sig
  val name : string
  val implementation : structure_item list create_binding
  val interface : signature_item create_binding
end

module type Function_types = sig
  type nonrec 'a create_binding = 'a create_binding

  module type S = S
end
