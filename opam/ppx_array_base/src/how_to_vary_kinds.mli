open! Ppxlib

module Whether_to_vary : sig
  type 'a t =
    | Vary of { kinds : expression }
    | Do_not_vary of 'a
end

type t =
  { input : core_type Whether_to_vary.t
      (* if you do not vary input kinds, an input type needs to be specified *)
  ; output : unit Whether_to_vary.t
  }

(** [base_layouts] are the common, non-product layouts. Not to be confused with the [Base]
    library. *)
val base_layouts : location -> expression

(** [base_or_null_layouts] are the common, non-product layouts, using [value_or_null]
    instead of [value]. *)
val base_or_null_layouts : location -> expression

(** {[
       let%template function_name : input_type ... -> = f
       [@@kinds __]
      ;;
    ]} *)
val structure_item
  :  t
  -> location
  -> function_name:string
  -> function_implementation:(input_type:core_type -> output_type:core_type -> expression)
  -> structure_item

(** {[
      val%template function_name : input_type ... -> f
      [@@kinds __]
    ]} *)
val signature_item
  :  t
  -> location
  -> function_name:string
  -> function_type:(input_type:core_type -> output_type:core_type -> core_type)
  -> signature_item
