open! Ppxlib

type t

(** [Base.Array.f] *)

val map : t
val mapi : t
val iter : t
val iteri : t
val fold : t
val all : t list

(** [extensions] expands e.g.

    {[
      [%%array.map: t]
    ]}

    into its implementation/interface. *)
val extensions : t -> Context_free.Rule.t list

(** [attributes] expands e.g.

    {[
      [@@@array.map]
    ]}

    into its implementation/interface. *)
val attributes : t -> Context_free.Rule.t list

(** Includes extensions in the deriving ppx. E.g.
    {[
      type foo [@@deriving_inline array ~map]

      [%%array.map: foo]

      [@@@end]
    ]} *)
module For_deriving : sig
  type function_ := t
  type t

  val flag : function_ -> t option Deriving.Args.param
  val structure_extensions : t -> location -> core_type -> structure_item list
  val signature_extensions : t -> location -> core_type -> signature_item list
end
