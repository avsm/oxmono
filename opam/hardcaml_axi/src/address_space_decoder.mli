(** Construction of address space decoders. This supports a slower but more complete full
    address space decoder and the faster but incomplete partial address decoder. *)

open Base
open Hardcaml

(** Specification of an address space. *)
module Address_space : sig
  type t =
    { address : int (** Base address of space *)
    ; size : int (** Size of address space *)
    }
  [@@deriving compare ~localize, hash, sexp_of]

  include Comparable.S with type t := t
end

(** Collection of address spaces held in sorted order and checked to be non-overlapping. *)
module Address_spaces : sig
  type t [@@deriving sexp_of]

  val create : Address_space.t list -> t
end

(** Address space sorted into a binary decoder tree. Only valid for address spaces which
    fulfill the requirements of the [partial_address_decoder] *)
module Address_space_tree : sig
  type t [@@deriving sexp_of]

  val create : address_bits:int -> Address_spaces.t -> t
end

(** Perform checks to ensure the address spaces are suitable for the partial address
    decoder.

    - sizes are powers of 2
    - addresses are some mutliple of the size *)
val valid_for_partial_address_decoder : address_bits:int -> Address_space.t -> unit

(** Instantiates address space decoder functions for a given [Comb] interface. *)
module Make (Comb : Comb.S) : sig
  (** Map of [Address_space] to the computed enable bit for that space. *)
  type t = Comb.t Map.M(Address_space).t

  (** Create a full address decoder. Precisely decodes each address space by comparing the
      upper and lower bounds to the address. Simple but slow and large. *)
  val full_address_decoder : address_spaces:Address_spaces.t -> address:Comb.t -> t

  (** Create a partial address decoder. The input [Address_space]s are validated first
      then an [Address_space_tree] is constructed from which the decoder bits are derived.
      Note that in this scheme every input address is mapped to some decoder output,
      however, within the specified spaces the decoding should match
      [full_address_decoder]. *)
  val partial_address_decoder : address_spaces:Address_spaces.t -> address:Comb.t -> t
end
