open Base
open Hardcaml

module Address_space = struct
  module T = struct
    type t =
      { address : int
      ; size : int
      }
    [@@deriving compare ~localize, hash, sexp_of]
  end

  include T
  include Comparable.Make (T)

  let size_is_pow2 a = Int.is_pow2 a.size

  let address_is_multiple_of_size a =
    Int.equal (Int.round_up ~to_multiple_of:a.size a.address) a.address
  ;;
end

module Address_spaces = struct
  type t = Address_space.t list [@@deriving sexp_of]

  let rec must_not_overlap (t : t) =
    match t with
    | [ _ ] | [] -> ()
    | a :: b :: tl ->
      let a_max = a.address + a.size in
      if b.address >= a_max
      then must_not_overlap (b :: tl)
      else
        raise_s
          [%message
            "Address spaces must not overlap" (a : Address_space.t) (b : Address_space.t)]
  ;;

  let create (t : t) =
    let t = List.sort t ~compare:(fun a b -> Int.compare a.address b.address) in
    must_not_overlap t;
    t
  ;;
end

module Address_space_tree = struct
  type t =
    | Nothing_to_decode
    | Address_space of Address_space.t
    | Branch of
        { bit : int
        ; zero : t
        ; one : t
        }
  [@@deriving sexp_of]

  let create ~address_bits address_spaces =
    let rec create bit (address_spaces : Address_spaces.t) =
      if bit < 0
      then Nothing_to_decode
      else (
        match address_spaces with
        | [] -> Nothing_to_decode
        | [ a ] -> Address_space a
        | address_spaces ->
          (* parition space in half *)
          let zero, one =
            List.partition_tf address_spaces ~f:(fun address_space ->
              address_space.address land (1 lsl bit) = 0)
          in
          (* Is one half empty? If so don't decode this bit *)
          if List.is_empty zero
          then create (bit - 1) one
          else if List.is_empty one
          then create (bit - 1) zero
          else (
            (* recurse into halves *)
            let zero = create (bit - 1) zero in
            let one = create (bit - 1) one in
            Branch { bit; zero; one }))
    in
    (* iterate down from MSB *)
    create (address_bits - 1) address_spaces
  ;;
end

let space_is_addressable ~address_bits (address_space : Address_space.t) =
  let max_address = 1 lsl address_bits in
  if address_space.address + address_space.size > max_address
  then
    raise_s
      [%message
        "Address space is not reachable with given address bits"
          (address_bits : int)
          (address_space : Address_space.t)]
;;

let valid_for_partial_address_decoder ~address_bits (address_space : Address_space.t) =
  if not (Address_space.size_is_pow2 address_space)
  then
    raise_s
      [%message
        "Address space size must be a power of 2" (address_space : Address_space.t)];
  if not (Address_space.address_is_multiple_of_size address_space)
  then
    raise_s
      [%message
        "Address space base address must be a multiple of the size"
          (address_space : Address_space.t)];
  space_is_addressable ~address_bits address_space
;;

module Make (Comb : Comb.S) = struct
  open Comb

  type t = Comb.t Map.M(Address_space).t

  let full_address_decoder ~(address_spaces : Address_spaces.t) ~address =
    let address_bits = width address in
    List.fold
      address_spaces
      ~init:(Map.empty (module Address_space))
      ~f:(fun map address_space ->
        space_is_addressable ~address_bits address_space;
        let decode =
          address
          >=:. address_space.address
          &: (address <=:. address_space.address + address_space.size - 1)
        in
        Map.add_exn map ~key:address_space ~data:decode)
  ;;

  let partial_address_decoder ~(address_spaces : Address_spaces.t) ~address =
    let address_bits = width address in
    (* properties we require in order to build the partial address decoder *)
    List.iter address_spaces ~f:(valid_for_partial_address_decoder ~address_bits);
    (* build the address space tree *)
    let tree = Address_space_tree.create ~address_bits address_spaces in
    let address_n = ~:address in
    let address = to_array address in
    let address_n = to_array address_n in
    (* Build the decoder. *)
    let rec build_decoder (tree : Address_space_tree.t) eqn decoded =
      match tree with
      | Nothing_to_decode -> decoded
      | Address_space address_space -> Map.add_exn decoded ~key:address_space ~data:eqn
      | Branch { bit; zero; one } ->
        let decoded = build_decoder zero (eqn &: address_n.(bit)) decoded in
        build_decoder one (eqn &: address.(bit)) decoded
    in
    build_decoder tree vdd (Map.empty (module Address_space))
  ;;
end
