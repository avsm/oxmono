open! Core.Core_stable
open Bidirectional_map_interfaces
open Bidirectional_map_interfaces_stable

module V1 = struct
  type ('l, 'lc, 'r, 'rc) t = ('l, 'lc, 'r, 'rc) Bidirectional_multimap.t

  module M = Bidirectional_multimap.M

  (* Comparisons, etc.: provided for deriving, do not actually have to be stable *)

  let compare_m__t = Bidirectional_multimap.compare_m__t
  let equal_m__t = Bidirectional_multimap.equal_m__t
  let hash_fold_m__t = Bidirectional_multimap.hash_fold_m__t
  let hash_m__t = Bidirectional_multimap.hash_m__t

  (* Serializations: implement via explicit round-trip to/from association list *)

  let sexp_of_m__t
    (type l r)
    (module Left : With_sexp_of with type t = l)
    (module Right : With_sexp_of with type t = r)
    t
    =
    [%sexp_of: (Left.t * Right.t) list] (Bidirectional_multimap.to_alist t)
  ;;

  let m__t_of_sexp
    (type l lc r rc)
    (module Left : With_of_sexp with type t = l and type comparator_witness = lc)
    (module Right : With_of_sexp with type t = r and type comparator_witness = rc)
    sexp
    =
    Bidirectional_multimap.of_alist
      (module Left)
      (module Right)
      ([%of_sexp: (Left.t * Right.t) list] sexp)
  ;;

  let bin_shape_caller_identity =
    Bin_prot.Shape.Uuid.of_string "75a4dad2-5fee-42ee-bb74-0b30343e7766"
  ;;

  let bin_shape_m__t
    (type l r)
    (module Left : With_bin_io with type t = l)
    (module Right : With_bin_io with type t = r)
    =
    Bin_shape.annotate bin_shape_caller_identity [%bin_shape: (Left.t * Right.t) list]
  ;;

  let bin_size_m__t
    (type l r)
    (module Left : With_bin_io with type t = l)
    (module Right : With_bin_io with type t = r)
    t
    =
    [%bin_size: (Left.t * Right.t) list] (Bidirectional_multimap.to_alist t)
  ;;

  let bin_write_m__t
    (type l r)
    (module Left : With_bin_io with type t = l)
    (module Right : With_bin_io with type t = r)
    ~pos
    buf
    t
    =
    [%bin_write: (Left.t * Right.t) list] ~pos buf (Bidirectional_multimap.to_alist t)
  ;;

  let bin_read_m__t
    (type l lc r rc)
    (module Left : With_bin_io with type t = l and type comparator_witness = lc)
    (module Right : With_bin_io with type t = r and type comparator_witness = rc)
    ~pos_ref
    buf
    =
    [%bin_read: (Left.t * Right.t) list] buf ~pos_ref
    |> Bidirectional_multimap.of_alist (module Left) (module Right)
  ;;

  let __bin_read_m__t__
    (type l lc r rc)
    (module _ : With_bin_io with type t = l and type comparator_witness = lc)
    (module _ : With_bin_io with type t = r and type comparator_witness = rc)
    ~pos_ref
    _
    _
    =
    Bin_prot.Common.raise_variant_wrong_type "bidirectional_multimap_stable" !pos_ref
  ;;

  let stable_witness_m__t
    (type l lc r rc)
    (module Left : With_stable_witness with type t = l and type comparator_witness = lc)
    (module Right : With_stable_witness with type t = r and type comparator_witness = rc)
    =
    let open Stable_witness.Export in
    (* There is no current function which combines two stable witnesses into a tuple
       stable witness, so we use [Stable_witness.assert_stable]. *)
    let (tuple_stable : (Left.t * Right.t) Stable_witness.t) =
      let (_ : Left.t Stable_witness.t) = Left.stable_witness in
      let (_ : Right.t Stable_witness.t) = Right.stable_witness in
      Stable_witness.assert_stable
    in
    let list_stable = stable_witness_list tuple_stable in
    Stable_witness.of_serializable
      list_stable
      (Bidirectional_multimap.of_alist (module Left) (module Right))
      Bidirectional_multimap.to_alist
  ;;

  module Provide_bin_io (Left : With_bin_io) (Right : With_bin_io) :
    Core.Binable.S with type t := M(Left)(Right).t =
    Binable.Of_binable.V2
      (struct
        type t = (Left.t * Right.t) list [@@deriving bin_io]
      end)
      (struct
        type t = M(Left)(Right).t

        let to_binable = Bidirectional_multimap.to_alist

        let of_binable alist =
          Bidirectional_multimap.of_alist (module Left) (module Right) alist
        ;;

        let caller_identity = bin_shape_caller_identity
      end)
end
