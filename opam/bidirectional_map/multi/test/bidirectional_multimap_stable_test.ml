open! Core
open Expect_test_helpers_core

let examples =
  List.map [ 0; 1; 2; 4; 8 ] ~f:(fun len ->
    List.init len ~f:(fun i ->
      let i = i + 1 in
      let floor = Int.floor_log2 i in
      let ceil = Int.ceil_log2 i in
      [ i, floor; i, ceil ])
    |> List.concat
    |> Bidirectional_multimap.of_alist (module Int) (module Int))
;;

module _ : module type of struct
  include Bidirectional_multimap_stable
end = struct
  module V1 = Bidirectional_multimap_stable.V1

  let%expect_test _ =
    let check () =
      [%expect
        {|
        (bin_shape_digest 0c7978980c84e21bbfb78562bf60eed9)
        ((sexp ()) (bin_io "\000"))
        ((sexp ((1 0))) (bin_io "\001\001\000"))
        ((sexp (
           (1 0)
           (2 1)))
         (bin_io "\002\001\000\002\001"))
        ((sexp (
           (1 0)
           (2 1)
           (3 1)
           (3 2)
           (4 2)))
         (bin_io "\005\001\000\002\001\003\001\003\002\004\002"))
        ((sexp (
           (1 0)
           (2 1)
           (3 1)
           (3 2)
           (4 2)
           (5 2)
           (5 3)
           (6 2)
           (6 3)
           (7 2)
           (7 3)
           (8 3)))
         (bin_io
          "\012\001\000\002\001\003\001\003\002\004\002\005\002\005\003\006\002\006\003\007\002\007\003\b\003"))
        |}]
    in
    print_and_check_stable_type
      (module struct
        type t = V1.M(Int)(Int).t [@@deriving compare, sexp]

        include V1.Provide_bin_io (Int) (Int)
      end)
      examples;
    check ();
    let module Stable_int = struct
      include Int

      let stable_witness = Stable_witness.Export.stable_witness_int
    end
    in
    print_and_check_stable_type
      (module struct
        type t = V1.M(Stable_int)(Stable_int).t
        [@@deriving bin_io, compare, sexp, stable_witness]
      end)
      examples;
    check ()
  ;;
end
