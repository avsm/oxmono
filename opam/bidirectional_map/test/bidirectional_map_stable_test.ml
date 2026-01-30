open! Core
open Expect_test_helpers_core

let examples =
  List.map [ 0; 1; 2; 4; 8 ] ~f:(fun len ->
    Bidirectional_map.of_alist_or_error
      (module Int)
      (module Int)
      (List.init len ~f:(fun i -> i, Int.pow 2 i))
    |> ok_exn)
;;

module _ : module type of struct
  include Bidirectional_map_stable
end = struct
  module V1 = Bidirectional_map_stable.V1

  let%expect_test _ =
    let check () =
      [%expect
        {|
        (bin_shape_digest f1e42e49cdc1a3490dc72da8962d8670)
        ((sexp ()) (bin_io "\000"))
        ((sexp ((0 1))) (bin_io "\001\000\001"))
        ((sexp (
           (0 1)
           (1 2)))
         (bin_io "\002\000\001\001\002"))
        ((sexp (
           (0 1)
           (1 2)
           (2 4)
           (3 8)))
         (bin_io "\004\000\001\001\002\002\004\003\b"))
        ((sexp (
           (0 1)
           (1 2)
           (2 4)
           (3 8)
           (4 16)
           (5 32)
           (6 64)
           (7 128)))
         (bin_io "\b\000\001\001\002\002\004\003\b\004\016\005 \006@\007\254\128\000"))
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
