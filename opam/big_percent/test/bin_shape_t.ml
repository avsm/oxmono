open! Core

(* [Big_percent.Stable.V3] changes shape with a [Bin_prot.Shape.Uuid.t]. *)

let%expect_test _ =
  print_endline [%bin_digest: Bignum.Stable.V3.t];
  [%expect {| f358e9c3caca8a9589275ba8d8349ae8 |}]
;;

let%expect_test _ =
  print_endline [%bin_digest: Big_percent.Stable.V3.t];
  [%expect {| 560265434b87090b3b43778278fc4f3a |}]
;;
