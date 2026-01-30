module Stable = struct
  open Core.Core_stable

  module V1 = struct
    type t =
      [ `Sexp of Sexp.V1.t
      | `String of string
      ]
    [@@deriving bin_io, sexp]

    let%expect_test "bin_digest Sexp_or_string.V1" =
      print_endline [%bin_digest: t];
      [%expect {| 7604679c48980b04476c108e66cf67c8 |}]
    ;;

    let to_string = function
      | `Sexp sexp -> Core.Sexp.to_string sexp
      | `String str -> str
    ;;
  end
end

type t = Stable.V1.t [@@deriving sexp_of]
