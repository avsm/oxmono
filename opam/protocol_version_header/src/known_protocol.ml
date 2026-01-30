open! Core

type t =
  | Krb
  | Krb_test_mode
  | Rpc
[@@deriving compare ~localize, enumerate, sexp, bin_io, globalize]

let magic_word = function
  | Krb -> "KRB2"
  | Krb_test_mode -> "KBT"
  | Rpc -> "RPC"
;;

let gen_magic_number word =
  String.to_list_rev word
  |> List.fold ~init:0 ~f:(fun acc c -> (acc * 256) + Char.to_int c)
;;

let magic_number t = gen_magic_number (magic_word t)
let by_magic_number = Int.Map.of_alist_exn (List.map all ~f:(fun p -> magic_number p, p))

(* We once minted a new magic number for Krb in order to change the protocol negotiation.
   Let's be careful that we don't reuse the old magic number *)
let retired_krb_word = "KRB"

(* Ensure tests break if the magic numbers are changed *)
let%test_unit "magic numbers" =
  assert (gen_magic_number retired_krb_word = 4_346_443);
  assert (magic_number Krb = 843_207_243);
  assert (magic_number Krb_test_mode = 5_521_995);
  assert (magic_number Rpc = 4_411_474)
;;

module For_test = struct
  let all_magic_numbers_including_retired =
    lazy (retired_krb_word :: List.map all ~f:magic_word |> List.map ~f:gen_magic_number)
  ;;
end
