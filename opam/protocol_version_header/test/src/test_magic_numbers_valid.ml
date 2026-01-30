open! Core
open Protocol_version_header

let magic_numbers =
  force Known_protocol.For_test.all_magic_numbers_including_retired
  @ Map.keys (force Krb_authorization_method.by_magic_number)
;;

let%test_unit "validate magic numbers" =
  (* Magic numbers must fit into OCaml integers (31 bits on 32 bit builds). *)
  assert (List.for_all magic_numbers ~f:(fun n -> n <= Int.of_float ((2. ** 30.) -. 1.)));
  (* Magic numbers are not too small, so it doesn't clash with potential protocol version
     numbers. *)
  assert (List.for_all magic_numbers ~f:(fun n -> n > For_test.max_supported_version));
  (* No duplicate magic numbers *)
  assert (not (List.contains_dup magic_numbers ~compare:Int.compare))
;;

let%test_unit "magic_number_bin_size is correct" =
  List.iter magic_numbers ~f:(fun magic_number ->
    let size = Int.bin_size_t magic_number in
    [%test_eq: int] size For_test.magic_number_bin_size)
;;

let%expect_test "show the value of our magic numbers" =
  List.iter magic_numbers ~f:(fun i -> print_s [%sexp (i : int)]);
  [%expect
    {|
    4346443
    843207243
    5521995
    4411474
    43244093
    43437982
    |}]
;;
