open! Core
open Protocol_version_header

let header_generator ~min_versions ~max_versions =
  let open Quickcheck.Generator.Let_syntax in
  let%bind protocol = Quickcheck.Generator.of_list Known_protocol.all in
  let%bind num_supported_versions = Int.gen_incl min_versions max_versions in
  let%bind supported_versions =
    List.gen_with_length
      num_supported_versions
      (Int.gen_incl 0 For_test.max_supported_version)
  in
  let%bind num_additional_magic_numbers =
    Int.gen_incl 0 (99 - num_supported_versions - 1)
  in
  let known_magic_numbers =
    List.map Known_protocol.all ~f:Known_protocol.magic_number |> Int.Set.of_list
  in
  let%bind additional_magic_numbers =
    List.gen_with_length
      num_additional_magic_numbers
      (Quickcheck.Generator.filter
         (Int.gen_uniform_incl (For_test.max_supported_version + 1) Int.max_value)
         ~f:(fun candidate -> not (Set.mem known_magic_numbers candidate)))
  in
  return (protocol, supported_versions, additional_magic_numbers)
;;
