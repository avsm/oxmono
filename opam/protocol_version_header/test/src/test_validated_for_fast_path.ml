open! Core
open Expect_test_helpers_core
open Protocol_version_header

let krb_magic = Known_protocol.magic_number Krb

module%test [@name "test validate"] _ = struct
  let validate_and_print versions =
    let iarray = Iarray.of_list versions in
    match For_test.Validated_for_fast_path.validate iarray with
    | This validated ->
      print_s
        [%message
          "Valid"
            (validated.protocol : Known_protocol.t)
            (validated.versions_start_index : int)]
    | Null -> print_s [%message "Invalid"]
  ;;

  module%test [@name "validation succeeds"] _ = struct
    let%expect_test "valid header with single version" =
      validate_and_print [ krb_magic; 1 ];
      [%expect
        {|
        (Valid
          (validated.protocol             Krb)
          (validated.versions_start_index 1))
        |}]
    ;;

    let%expect_test "valid header with multiple versions" =
      validate_and_print [ krb_magic; 1; 2; 3 ];
      [%expect
        {|
        (Valid
          (validated.protocol             Krb)
          (validated.versions_start_index 1))
        |}]
    ;;

    let%expect_test "versions with duplicate values" =
      validate_and_print [ krb_magic; 1; 1; 2; 2; 3 ];
      [%expect
        {|
        (Valid
          (validated.protocol             Krb)
          (validated.versions_start_index 1))
        |}]
    ;;
  end

  module%test [@name "validation fails"] _ = struct
    let%expect_test "empty header" =
      validate_and_print [];
      [%expect {| Invalid |}]
    ;;

    let%expect_test "no magic number" =
      validate_and_print [ 1; 2; 3 ];
      [%expect {| Invalid |}]
    ;;

    let%expect_test "no versions after magic" =
      validate_and_print [ krb_magic ];
      [%expect {| Invalid |}]
    ;;

    let%expect_test "versions not in nondecreasing order" =
      validate_and_print [ krb_magic; 3; 2; 1 ];
      [%expect {| Invalid |}]
    ;;

    let%expect_test "multiple magic numbers" =
      let rpc_magic = Known_protocol.magic_number Rpc in
      validate_and_print [ krb_magic; rpc_magic; 1 ];
      [%expect {| Invalid |}]
    ;;

    let%expect_test "version too large" =
      validate_and_print
        [ krb_magic; 1; Protocol_version_header.For_test.max_supported_version + 1 ];
      [%expect {| Invalid |}]
    ;;
  end
end

module%test [@name "test max_common_version"] _ = struct
  let test_max_common ~us_versions ~peer_versions =
    let us_header = Iarray.of_list (krb_magic :: us_versions) in
    let peer_header = Iarray.of_list (krb_magic :: peer_versions) in
    match
      ( For_test.Validated_for_fast_path.validate us_header
      , For_test.Validated_for_fast_path.validate peer_header )
    with
    | This us, This peer ->
      let result = For_test.Validated_for_fast_path.max_common_version us peer in
      print_s [%message "" ~_:(Or_null.to_option result : int option)]
    | _ -> print_s [%message "Validation failed"]
  ;;

  let%expect_test "single common version" =
    test_max_common ~us_versions:[ 1 ] ~peer_versions:[ 1 ];
    [%expect {| (1) |}]
  ;;

  let%expect_test "multiple common versions" =
    test_max_common ~us_versions:[ 1; 2; 3 ] ~peer_versions:[ 1; 2; 3 ];
    [%expect {| (3) |}]
  ;;

  let%expect_test "no common versions" =
    test_max_common ~us_versions:[ 1; 2 ] ~peer_versions:[ 3; 4 ];
    [%expect {| () |}]
  ;;

  let%expect_test "partial overlap" =
    test_max_common ~us_versions:[ 1; 2; 3; 4 ] ~peer_versions:[ 3; 4; 5; 6 ];
    [%expect {| (4) |}]
  ;;

  let%expect_test "subset" =
    test_max_common ~us_versions:[ 1; 2; 3; 4; 5 ] ~peer_versions:[ 2; 4 ];
    [%expect {| (4) |}]
  ;;

  let%expect_test "superset" =
    test_max_common ~us_versions:[ 2; 4 ] ~peer_versions:[ 1; 2; 3; 4; 5 ];
    [%expect {| (4) |}]
  ;;

  let%expect_test "interleaved versions" =
    test_max_common ~us_versions:[ 1; 3; 5; 7 ] ~peer_versions:[ 2; 3; 6; 7 ];
    [%expect {| (7) |}]
  ;;
end
