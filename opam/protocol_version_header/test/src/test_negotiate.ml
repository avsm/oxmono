open! Core
open Expect_test_helpers_core
open Protocol_version_header

module Legacy = struct
  type t = int list [@@deriving bin_io]
end

let test ~us:(p_us, v_us) ~peer:(p_peer, v_peer) =
  let us = create_exn () ~protocol:p_us ~supported_versions:v_us in
  let peer = create_exn () ~protocol:p_peer ~supported_versions:v_peer in
  let result = negotiate ~allow_legacy_peer:false ~us ~peer in
  print_s [%message "" ~_:(result : int Or_error.t)]
;;

let test_legacy ~allow_legacy_peer ~us:(p_us, v_us) ~peer =
  let us = create_exn () ~protocol:p_us ~supported_versions:v_us in
  let peer =
    let str = Binable.to_string (module Legacy) peer in
    Binable.of_string (module Protocol_version_header) str
  in
  let result = negotiate ~allow_legacy_peer ~us ~peer in
  print_s [%message "" ~_:(result : int Or_error.t)]
;;

let%expect_test "We have a bad header" =
  let peer = create_exn ~protocol:Krb ~supported_versions:[ 1 ] () in
  let us =
    let str = Binable.to_string (module Legacy) [ 1 ] in
    Binable.of_string (module Protocol_version_header) str
  in
  let result = negotiate ~allow_legacy_peer:false ~us ~peer in
  print_s [%message "" ~_:(result : int Or_error.t)];
  [%expect
    {|
    (Error (
      "[Protocol_version_header.negotiate]: Could not determine our own protocol"
      (us_versions (1))))
    |}];
  let result = negotiate ~allow_legacy_peer:true ~us ~peer in
  print_s [%message "" ~_:(result : int Or_error.t)];
  [%expect
    {|
    (Error (
      "[Protocol_version_header.negotiate]: Could not determine our own protocol"
      (us_versions (1))))
    |}]
;;

let%expect_test _ =
  test ~us:(Krb, [ 1 ]) ~peer:(Krb, [ 1 ]);
  let () = [%expect {| (Ok 1) |}] in
  test ~us:(Krb, [ 1 ]) ~peer:(Krb, [ 2 ]);
  [%expect
    {|
    (Error (
      "[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
      (us_versions   (1))
      (peer_versions (2))
      (protocol Krb)))
    |}];
  test ~us:(Krb, [ 1 ]) ~peer:(Rpc, [ 1 ]);
  [%expect
    {|
    (Error (
      "[Protocol_version_header.negotiate]: Peer is using a different protocol from us"
      (us_protocol   Krb)
      (peer_protocol Rpc)))
    |}];
  test_legacy ~allow_legacy_peer:false ~us:(Krb, [ 1; 2 ]) ~peer:[ 1 ];
  [%expect
    {|
    (Error (
      "[Protocol_version_header.negotiate]: Could not determine peer's protocol"
      (us_protocol   Krb)
      (peer_protocol Unknown)))
    |}];
  test_legacy ~allow_legacy_peer:true ~us:(Krb, [ 1; 2 ]) ~peer:[ 1 ];
  [%expect {| (Ok 1) |}];
  test_legacy ~allow_legacy_peer:true ~us:(Krb, [ 2 ]) ~peer:[ 1 ];
  [%expect
    {|
    (Error (
      "[Protocol_version_header.negotiate]: Peer and us share no compatible versions"
      (us_versions   (2))
      (peer_versions (1))
      (protocol Krb)))
    |}]
;;

let%test_unit "negotiate returns correct max common version" =
  let generator =
    let open Quickcheck.Generator.Let_syntax in
    let%bind protocol = Quickcheck.Generator.of_list Known_protocol.all in
    let%bind _us_protocol, us_versions, us_additional =
      Generators.header_generator ~min_versions:1 ~max_versions:30
    in
    let%bind _peer_protocol, peer_versions, peer_additional =
      Generators.header_generator ~min_versions:1 ~max_versions:30
    in
    (* We want to make sure to permute the header lists sometimes so that we're still
       testing the slow path and not only the fast path. *)
    let conditionally_permute header =
      let%bind permute = Quickcheck.Generator.bool in
      if permute
      then (
        let header = Expert.raw_version_list header in
        let%map header = List.gen_permutations header in
        For_test.create_unvalidated header)
      else return header
    in
    let%bind us =
      conditionally_permute
        (create_exn
           ()
           ~protocol
           ~supported_versions:us_versions
           ~additional_magic_numbers:us_additional)
    in
    let%bind peer =
      conditionally_permute
        (create_exn
           ()
           ~protocol
           ~supported_versions:peer_versions
           ~additional_magic_numbers:peer_additional)
    in
    return (us, us_versions, peer, peer_versions)
  in
  Quickcheck.test generator ~f:(fun (us, us_versions, peer, peer_versions) ->
    let negotiated_version = negotiate ~allow_legacy_peer:false ~us ~peer in
    let us_set = Int.Set.of_list us_versions in
    let peer_set = Int.Set.of_list peer_versions in
    let common_versions = Set.inter us_set peer_set |> Set.to_list in
    match common_versions with
    | [] ->
      (match negotiated_version with
       | Error _ -> ()
       | Ok version ->
         raise_s
           [%message
             "negotiate succeeded but no common versions exist"
               (version : int)
               (us_versions : int list)
               (peer_versions : int list)])
    | _ :: _ ->
      let expected_max =
        List.max_elt common_versions ~compare:Int.compare |> Option.value_exn
      in
      (match negotiated_version with
       | Error err ->
         raise_s
           [%message
             "negotiate failed but common versions exist"
               (err : Error.t)
               (expected_max : int)
               (common_versions : int list)
               (us_versions : int list)
               (peer_versions : int list)]
       | Ok version ->
         if version <> expected_max
         then
           raise_s
             [%message
               "negotiate returned wrong version"
                 (version : int)
                 (expected_max : int)
                 (common_versions : int list)
                 (us_versions : int list)
                 (peer_versions : int list)]))
;;
