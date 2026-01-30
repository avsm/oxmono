open! Core
module Known_protocol = Known_protocol

let max_supported_version = 1_000_000
let outside_max_supported_version_range num = num > max_supported_version

module Bounded_list_in_case_someone_sends_garbage_on_the_wire =
List_with_max_len.Make (struct
    let max_len = 100
    let context = Info.of_string "Protocol_version_header"
  end)

include struct
  open Stable_witness.Export

  type t = int Bounded_list_in_case_someone_sends_garbage_on_the_wire.t
  [@@deriving bin_io ~localize, globalize, sexp, stable_witness]

  let[@zero_alloc opt] bin_read_t__local buf ~pos_ref = exclave_
    (* We can assume this will not allocate, as [bin_read_list__local] will not allocate
       as long as its argument doesn't, and [bin_read_int] doesn't allocate *)
    (Bounded_list_in_case_someone_sends_garbage_on_the_wire.bin_read_t__local
    [@zero_alloc assume])
      bin_read_int
      buf
      ~pos_ref
  ;;
end

(** Witnesses that the protocol version header starts with some magic numbers outside of
    the valid version range exactly one of which is a known protocol, and then is followed
    by version numbers in monotonic nondecreasing order. *)
module Validated_for_fast_path = struct
  type t =
    { header : int iarray
    ; protocol : Known_protocol.t
    ; versions_start_index : int
    }

  module Protocol_and_versions_start = struct
    type t =
      { protocol : Known_protocol.t
      ; versions_start_index : int
      ; last_seen_version : int
      }
  end

  module Fold_state = struct
    type t =
      | Invalid
      | Start
      | Found_protocol of Known_protocol.t
      | Found_protocol_and_versions_start of Protocol_and_versions_start.t
  end

  let[@zero_alloc opt] validate (wire : int iarray) : t or_null = exclave_
    let fold_result =
      Iarray.Local.foldi wire ~init:Fold_state.Start ~f:(fun index acc item -> exclave_
        match acc with
        | Invalid -> Invalid
        | Start ->
          if outside_max_supported_version_range item
          then (
            match Map.find Known_protocol.by_magic_number item with
            | Some protocol -> Found_protocol protocol
            | None ->
              (* We ignore magic numbers that aren't known protocols so that clients can
                 use this to smuggle along data not used in version negotiation. *)
              Start)
          else Invalid
        | Found_protocol protocol ->
          if outside_max_supported_version_range item
          then (
            match Map.find Known_protocol.by_magic_number item with
            | Some (_ : Known_protocol.t) ->
              (* Now that we've found a known protocol, finding another one means that
                 this protocol version header is invalid. *)
              Invalid
            | None -> Found_protocol protocol)
          else
            (* This must be the first time we've seen a valid version number. *)
            Found_protocol_and_versions_start
              { Protocol_and_versions_start.protocol
              ; versions_start_index = index
              ; last_seen_version = item
              }
        | Found_protocol_and_versions_start
            { protocol; versions_start_index; last_seen_version } ->
          if outside_max_supported_version_range item || item < last_seen_version
             (* Now that we've seen a valid version number, any subsequent invalid version
                number means we can't be in the fast path (though this could still be a
                valid protocol version header as long as it was some other magic number)
                since our fast path assumes that all the version numbers are continguous
                in memory.

                If this is a valid version number, but it's less than the previous version
                number that we saw, then we know that the version numbers are not in
                monotonic nondecreasing order, which also means we can't do the fast path.
             *)
          then Invalid
          else
            Found_protocol_and_versions_start
              { protocol; versions_start_index; last_seen_version = item })
    in
    match fold_result with
    | Found_protocol_and_versions_start
        (* If we got to this point, then we saw exactly one protocol magic number followed
           by monotonically nondecreasing version numbers. *)
        { protocol; versions_start_index; last_seen_version = _ } ->
      This { header = wire; protocol; versions_start_index }
    | Found_protocol (_ : Known_protocol.t) | Start | Invalid -> Null
  ;;

  let[@zero_alloc opt] max_common_version (local_ (us : t)) (local_ (peer : t))
    : int or_null
    =
    let us_len = Iarray.length us.header in
    let peer_len = Iarray.length peer.header in
    let rec loop us_idx peer_idx =
      if us_idx < us.versions_start_index || peer_idx < peer.versions_start_index
      then Null
      else (
        let us_version = us.header.:(us_idx) in
        let peer_version = peer.header.:(peer_idx) in
        match
          Ordering.of_int (([%compare: int] [@mode local]) us_version peer_version)
        with
        | Equal -> This us_version
        | Greater -> loop (us_idx - 1) peer_idx
        | Less -> loop us_idx (peer_idx - 1))
    in
    loop (us_len - 1) (peer_len - 1) [@nontail]
  ;;
end

let known_protocol_magic_numbers = lazy (Map.key_set Known_protocol.by_magic_number)

let create_exn ?(additional_magic_numbers = []) ~protocol ~supported_versions () =
  let protocol_magic_number = Known_protocol.magic_number protocol in
  if List.exists supported_versions ~f:outside_max_supported_version_range
  then
    raise_s
      [%message
        "Unable to advertise versions larger than max supported version"
          (max_supported_version : int)
          (supported_versions : int list)];
  if List.exists additional_magic_numbers ~f:(Fn.non outside_max_supported_version_range)
  then
    raise_s
      [%message
        "[additional_magic_numbers] shouldn't be within [max_supported_version] range"
          (max_supported_version : int)
          (additional_magic_numbers : int list)];
  if List.exists
       additional_magic_numbers
       ~f:(Set.mem (force known_protocol_magic_numbers))
  then
    raise_s
      [%message
        "[additional_magic_numbers] shouldn't be overlapping with potential \
         [protocol_magic_number]s"
          (additional_magic_numbers : int list)
          ~known_protocol_magic_numbers:(force known_protocol_magic_numbers : Int.Set.t)];
  let supported_versions = List.dedup_and_sort supported_versions ~compare:Int.compare in
  protocol_magic_number :: (additional_magic_numbers @ supported_versions)
  |> Bounded_list_in_case_someone_sends_garbage_on_the_wire.of_list_exn
;;

let get_protocol (t : t) = exclave_
  let protocols, versions =
    Iarray.Local.fold
      (Bounded_list_in_case_someone_sends_garbage_on_the_wire.to_iarray t)
      ~init:([], [])
      ~f:(fun (protocols, versions) (v : int) -> exclave_
        match Map.find Known_protocol.by_magic_number v with
        | Some v -> v :: protocols, versions
        | None ->
          if not (outside_max_supported_version_range v)
          then protocols, v :: versions
          else protocols, versions)
  in
  match protocols with
  | [] -> Ok (None, versions)
  | [ protocol ] -> Ok (Some protocol, versions)
  | _ ->
    let protocols = [%globalize: Known_protocol.t list] protocols in
    let versions = [%globalize: int list] versions in
    Or_error.error_s
      [%message
        "[Protocol_version_header.negotiate]: Could not determine protocol: Multiple \
         magic numbers corresponding to known protocols observed in header"
          (protocols : Known_protocol.t list)
          (versions : int list)]
;;

let negotiate ~allow_legacy_peer ~(us : t) ~(peer : t) =
  match
    ( Validated_for_fast_path.validate
        (Bounded_list_in_case_someone_sends_garbage_on_the_wire.to_iarray us)
    , Validated_for_fast_path.validate
        (Bounded_list_in_case_someone_sends_garbage_on_the_wire.to_iarray peer) )
  with
  | This us, This peer ->
    if not (([%compare.equal: Known_protocol.t] [@mode local]) us.protocol peer.protocol)
    then
      Or_error.error_s
        [%message
          "[Protocol_version_header.negotiate]: Peer is using a different protocol from \
           us"
            ~us_protocol:(us.protocol : Known_protocol.t)
            ~peer_protocol:(peer.protocol : Known_protocol.t)]
    else (
      match Validated_for_fast_path.max_common_version us peer with
      | This version -> Ok version
      | Null ->
        let extract_versions (v : Validated_for_fast_path.t) =
          let versions = Iarray.Local.subo ~pos:v.versions_start_index v.header in
          [%globalize: int iarray] versions [@nontail]
        in
        let us_versions = extract_versions us in
        let peer_versions = extract_versions peer in
        let protocol = us.protocol in
        Or_error.error_s
          [%message
            "[Protocol_version_header.negotiate]: Peer and us share no compatible \
             versions"
              (us_versions : int iarray)
              (peer_versions : int iarray)
              (protocol : Known_protocol.t)])
  | _ ->
    let open Or_error.Let_syntax in
    let%bind us_protocol, us_versions =
      get_protocol us |> [%globalize: (Known_protocol.t option * int list) Or_error.t]
    in
    let%bind peer_protocol, peer_versions =
      get_protocol peer |> [%globalize: (Known_protocol.t option * int list) Or_error.t]
    in
    let%bind us_protocol =
      match us_protocol with
      | Some x -> return x
      | None ->
        error_s
          [%message
            "[Protocol_version_header.negotiate]: Could not determine our own protocol"
              (us_versions : int list)]
    in
    let%bind peer_protocol =
      match peer_protocol with
      | Some x -> return x
      | None ->
        (* we assume peer is speaking our protocol if [allow_legacy_peer] *)
        if allow_legacy_peer
        then return us_protocol
        else (
          let peer_protocol = `Unknown in
          Or_error.error_s
            [%message
              "[Protocol_version_header.negotiate]: Could not determine peer's protocol"
                (us_protocol : Known_protocol.t)
                (peer_protocol : [ `Unknown ])])
    in
    if not ([%compare.equal: Known_protocol.t] us_protocol peer_protocol)
    then
      Or_error.error_s
        [%message
          "[Protocol_version_header.negotiate]: Peer is using a different protocol from \
           us"
            (us_protocol : Known_protocol.t)
            (peer_protocol : Known_protocol.t)]
    else (
      let protocol = us_protocol in
      let peer_version_set = Hash_set.of_list (module Int) peer_versions in
      match
        List.filter us_versions ~f:(Hash_set.mem peer_version_set)
        |> List.reduce ~f:Int.max
      with
      | Some version -> Ok version
      | None ->
        Or_error.error_s
          [%message
            "[Protocol_version_header.negotiate]: Peer and us share no compatible \
             versions"
              (us_versions : int list)
              (peer_versions : int list)
              (protocol : Known_protocol.t)])
;;

let matches_magic_prefix (t : t) ~protocol =
  let magic_number = Known_protocol.magic_number protocol in
  Iarray.mem
    ~equal:Int.equal
    (Bounded_list_in_case_someone_sends_garbage_on_the_wire.to_iarray t)
    magic_number
;;

let contains_magic_prefix ~protocol =
  Bin_prot.Type_class.cnv_reader (matches_magic_prefix ~protocol) bin_t.reader
;;

let any_magic_prefix =
  let f t =
    List.find Known_protocol.all ~f:(fun protocol -> matches_magic_prefix ~protocol t)
  in
  Bin_prot.Type_class.cnv_reader f bin_t.reader
;;

let magic_number_bin_size = 5

module Magic_prefix_bin_repr = struct
  type t = int [@@deriving bin_shape, bin_write]

  (*=The bin prot representation of a protocol version header is the standard
     representation for an int list:

     | nat0 indicating size | element 0 | element 1 | ...

     [create_exn] will always put the known protocol magic number as "element 0".

     The bin size of "nat0 indicating size" and "element 0" is fixed (there are expect
     tests to make sure we never change them). *)
  let bin_size =
    bin_size_t Bounded_list_in_case_someone_sends_garbage_on_the_wire.max_len
    + magic_number_bin_size
  ;;

  let bin_read_t buf ~pos_ref =
    let (_list_length : Bin_prot.Nat0.t) = Bin_prot.Read.bin_read_nat0 buf ~pos_ref in
    Bin_prot.Read.bin_read_int buf ~pos_ref
  ;;

  let bin_reader_t =
    { Bin_prot.Type_class.read = bin_read_t; vtag_read = Int.__bin_read_t__ }
  ;;
end

let any_magic_prefix_from_six_bytes =
  Bin_prot.Type_class.cnv_reader
    (fun magic_number -> (Map.find Known_protocol.by_magic_number) magic_number)
    Magic_prefix_bin_repr.bin_reader_t
;;

let any_magic_prefix_from_six_bytes_bin_size = Magic_prefix_bin_repr.bin_size

module Pair = struct
  type nonrec t =
    { us : t
    ; peer : t
    }
end

module Expert = struct
  let raw_version_list = Bounded_list_in_case_someone_sends_garbage_on_the_wire.to_list
  let none = Bounded_list_in_case_someone_sends_garbage_on_the_wire.of_list_exn []

  let is_none (t : t) =
    Iarray.is_empty (Bounded_list_in_case_someone_sends_garbage_on_the_wire.to_iarray t)
  ;;
end

module For_test = struct
  module Make_list_with_max_len = List_with_max_len.Make

  let magic_number_bin_size = magic_number_bin_size
  let max_supported_version = max_supported_version

  module Validated_for_fast_path = Validated_for_fast_path

  let create_unvalidated =
    Bounded_list_in_case_someone_sends_garbage_on_the_wire.of_list_exn
  ;;
end

let%test_unit "bin sizes are not changed by accident" =
  (* Ensure the bin_size of Bounded_list_in_case_someone_sends_garbage_on_the_wire.max_len
     is always 1. This means that regardless of how long the list of versions in the
     protocol header is, the representation will be the same number of bytes. The
     representation of an int jumps to 3 bytes at the value 128. *)
  let bounded_list_bin_size =
    Int.bin_size_t Bounded_list_in_case_someone_sends_garbage_on_the_wire.max_len
  in
  assert (bounded_list_bin_size = 1);
  assert (6 = magic_number_bin_size + bounded_list_bin_size);
  assert (
    any_magic_prefix_from_six_bytes_bin_size
    = magic_number_bin_size + bounded_list_bin_size)
;;
