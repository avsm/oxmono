open! Core
open Filesystem_types
open Expect_test_helpers_core

include (
  File_kind :
  sig
    type t = File_kind.t =
      | Block_device
      | Character_device
      | Directory
      | Fifo
      | Regular
      | Socket
      | Symlink
    [@@deriving compare ~localize, enumerate, equal ~localize, hash, quickcheck, sexp_of]
  end)

let%expect_test "[sexp_of_t]" =
  List.iter all ~f:(fun t -> print_s (sexp_of_t t));
  [%expect
    {|
    Block_device
    Character_device
    Directory
    Fifo
    Regular
    Socket
    Symlink
    |}]
;;

let to_unix_file_kind = File_kind.to_unix_file_kind
let of_unix_file_kind = File_kind.of_unix_file_kind

let%expect_test "[to_unix_file_kind] / [of_unix_file_kind]" =
  List.iter all ~f:(fun t ->
    let file_kind = to_unix_file_kind t in
    print_s [%sexp (file_kind : Core_unix.file_kind)];
    let round_trip = of_unix_file_kind file_kind in
    require_equal (module File_kind) t round_trip);
  [%expect
    {|
    S_BLK
    S_CHR
    S_DIR
    S_FIFO
    S_REG
    S_SOCK
    S_LNK
    |}]
;;

let to_async_file_kind = File_kind.to_async_file_kind
let of_async_file_kind = File_kind.of_async_file_kind

let%expect_test "[to_async_file_kind] / [of_async_file_kind]" =
  List.iter all ~f:(fun t ->
    let file_kind = to_async_file_kind t in
    print_s
      [%sexp
        (file_kind : [ `Block | `Char | `Directory | `Fifo | `File | `Link | `Socket ])];
    let round_trip = of_async_file_kind file_kind in
    require_equal (module File_kind) t round_trip);
  [%expect
    {|
    Block
    Char
    Directory
    Fifo
    File
    Socket
    Link
    |}]
;;
