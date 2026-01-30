open! Core
include File_kind_intf

type t =
  | Block_device
  | Character_device
  | Directory
  | Fifo
  | Regular
  | Socket
  | Symlink
[@@deriving
  compare ~portable ~localize
  , enumerate
  , equal ~portable ~localize
  , hash ~portable
  , quickcheck ~portable
  , sexp_of ~portable]

let to_unix_file_kind : t -> Core_unix.file_kind = function
  | Block_device -> S_BLK
  | Character_device -> S_CHR
  | Directory -> S_DIR
  | Fifo -> S_FIFO
  | Regular -> S_REG
  | Socket -> S_SOCK
  | Symlink -> S_LNK
;;

let of_unix_file_kind : Core_unix.file_kind -> t = function
  | S_BLK -> Block_device
  | S_CHR -> Character_device
  | S_DIR -> Directory
  | S_FIFO -> Fifo
  | S_REG -> Regular
  | S_SOCK -> Socket
  | S_LNK -> Symlink
;;

let to_async_file_kind = function
  | Block_device -> `Block
  | Character_device -> `Char
  | Directory -> `Directory
  | Fifo -> `Fifo
  | Regular -> `File
  | Symlink -> `Link
  | Socket -> `Socket
;;

let of_async_file_kind = function
  | `Block -> Block_device
  | `Char -> Character_device
  | `Directory -> Directory
  | `Fifo -> Fifo
  | `File -> Regular
  | `Link -> Symlink
  | `Socket -> Socket
;;
