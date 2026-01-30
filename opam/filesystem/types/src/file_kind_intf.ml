open! Core

module type S = sig @@ portable
  (** Represents file kinds returned by [stat]. *)
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

  (** Conversions *)

  val to_unix_file_kind : t -> Core_unix.file_kind
  val of_unix_file_kind : Core_unix.file_kind -> t

  val to_async_file_kind
    :  t
    -> [ `Block | `Char | `Directory | `Fifo | `File | `Link | `Socket ]

  val of_async_file_kind
    :  [ `Block | `Char | `Directory | `Fifo | `File | `Link | `Socket ]
    -> t
end

module type File_kind = sig
  module type S = S

  include S
end
