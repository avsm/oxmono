open! Core

type t =
  | Krb
  | Krb_test_mode
  | Rpc
[@@deriving compare ~localize, enumerate, sexp, globalize]

val magic_number : t -> int
val by_magic_number : t Int.Map.t

module For_test : sig
  val all_magic_numbers_including_retired : int list Lazy.t
end
