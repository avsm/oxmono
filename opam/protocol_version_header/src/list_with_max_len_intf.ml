open! Core

module type Config = sig
  val max_len : int
  val context : Info.t
end

module type S = sig
  type 'a t [@@deriving bin_io ~localize, globalize, sexp, stable_witness]

  val bin_read_t__local : ('a, 'a t) Bin_prot.Read.reader1__local

  (** [of_list_exn l] raises if [List.length l] is larger than the supplied [max_len]. *)
  val of_list_exn : 'a list -> 'a t

  val max_len : int
  val to_list : 'a t -> 'a list
  val fold : 'a t -> init:'b -> f:local_ ('b -> 'a -> 'b) -> 'b
  val to_iarray : 'a t -> 'a iarray
end

module type List_with_max_len = sig
  module type Config = Config
  module type S = S

  module Make (Config : Config) : S
end
