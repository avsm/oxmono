open! Core
open! Import

module type S = sig
  type version
  type 'a t [@@deriving bin_io, sexp]

  val version : version
end

module type Versioned = sig
  module Stable : sig
    module Version : sig
      type t = V2 [@@deriving bin_io, sexp, compare ~localize]

      val ( <> ) : t -> t -> bool
      val to_string : t -> string
    end

    module Make (T : S with type version := Version.t) : sig
      type 'a t = 'a T.t

      include Binable.S1 with type 'a t := 'a T.t
      include Sexpable.S1 with type 'a t := 'a T.t
    end
  end
end
