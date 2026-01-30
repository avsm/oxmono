open! Core

module Make (M : sig
    type t [@@deriving bin_io]

    include Diffable.S_plain with type t := t

    module Diff : sig
      type t [@@deriving bin_io, sexp_of]

      include module type of Diff with type t := t
    end
  end) : Polling_state_rpc.Response with type t = M.t and type Update.t = M.Diff.t option
