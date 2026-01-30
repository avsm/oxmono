open! Core

module Make (M : sig
    type t [@@deriving bin_io]

    include Diffable.S_plain with type t := t

    module Diff : sig
      type t [@@deriving bin_io, sexp_of]

      include module type of Diff with type t := t
    end
  end) =
struct
  type t = M.t [@@deriving bin_io]

  module Update = struct
    type t = M.Diff.t option [@@deriving sexp_of, bin_io]
  end

  let diffs ~from ~to_ : Update.t =
    Diffable.Optional_diff.to_option (M.Diff.get ~from ~to_) [@nontail]
  ;;

  let update t = function
    | None -> t
    | Some update -> M.Diff.apply_exn t update
  ;;
end
