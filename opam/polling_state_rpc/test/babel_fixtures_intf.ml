open! Core.Core_stable

module Stable = struct
  let name = "my-rpc"

  module V1 = struct
    module Query = struct
      type t = { inc : Bool.V1.t } [@@deriving equal, bin_io, sexp_of]
    end

    module Response = struct
      type t = { c : Int.V1.t } [@@deriving bin_io, sexp_of]

      module Update = struct
        type t = Int.V1.t [@@deriving bin_io, sexp_of]
      end

      let diffs ~from:{ c = from } ~to_:{ c = to_ } = to_ - from
      let update { c } d = { c = c + d }
    end
  end

  module V2 = struct
    module Query = struct
      type t = { add : Int.V1.t } [@@deriving equal, bin_io, sexp_of]

      open! Core

      let of_v1_t { V1.Query.inc } = { add = (if inc then 1 else 0) }
      let to_v1_t { add } = { V1.Query.inc = add > 0 }
    end

    module Response = struct
      type t = { count : Int.V1.t } [@@deriving bin_io, sexp_of]

      let of_v1_t { V1.Response.c } = { count = c }
      let to_v1_t { count } = { V1.Response.c = count }

      module Update = struct
        type t = { count_delta : Int.V1.t } [@@deriving bin_io, sexp_of]

        let of_v1_t count_delta = { count_delta }
        let to_v1_t { count_delta; _ } = count_delta
      end

      let diffs ~from:{ count = from } ~to_:{ count = to_ } =
        Update.{ count_delta = to_ - from }
      ;;

      let update t Update.{ count_delta } =
        if count_delta = 0 then t else { count = t.count + count_delta }
      ;;
    end
  end
end

module type Babel_fixtures_intf = sig
  module Stable : module type of Stable
end
