module Stable = struct
  open Core.Core_stable

  module Version = struct
    type t = V2 [@@deriving bin_io, sexp, compare ~localize]

    let%expect_test "bin_digest Message.Version.V2" =
      print_endline [%bin_digest: t];
      [%expect {| 6ae8dff060dc8c96585060b4f76d2974 |}]
    ;;

    let ( <> ) t1 t2 = compare t1 t2 <> 0
    let to_string t = Core.Sexp.to_string (sexp_of_t t)
  end

  module Make (T : Versioned_intf.S with type version := Version.t) = struct
    module Versioned = struct
      type 'a t = Version.t * 'a T.t [@@deriving bin_io, sexp]
    end

    let of_versioned (version, t) =
      if Version.( <> ) version T.version
      then
        Core.failwithf
          !"version mismatch %{Version} <> to expected version %{Version}"
          version
          T.version
          ()
      else t
    ;;

    type 'time t = 'time T.t

    include
      Sexpable.Of_sexpable1.V1
        (Versioned)
        (struct
          type nonrec 'time t = 'time T.t

          let to_sexpable t = T.version, t
          let of_sexpable = of_versioned
        end)

    include
      Binable.Of_binable1.V1 [@alert "-legacy"]
        (Versioned)
        (struct
          type 'time t = 'time T.t

          let to_binable t = T.version, t
          let of_binable = of_versioned
        end)
  end
end
