open! Core

module Stable = struct
  module V1 = struct
    type t = string [@@deriving compare, sexp_of]

    let fold_with_start_pos t ~init ~f =
      let require_uchar pos = function
        | `Malformed s -> raise_s [%message "Not UTF-8" ~_:(s : string) (pos : int)]
        | `Uchar uchar -> uchar
      in
      Uutf.String.fold_utf_8
        (fun init pos x -> f init pos (require_uchar pos x))
        init
        t [@nontail]
    ;;

    let invariant t =
      Invariant.invariant t [%sexp_of: t] (fun () ->
        fold_with_start_pos t ~init:() ~f:(fun () (_ : int) (_ : Uchar.t) -> ()))
    ;;

    let of_string t =
      invariant t;
      t
    ;;

    let to_string = String.to_string
    let t_of_sexp s = string_of_sexp s |> of_string

    include
      Binable.Of_binable_with_uuid
        (struct
          type t = String.Stable.V1.t [@@deriving bin_io]
        end)
        (struct
          type nonrec t = t

          let of_binable = of_string
          let to_binable = to_string

          let caller_identity =
            Bin_prot.Shape.Uuid.of_string "5bc29e13-1c6f-4b6d-b431-3befb256ebda"
          ;;
        end)

    let%expect_test "" =
      print_endline [%bin_digest: t];
      [%expect {| 0fae4a3589e35c8bf51b9052f31761d0 |}]
    ;;
  end
end

include Stable.V1

include Container.Make0 (struct
    type nonrec t = t

    module Elt = Uchar

    let fold t ~init ~f =
      fold_with_start_pos t ~init ~f:(fun init _pos uchar -> f init uchar) [@nontail]
    ;;

    let fold_until t ~init ~f ~finish = Container.fold_until ~fold t ~init ~f ~finish
    let fold = `Custom fold
    let iter_until = `Define_using_fold_until
    let iter = `Define_using_fold
    let length = `Define_using_fold
  end)

let concat ?sep ts = String.concat ts ?sep
let is_empty = String.is_empty

let split str ~on =
  match on with
  | '\000' .. '\127' -> String.split str ~on
  | '\128' .. '\255' ->
    raise_s [%sexp "Utf8_text.split: can't split on a non-ascii char", (on : char)]
;;

let assumed_width_per_uchar = 1
let width t = sum (module Int) t ~f:(const assumed_width_per_uchar)
let bytes = String.length

let of_uchar_list uchars =
  let buf = Buffer.create 8 (* arbitrary small number *) in
  List.iter uchars ~f:(Uutf.Buffer.add_utf_8 buf);
  of_string (Buffer.contents buf)
;;

include
  Quickcheckable.Of_quickcheckable
    (struct
      module Uchar = struct
        type t = Uchar.t

        include
          Quickcheckable.Of_quickcheckable_filtered
            (Int)
            (struct
              type nonrec t = t

              let of_quickcheckable = Uchar.of_scalar
              let to_quickcheckable = Uchar.to_scalar
            end)
      end

      type t = Uchar.t list [@@deriving quickcheck]
    end)
    (struct
      type nonrec t = t

      let of_quickcheckable = of_uchar_list
      let to_quickcheckable = to_list
    end)

let iteri t ~f =
  ignore
    (fold t ~init:0 ~f:(fun i uchar ->
       f i uchar;
       i + 1)
     : int)
;;
