open! Core
include Bignum

(* The [of_*] functions are needed for [Stable.V4] sexp serialization. The [to_*]
   functions need friends. *)
let of_mult b = b
let to_mult t = t
let of_percentage b = b / of_int 100
let to_percentage t = t * of_int 100
let of_bp b = b / of_int 10_000
let to_bp t = t * of_int 10_000

exception Nan_or_inf [@@deriving sexp]

let really_of_string
  str
  ~allow_nan_or_inf
  ~bignum_of_string
  ~function_to_refer_to_in_exception
  =
  let verify_numeric_exn b =
    if allow_nan_or_inf || is_real b then b else raise Nan_or_inf
  in
  match String.chop_suffix str ~suffix:"x" with
  | Some str -> str |> bignum_of_string |> of_mult |> verify_numeric_exn
  | None ->
    (match String.chop_suffix str ~suffix:"%" with
     | Some str -> str |> bignum_of_string |> of_percentage |> verify_numeric_exn
     | None ->
       (match String.chop_suffix str ~suffix:"bp" with
        | Some str -> str |> bignum_of_string |> of_bp |> verify_numeric_exn
        | None ->
          failwithf
            "Big_percent.%s: must end in x, %%, or bp: %s"
            function_to_refer_to_in_exception
            str
            ()))
;;

module type Formatter = sig @@ portable
  type nonrec t = t

  val which_suffix : (t -> string) -> t -> string
  val nan_suffix : string
  val inf_suffix : string
end

module Make_stringable (Formatter : Formatter) = struct
  type t = Formatter.t

  (* "NAN" and "INF" follow the same convention as [Percent]. *)
  let really_to_string bignum_to_string t =
    if is_nan t
    then "NAN" ^ Formatter.nan_suffix
    else if is_positive_infinity t
    then "INF" ^ Formatter.inf_suffix
    else if is_negative_infinity t
    then "-INF" ^ Formatter.inf_suffix
    else Formatter.which_suffix bignum_to_string t
  ;;

  let to_string_accurate t = really_to_string to_string_accurate t

  let to_string_hum ?delimiter ?decimals ?strip_zero =
    really_to_string (to_string_hum ?delimiter ?decimals ?strip_zero)
  ;;
end

(* Used for both stable and unstable string representations. *)
let which_suffix bignum_to_string t =
  let t_abs = abs t in
  if t_abs = zero
  then "0x"
  else if t_abs >= of_int 1
  then bignum_to_string (t * of_int 1) ^ "x"
  else if t_abs >= of_float_decimal 0.01
  then bignum_to_string (t * of_int 100) ^ "%"
  else bignum_to_string (t * of_int 10_000) ^ "bp"
;;

module Stable = struct
  module V3 = struct
    include Bignum.Stable.V3

    let bin_shape_t =
      Bin_prot.Shape.basetype
        (Bin_prot.Shape.Uuid.of_string "ffa03fdf-f636-45fe-f8e1-d3635e0d9e12")
        [ bin_shape_t ]
    ;;

    let t_of_sexp_with_version_parameter_for_failure_case sexp ~which_version_failed =
      let fail s =
        failwithf
          "unable to parse as Big_percent.Stable.V%i.t. Expected a plain Bignum without \
           suffixes, but received %S"
          which_version_failed
          s
          ()
      in
      try t_of_sexp sexp with
      | _ -> sexp |> Sexp.to_string |> fail
    ;;

    (* Rewrite the fail message to refer to [Big_percent.Stable.V3] instead of [Bignum].
    *)
    let t_of_sexp sexp =
      t_of_sexp_with_version_parameter_for_failure_case sexp ~which_version_failed:3
    ;;
  end

  module V4 = struct
    include V3

    let t_sexp_grammar = { Sexplib.Sexp_grammar.untyped = String }

    let t_of_sexp sexp =
      t_of_sexp_with_version_parameter_for_failure_case sexp ~which_version_failed:4
    ;;

    include Make_stringable (struct
        type nonrec t = t

        let which_suffix = which_suffix
        let nan_suffix = "bp"
        let inf_suffix = "x"
      end)

    include (
      Sexpable.Stable.Of_stringable.V1 [@modality portable] (struct
        type nonrec t = t

        let of_string str =
          really_of_string
            str
            ~allow_nan_or_inf:true
            ~bignum_of_string:(fun str -> str |> Sexp.of_string |> t_of_sexp)
            ~function_to_refer_to_in_exception:"Stable.V4.t_of_sexp"
        ;;

        let to_string t =
          let bignum_to_string t = t |> sexp_of_t |> Sexp.to_string in
          really_to_string bignum_to_string t
        ;;
      end) :
      sig
      @@ portable
        include Sexpable.S with type t := t
      end)
  end
end

let of_bp_int i = i |> of_int |> of_bp
let to_bp_int t = t |> to_bp |> to_int

(* Zarith overflow. *)
let to_bp_int_exn t = t |> to_bp |> to_int_exn
let of_percent_decimal p = p |> Percent.to_mult |> Bignum.of_float_decimal |> of_mult
let of_percent_dyadic p = p |> Percent.to_mult |> Bignum.of_float_dyadic |> of_mult
let to_percent t = t |> to_mult |> Bignum.to_float |> Percent.of_mult

(* Currently only supports decimal rounding. *)
let round_decimal_mult p ~digits = round_decimal p ~digits

let round_decimal_percentage p ~digits =
  round_decimal (p * of_int 100) ~digits / of_int 100
;;

let round_decimal_bp p ~digits =
  round_decimal ~dir:`Nearest (p * of_int 10000) ~digits / of_int 10000
;;

let of_string_allow_nan_and_inf_internal string =
  really_of_string ~allow_nan_or_inf:true ~bignum_of_string:of_string string
;;

include Make_stringable (struct
    type nonrec t = t

    let which_suffix = which_suffix
    let nan_suffix = "bp"
    let inf_suffix = "x"
  end)

include (
  Sexpable.Stable.Of_stringable.V1 [@modality portable] (struct
    type nonrec t = t

    let of_string string =
      of_string_allow_nan_and_inf_internal
        ~function_to_refer_to_in_exception:"Unstable.t_of_sexp"
        string
    ;;

    let to_string = to_string_accurate
  end) :
  sig
  @@ portable
    include Sexpable.S with type t := t
  end)

let of_string_exn string =
  really_of_string
    string
    ~allow_nan_or_inf:false
    ~bignum_of_string:of_string
    ~function_to_refer_to_in_exception:"of_string_exn"
;;

let of_string_allow_nan_and_inf string =
  of_string_allow_nan_and_inf_internal
    string
    ~function_to_refer_to_in_exception:"of_string_allow_nan_and_inf"
;;

let apply t b = t * b
let scale t b = t * b
let zero = zero
let one_hundred_percent = of_int 1
let ( // ) x y = of_mult x / of_mult y
let is_zero t = t = zero
let is_inf t = t = Bignum.of_string "inf"

module Unstable = struct
  type t = Bignum.Unstable.t [@@deriving bin_io, compare ~localize, equal ~localize, hash]

  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp
end

module Always_percentage = struct
  include Make_stringable (struct
      type nonrec t = t

      let which_suffix bignum_to_string t = bignum_to_string (t * of_int 100) ^ "%"
      let nan_suffix = "%"
      let inf_suffix = "%"
    end)

  include (
    Sexpable.Stable.Of_stringable.V1 [@modality portable] (struct
      type nonrec t = t

      let of_string string =
        of_string_allow_nan_and_inf_internal
          string
          ~function_to_refer_to_in_exception:"Always_percentage.t_of_sexp"
      ;;

      let to_string = to_string_accurate
    end) :
    sig
    @@ portable
      include Sexpable.S with type t := t
    end)
end
