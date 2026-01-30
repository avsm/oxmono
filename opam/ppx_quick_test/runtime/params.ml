open! Core

module One = struct
  type 'a t =
    { sexp_of : 'a -> Sexp.t
    ; of_sexp : (Sexp.t -> 'a) option
    ; generator : 'a Quickcheck.Generator.t
    ; shrinker : 'a Quickcheck.Shrinker.t
    }
end

type ('tup, 'fn, 'res) many =
  | [] : (unit, 'res, 'res) many
  | ( :: ) : 'a One.t * ('tup, 'fn, 'res) many -> ('a * 'tup, 'a -> 'fn, 'res) many

type ('tup, 'fn, 'res) t =
  | One : 'a One.t -> ('a, 'a -> 'res, 'res) t
  | Many : ('tup, 'fn, 'res) many -> ('tup, 'fn, 'res) t

let call : type tup fn res. (tup, fn, res) t -> tup -> fn -> res =
  let rec loop : type tup fn res. (tup, fn, res) many -> tup -> fn -> res =
    fun params input f ->
    match params, input with
    | [], () -> f
    | _ :: params, (value, input) -> loop params input (f value)
  in
  fun params tup fn ->
    match params with
    | One _ -> fn tup
    | Many many -> loop many tup fn
;;

let sexp_of_tup : type tup fn res. (tup, fn, res) t -> (tup -> Sexp.t) Staged.t =
  let rec loop : type tup fn res. (tup, fn, res) many -> tup -> Sexp.t list =
    fun params tup ->
    match params, tup with
    | [], () -> []
    | one :: params, (item, tup) -> one.sexp_of item :: loop params tup
  in
  function
  | One one -> stage one.sexp_of
  | Many many -> stage (fun tup -> Sexp.List (loop many tup))
;;

let tup_of_sexp : type tup fn res. (tup, fn, res) t -> (Sexp.t -> tup) Staged.t =
  let one_of_sexp (one : _ One.t) sexp =
    match one.of_sexp with
    | Some of_sexp -> of_sexp sexp
    | None -> raise_s [%message "[ppx_quick_test] bug! Unexpectedly called [of_sexp]."]
  in
  let rec loop : type tup fn res. (tup, fn, res) many -> Sexp.t list -> tup =
    fun params sexps ->
    match params, sexps with
    | [], [] -> ()
    | one :: params, sexp :: sexps -> one_of_sexp one sexp, loop params sexps
    | [], _ :: _ -> raise_s [%message "too many entries in sexp"]
    | _ :: _, [] -> raise_s [%message "not enough entries in sexp"]
  in
  function
  | One one -> stage (one_of_sexp one)
  | Many many ->
    stage (fun (sexp : Sexp.t) ->
      match sexp with
      | List sexps -> loop many sexps
      | Atom _ -> raise_s [%message "expected list of sexps"])
;;

let tup_generator : type tup fn res. (tup, fn, res) t -> tup Quickcheck.Generator.t =
  let open Quickcheck.Generator.Let_syntax in
  let rec loop : type tup fn res. (tup, fn, res) many -> tup Quickcheck.Generator.t
    = function
    | [] -> return ()
    | one :: params ->
      let%bind rest = loop params in
      let%map one_value = one.generator in
      one_value, rest
  in
  function
  | One one -> one.generator
  | Many many -> loop many
;;

let tup_shrinker : type tup fn res. (tup, fn, res) t -> tup Quickcheck.Shrinker.t =
  let module Shrinker = Quickcheck.Shrinker in
  let rec loop : type tup fn res. (tup, fn, res) many -> tup Quickcheck.Shrinker.t list
    = function
    | [] -> []
    | one :: params ->
      let rest_shrinkers = loop params in
      let shrink_one =
        Shrinker.create (fun (one_value, rest) ->
          Sequence.map (Shrinker.shrink one.shrinker one_value) ~f:(fun one_value ->
            one_value, rest))
      in
      let shrink_rest =
        List.map rest_shrinkers ~f:(fun shrinker ->
          Shrinker.create (fun (one_value, rest) ->
            Sequence.map (Shrinker.shrink shrinker rest) ~f:(fun rest -> one_value, rest)))
      in
      shrink_one :: shrink_rest
  in
  function
  | One one -> one.shrinker
  | Many many ->
    let shrinkers = loop many in
    Shrinker.create (fun tup ->
      List.map shrinkers ~f:(fun shrinker -> Shrinker.shrink shrinker tup)
      |> Sequence.round_robin)
;;
