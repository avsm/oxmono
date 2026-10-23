(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

module Scheme = struct
  type t = Doi | Pmc

  let all = [ Doi; Pmc ]
  let to_string = function Doi -> "doi" | Pmc -> "pmc"

  let of_string s =
    match String.lowercase_ascii (String.trim s) with
    | "doi" -> Ok Doi
    | "pmc" -> Ok Pmc
    | s -> Error (Printf.sprintf "unknown identifier scheme %S" s)
end

type t =
  | Doi of Doi.t
  | Pmc of int

let scheme = function Doi _ -> Scheme.Doi | Pmc _ -> Scheme.Pmc

(* PMCIDs are written with the "PMC" tag in the corpus filenames and
   in NCBI's own tables, so the canonical id keeps it; the number is
   what is stored, which is also what the shard arithmetic needs. *)
let pmc_id n = "PMC" ^ string_of_int n

let id = function Doi d -> Doi.to_string d | Pmc n -> pmc_id n

let of_pmc s =
  let s = String.trim s in
  let digits =
    let tag = "pmc" in
    let n = String.length tag in
    if
      String.length s > n
      && String.equal (String.lowercase_ascii (String.sub s 0 n)) tag
    then String.sub s n (String.length s - n)
    else s
  in
  if digits = "" then Error "the PMCID number is empty"
  else if not (String.for_all (function '0' .. '9' -> true | _ -> false) digits)
  then Error (Printf.sprintf "the PMCID %S is not \"PMC\" and digits" s)
  else
    match int_of_string_opt digits with
    | Some n when n > 0 -> Ok (Pmc n)
    | _ -> Error (Printf.sprintf "the PMCID %S is not a positive number" s)

let of_id (scheme : Scheme.t) s =
  match scheme with
  | Doi -> Result.map (fun d -> Doi d) (Doi.of_string s)
  | Pmc -> of_pmc s

let of_string s =
  let s = String.trim s in
  match String.index_opt s ':' with
  | None -> Error "a uid is <scheme>:<id>, with no \":\" present"
  | Some i ->
    let label = String.sub s 0 i in
    let rest = String.sub s (i + 1) (String.length s - i - 1) in
    Result.bind (Scheme.of_string label) (fun scheme -> of_id scheme rest)

let of_input s =
  let s = String.trim s in
  match of_string s with
  | Ok uid -> Ok uid
  | Error _ ->
      if String.starts_with ~prefix:"PMC" (String.uppercase_ascii s)
      then of_id Scheme.Pmc s else of_id Scheme.Doi s

let to_string t = Scheme.to_string (scheme t) ^ ":" ^ id t
let doi = function Doi d -> Some d | Pmc _ -> None
let pmcid = function Doi _ -> None | Pmc n -> Some (pmc_id n)


let compare a b =
  match a, b with
  | Doi a, Doi b -> String.compare (Doi.to_string a) (Doi.to_string b)
  | Pmc a, Pmc b -> Int.compare a b
  | Doi _, Pmc _ -> -1
  | Pmc _, Doi _ -> 1

let jsont =
  let dec s =
    match of_string s with
    | Ok u -> u
    | Error e -> Jsont.Error.msg Jsont.Meta.none e
  in
  Jsont.map ~kind:"Uid" ~dec ~enc:to_string Jsont.string
