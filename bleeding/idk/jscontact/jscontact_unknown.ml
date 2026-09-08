(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = Jsont.json

let empty = Jsont.Object ([], Jsont.Meta.none)

(* Every value of type t is a Jsont.Object; the other case is for totality. *)
let members = function Jsont.Object (mems, _) -> mems | _ -> []
let meta = function Jsont.Object (_, m) -> m | _ -> Jsont.Meta.none
let is_empty u = members u = []
let find u name = Option.map snd (Jsont.Json.find_mem name (members u))
let names u = List.map (fun ((n, _), _) -> n) (members u)
let to_list u = List.map (fun ((n, _), v) -> (n, v)) (members u)
let to_json u = u

let without u name =
  List.filter (fun ((n, _), _) -> not (String.equal n name)) (members u)

let add u name v =
  Jsont.Object (without u name @ [ ((name, Jsont.Meta.none), v) ], meta u)

let remove u name = Jsont.Object (without u name, meta u)
let of_list l = List.fold_left (fun u (n, v) -> add u n v) empty l
let equal a b = Jsont.Json.equal a b

let pp ppf u =
  Format.fprintf ppf "@[<1>{%a}@]"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
       Format.pp_print_string)
    (names u)

(* Section 1.3 requires JSContact data to be valid I-JSON, and RFC 7493
   Section 2.3 forbids an object from naming the same member twice. Jsont's own
   json_mems keeps both, which would decode and then re-encode invalid JSON, so
   the members are collected here instead. *)
let mems =
  let dec_empty () = [] in
  let dec_add meta name v acc = (((name, meta) : Jsont.name), v) :: acc in
  let dec_finish meta acc =
    let by_name ((a, _), _) ((b, _), _) = String.compare a b in
    let rec check = function
      | ((a, m), _) :: (((b, _), _) :: _ as rest) ->
          if String.equal a b then Jsont.Error.msgf m "duplicate member %S" a
          else check rest
      | _ -> ()
    in
    check (List.stable_sort by_name acc);
    Jsont.Object (List.rev acc, meta)
  in
  let enc =
    {
      Jsont.Object.Mems.enc =
        (fun add u acc ->
          List.fold_left (fun acc ((n, m), v) -> add m n v acc) acc (members u));
    }
  in
  Jsont.Object.Mems.map ~kind:"unknown members" ~dec_empty ~dec_add ~dec_finish
    ~enc Jsont.json

let validate ?in_type u =
  let rec check = function
    | [] -> Ok u
    | name :: names -> (
        match Jscontact_registry.validate_property_name ?in_type name with
        | Ok _ -> check names
        | Error msg -> Error msg)
  in
  check (names u)
