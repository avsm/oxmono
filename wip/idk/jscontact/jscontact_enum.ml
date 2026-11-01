(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module type VALUES = sig
  type t : immutable_data

  val kind : string @@ portable
  val to_string : t -> string @@ portable
  val of_string : string -> t @@ portable
  val is_vendor : t -> bool @@ portable
end

module type VALUE = sig
  type t : immutable_data

  val to_string : t -> string @@ portable
  val of_string : string -> t @@ portable
  val is_vendor : t -> bool @@ portable
  val equal : t -> t -> bool @@ portable
  val compare : t -> t -> int @@ portable
  val pp : Format.formatter -> t -> unit @@ portable
  val validate : t -> t Jscontact_valid.t @@ portable
  val equal_set : t list -> t list -> bool @@ portable
  val jsont : t Jsont.t @@ portable
  val set_jsont : t list Jsont.t @@ portable
end

module type S = sig
  type t : immutable_data

  include VALUE with type t := t

  val validate_set : t list -> t list Jscontact_valid.t @@ portable
end

module Make (V : VALUES) = struct
  include V

  let equal a b = String.equal (to_string a) (to_string b)
  let compare a b = String.compare (to_string a) (to_string b)
  let pp ppf v = Format.pp_print_string ppf (to_string v)

  let validate v =
    if not (is_vendor v) then Ok v
    else
      match Jscontact_vendor.validate_extension (to_string v) with
      | Ok _ -> Ok v
      | Error msg -> Error (Printf.sprintf "%s: %s" kind msg)

  let validate_set vs = Jscontact_valid.list validate vs

  let equal_set a b =
    List.equal equal (List.sort_uniq compare a) (List.sort_uniq compare b)

  (* An encoder must report a bad value with Jsont.Error, not Invalid_argument:
     Jsont_bytesrw.encode_string catches the former alone. *)
  let enc v =
    match validate v with
    | Ok v -> to_string v
    | Error msg -> Jsont.Error.msgf Jsont.Meta.none "%s" msg

  let jsont = Jsont.map ~kind ~dec:of_string ~enc Jsont.string

  let set_jsont =
    Jscontact_json.Map.bool_set ~kind ~show:to_string ~of_string ~to_string:enc
      ()
end
