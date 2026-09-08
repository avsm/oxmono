(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Component = struct
  module Kind = struct
    type t =
      [ `Room
      | `Apartment
      | `Floor
      | `Building
      | `Number
      | `Name
      | `Block
      | `Subdistrict
      | `District
      | `Locality
      | `Region
      | `Postcode
      | `Country
      | `Direction
      | `Landmark
      | `Post_office_box
      | `Separator
      | `Vendor of string ]

    include Jscontact_enum.Make (struct
      type nonrec t = t

      let kind = "kind"

      let to_string = function
        | `Room -> "room"
        | `Apartment -> "apartment"
        | `Floor -> "floor"
        | `Building -> "building"
        | `Number -> "number"
        | `Name -> "name"
        | `Block -> "block"
        | `Subdistrict -> "subdistrict"
        | `District -> "district"
        | `Locality -> "locality"
        | `Region -> "region"
        | `Postcode -> "postcode"
        | `Country -> "country"
        | `Direction -> "direction"
        | `Landmark -> "landmark"
        | `Post_office_box -> "postOfficeBox"
        | `Separator -> "separator"
        | `Vendor s -> s

      let of_string = function
        | "room" -> `Room
        | "apartment" -> `Apartment
        | "floor" -> `Floor
        | "building" -> `Building
        | "number" -> `Number
        | "name" -> `Name
        | "block" -> `Block
        | "subdistrict" -> `Subdistrict
        | "district" -> `District
        | "locality" -> `Locality
        | "region" -> `Region
        | "postcode" -> `Postcode
        | "country" -> `Country
        | "direction" -> `Direction
        | "landmark" -> `Landmark
        | "postOfficeBox" -> `Post_office_box
        | "separator" -> `Separator
        | s -> `Vendor s

      let is_vendor = function `Vendor _ -> true | _ -> false
    end)
  end

  type t = {
    value : string;
    kind : Kind.t;
    phonetic : string option;
    unknown : Jscontact_unknown.t;
  }

  let make ?phonetic ?(unknown = Jscontact_unknown.empty) kind value =
    { value; kind; phonetic; unknown }

  let equal a b =
    String.equal a.value b.value
    && Kind.equal a.kind b.kind
    && Option.equal String.equal a.phonetic b.phonetic
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf c = Format.fprintf ppf "@[%a %S@]" Kind.pp c.kind c.value
  let kind = "AddressComponent"

  let validate c =
    Jscontact_valid.(
      let* _ = Kind.validate c.kind in
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind c.unknown) in
      ok c)

  let ctor value kind phonetic unknown = { value; kind; phonetic; unknown }

  let jsont =
    Jsont.Object.map ~kind (fun () -> ctor)
    |> Jscontact_json.type_mem kind
    |> Jsont.Object.mem "value" Jsont.string ~enc:(fun t -> t.value)
    |> Jsont.Object.mem "kind" Kind.jsont ~enc:(fun t -> t.kind)
    |> Jsont.Object.opt_mem "phonetic" Jsont.string ~enc:(fun t -> t.phonetic)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end

type t = {
  components : Component.t list option;
  is_ordered : bool;
  country_code : string option;
  coordinates : string option;
  time_zone : string option;
  contexts : Jscontact_context.t list option;
  full : string option;
  default_separator : string option;
  pref : int option;
  phonetic_script : string option;
  phonetic_system : Jscontact_phonetic.t option;
  unknown : Jscontact_unknown.t;
}

let make ?components ?(is_ordered = false) ?country_code ?coordinates ?time_zone
    ?contexts ?full ?default_separator ?pref ?phonetic_script ?phonetic_system
    ?(unknown = Jscontact_unknown.empty) () =
  {
    components;
    is_ordered;
    country_code;
    coordinates;
    time_zone;
    contexts;
    full;
    default_separator;
    pref;
    phonetic_script;
    phonetic_system;
    unknown;
  }

let equal a b =
  Option.equal (List.equal Component.equal) a.components b.components
  && Bool.equal a.is_ordered b.is_ordered
  && Option.equal String.equal a.country_code b.country_code
  && Option.equal String.equal a.coordinates b.coordinates
  && Option.equal String.equal a.time_zone b.time_zone
  && Option.equal Jscontact_context.equal_set a.contexts b.contexts
  && Option.equal String.equal a.full b.full
  && Option.equal String.equal a.default_separator b.default_separator
  && Option.equal Int.equal a.pref b.pref
  && Option.equal String.equal a.phonetic_script b.phonetic_script
  && Option.equal Jscontact_phonetic.equal a.phonetic_system b.phonetic_system
  && Jscontact_unknown.equal a.unknown b.unknown

let pp ppf a =
  match a.full with
  | Some full -> Format.pp_print_string ppf full
  | None ->
      let value ppf (c : Component.t) = Format.pp_print_string ppf c.value in
      Format.fprintf ppf "@[%a@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space value)
        (Option.value ~default:[] a.components)

let kind = "Address"

(* RFC 9553 Section 2.5.1.1: coordinates is a "geo:" URI. RFC 3986 Section 3.1
   makes a URI scheme case-insensitive. *)
let is_geo_uri s =
  String.length s > 4
  && String.equal (String.lowercase_ascii (String.sub s 0 4)) "geo:"
  && Jscontact_uri.is_valid s

let validate a =
  let is_separator (c : Component.t) = Component.Kind.equal c.kind `Separator in
  let components = Option.value ~default:[] a.components in
  Jscontact_valid.(
    let* () =
      check
        (Option.is_some a.components
        || Option.is_some a.coordinates
        || Option.is_some a.country_code
        || Option.is_some a.full || Option.is_some a.time_zone)
        "at least one of components, coordinates, countryCode, full and \
         timeZone must be set"
    in
    let* () =
      Jscontact_components.validate ~is_separator
        ~has_phonetic:(fun (c : Component.t) -> Option.is_some c.phonetic)
        ~is_ordered:a.is_ordered ~default_separator:a.default_separator
        ~has_phonetic_system:
          (Option.is_some a.phonetic_script || Option.is_some a.phonetic_system)
        a.components
    in
    let* _ = in_ "components" (list Component.validate components) in
    let* _ =
      match a.country_code with
      | None -> ok ()
      | Some c ->
          check
            (String.length c = 2 && String.for_all Jscontact_ascii.is_alpha c)
            "countryCode: %S is not an Alpha-2 country code, which is two \
             letters"
            c
    in
    let* _ =
      match a.coordinates with
      | None -> ok ()
      | Some c -> check (is_geo_uri c) "coordinates: %S is not a \"geo:\" URI" c
    in
    let* _ = opt (Jscontact_context.validate_set ~address:true) a.contexts in
    let* _ = opt Jscontact_pref.validate a.pref in
    let* _ = opt Jscontact_phonetic.validate_script a.phonetic_script in
    let* _ = opt Jscontact_phonetic.validate a.phonetic_system in
    let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind a.unknown) in
    ok a)

let ctor components is_ordered country_code coordinates time_zone contexts full
    default_separator pref phonetic_script phonetic_system unknown =
  {
    components;
    is_ordered;
    country_code;
    coordinates;
    time_zone;
    contexts;
    full;
    default_separator;
    pref;
    phonetic_script;
    phonetic_system;
    unknown;
  }

let jsont =
  Jsont.Object.map ~kind (fun () -> ctor)
  |> Jscontact_json.type_mem kind
  |> Jsont.Object.opt_mem "components" (Jsont.list Component.jsont)
       ~enc:(fun t -> t.components)
  |> Jsont.Object.mem "isOrdered" Jsont.bool ~dec_absent:(fun () -> false)
       ~enc_omit:(fun b -> not b)
       ~enc:(fun t -> t.is_ordered)
  |> Jsont.Object.opt_mem "countryCode" Jsont.string ~enc:(fun t ->
      t.country_code)
  |> Jsont.Object.opt_mem "coordinates" Jsont.string ~enc:(fun t ->
      t.coordinates)
  |> Jsont.Object.opt_mem "timeZone" Jsont.string ~enc:(fun t -> t.time_zone)
  |> Jsont.Object.opt_mem "contexts" Jscontact_context.set_jsont ~enc:(fun t ->
      t.contexts)
  |> Jsont.Object.opt_mem "full" Jsont.string ~enc:(fun t -> t.full)
  |> Jsont.Object.opt_mem "defaultSeparator" Jsont.string ~enc:(fun t ->
      t.default_separator)
  |> Jsont.Object.opt_mem "pref" Jscontact_pref.jsont ~enc:(fun t -> t.pref)
  |> Jsont.Object.opt_mem "phoneticScript" Jsont.string ~enc:(fun t ->
      t.phonetic_script)
  |> Jsont.Object.opt_mem "phoneticSystem" Jscontact_phonetic.jsont
       ~enc:(fun t -> t.phonetic_system)
  |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t -> t.unknown)
  |> Jsont.Object.finish
