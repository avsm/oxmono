(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Component = struct
  module Kind = struct
    type t =
      [ `Title
      | `Given
      | `Given2
      | `Surname
      | `Surname2
      | `Credential
      | `Generation
      | `Separator
      | `Vendor of string ]

    include Jscontact_enum.Make (struct
      type nonrec t = t

      let kind = "kind"

      let to_string = function
        | `Title -> "title"
        | `Given -> "given"
        | `Given2 -> "given2"
        | `Surname -> "surname"
        | `Surname2 -> "surname2"
        | `Credential -> "credential"
        | `Generation -> "generation"
        | `Separator -> "separator"
        | `Vendor s -> s

      let of_string = function
        | "title" -> `Title
        | "given" -> `Given
        | "given2" -> `Given2
        | "surname" -> `Surname
        | "surname2" -> `Surname2
        | "credential" -> `Credential
        | "generation" -> `Generation
        | "separator" -> `Separator
        | s -> `Vendor s

      let is_vendor = function `Vendor _ -> true | _ -> false
    end)
  end

  type t = {
    kind : Kind.t;
    value : string;
    phonetic : string option;
    unknown : Jscontact_unknown.t;
  }

  let make ?phonetic ?(unknown = Jscontact_unknown.empty) kind value =
    { kind; value; phonetic; unknown }

  let equal a b =
    Kind.equal a.kind b.kind
    && String.equal a.value b.value
    && Option.equal String.equal a.phonetic b.phonetic
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf c = Format.fprintf ppf "@[%a:%S@]" Kind.pp c.kind c.value
  let kind = "NameComponent"

  let validate c =
    Jscontact_valid.(
      let* _ = Kind.validate c.kind in
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind c.unknown) in
      ok c)

  let ctor value kind phonetic unknown = { kind; value; phonetic; unknown }

  (* The members are written in the order Section 2.2.1.2 defines them, which
     is the order Section 2.5.1.2 defines an AddressComponent in too. *)
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

module Nickname = struct
  type t = {
    name : string;
    contexts : Jscontact_context.t list option;
    pref : int option;
    unknown : Jscontact_unknown.t;
  }

  let make ?contexts ?pref ?(unknown = Jscontact_unknown.empty) name =
    { name; contexts; pref; unknown }

  let equal a b =
    String.equal a.name b.name
    && Option.equal Jscontact_context.equal_set a.contexts b.contexts
    && Option.equal Int.equal a.pref b.pref
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf n = Format.fprintf ppf "%S" n.name
  let kind = "Nickname"

  let validate n =
    Jscontact_valid.(
      let* _ = opt Jscontact_context.validate_set n.contexts in
      let* _ = opt Jscontact_pref.validate n.pref in
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind n.unknown) in
      ok n)

  let ctor name contexts pref unknown = { name; contexts; pref; unknown }

  let jsont =
    Jsont.Object.map ~kind (fun () -> ctor)
    |> Jscontact_json.type_mem kind
    |> Jsont.Object.mem "name" Jsont.string ~enc:(fun t -> t.name)
    |> Jsont.Object.opt_mem "contexts" Jscontact_context.set_jsont
         ~enc:(fun t -> t.contexts)
    |> Jsont.Object.opt_mem "pref" Jscontact_pref.jsont ~enc:(fun t -> t.pref)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end

type t = {
  components : Component.t list option;
  is_ordered : bool;
  default_separator : string option;
  full : string option;
  sort_as : (Component.Kind.t * string) list option;
  phonetic_script : string option;
  phonetic_system : Jscontact_phonetic.t option;
  unknown : Jscontact_unknown.t;
}

let make ?components ?(is_ordered = false) ?default_separator ?full ?sort_as
    ?phonetic_script ?phonetic_system ?(unknown = Jscontact_unknown.empty) () =
  {
    components;
    is_ordered;
    default_separator;
    full;
    sort_as;
    phonetic_script;
    phonetic_system;
    unknown;
  }

let equal a b =
  Option.equal (List.equal Component.equal) a.components b.components
  && Bool.equal a.is_ordered b.is_ordered
  && Option.equal String.equal a.default_separator b.default_separator
  && Option.equal String.equal a.full b.full
  && Option.equal
       (Jscontact_json.Map.equal ~key:Component.Kind.compare String.equal)
       a.sort_as b.sort_as
  && Option.equal String.equal a.phonetic_script b.phonetic_script
  && Option.equal Jscontact_phonetic.equal a.phonetic_system b.phonetic_system
  && Jscontact_unknown.equal a.unknown b.unknown

let pp ppf n =
  match (n.full, n.components) with
  | Some full, _ -> Format.fprintf ppf "%S" full
  | None, Some cs ->
      Format.fprintf ppf "@[<1>[%a]@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
           Component.pp)
        cs
  | None, None -> Format.pp_print_string ppf "[]"

let kind = "Name"
let is_separator c = Component.Kind.equal c.Component.kind `Separator

(* RFC 9553 Section 2.2.1.1: each key of sortAs is a name component kind that
   at least one component of the name has. *)
let validate_sort_as cs m =
  let known k =
    List.exists (fun c -> Component.Kind.equal c.Component.kind k) cs
  in
  let rec loop = function
    | [] -> Jscontact_valid.ok m
    | (k, _) :: rest ->
        Jscontact_valid.(
          let* _ = in_ "sortAs" (Component.Kind.validate k) in
          let* () =
            check (known k) "sortAs: no component has the kind %S"
              (Component.Kind.to_string k)
          in
          loop rest)
  in
  loop m

let validate n =
  Jscontact_valid.(
    let* () =
      Jscontact_components.validate ~is_separator
        ~has_phonetic:(fun c -> c.Component.phonetic <> None)
        ~is_ordered:n.is_ordered ~default_separator:n.default_separator
        ~has_phonetic_system:
          (n.phonetic_script <> None || n.phonetic_system <> None)
        n.components
    in
    let* () =
      match (n.components, n.sort_as) with
      | Some cs, Some m -> Result.map ignore (validate_sort_as cs m)
      | Some _, None -> ok ()
      | None, sort_as ->
          let* () =
            check (n.full <> None) "components: must be set if full is not set"
          in
          check (sort_as = None)
            "sortAs: must not be set if components is not set"
    in
    let* _ = opt Jscontact_phonetic.validate_script n.phonetic_script in
    let* _ = opt Jscontact_phonetic.validate n.phonetic_system in
    let* _ = in_ "components" (opt (list Component.validate) n.components) in
    let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind n.unknown) in
    ok n)

let sort_as_jsont =
  Jscontact_json.Map.of_key ~kind:"sortAs" ~to_string:Component.Kind.to_string
    ~of_string:Component.Kind.of_string Jsont.string

let ctor components is_ordered default_separator full sort_as phonetic_script
    phonetic_system unknown =
  {
    components;
    is_ordered;
    default_separator;
    full;
    sort_as;
    phonetic_script;
    phonetic_system;
    unknown;
  }

let jsont =
  Jsont.Object.map ~kind (fun () -> ctor)
  |> Jscontact_json.type_mem kind
  |> Jsont.Object.opt_mem "components" (Jsont.list Component.jsont)
       ~enc:(fun t -> t.components)
  (* Section 2.2.1.1 defaults isOrdered to false, so it is not written back. *)
  |> Jsont.Object.mem "isOrdered" Jsont.bool ~dec_absent:(fun () -> false)
       ~enc:(fun t -> t.is_ordered)
       ~enc_omit:(fun v -> v = false)
  |> Jsont.Object.opt_mem "defaultSeparator" Jsont.string ~enc:(fun t ->
      t.default_separator)
  |> Jsont.Object.opt_mem "full" Jsont.string ~enc:(fun t -> t.full)
  |> Jsont.Object.opt_mem "sortAs" sort_as_jsont ~enc:(fun t -> t.sort_as)
  |> Jsont.Object.opt_mem "phoneticScript" Jsont.string ~enc:(fun t ->
      t.phonetic_script)
  |> Jsont.Object.opt_mem "phoneticSystem" Jscontact_phonetic.jsont
       ~enc:(fun t -> t.phonetic_system)
  |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t -> t.unknown)
  |> Jsont.Object.finish
