(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let equal_contexts a b = Option.equal Jscontact_context.equal_set a b

let pp_contexts ppf cs =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
    Jscontact_context.pp ppf cs

module Organization = struct
  module Org_unit = struct
    type t = {
      name : string;
      sort_as : string option;
      unknown : Jscontact_unknown.t;
    }

    let make ?sort_as ?(unknown = Jscontact_unknown.empty) name =
      { name; sort_as; unknown }

    let equal a b =
      String.equal a.name b.name
      && Option.equal String.equal a.sort_as b.sort_as
      && Jscontact_unknown.equal a.unknown b.unknown

    let pp ppf u = Format.fprintf ppf "@[%s@]" u.name
    let kind = "OrgUnit"

    let validate u =
      Jscontact_valid.(
        let* _ =
          in_ kind (Jscontact_unknown.validate ~in_type:kind u.unknown)
        in
        ok u)

    let ctor () name sort_as unknown = { name; sort_as; unknown }

    let jsont =
      Jsont.Object.map ~kind ctor
      |> Jscontact_json.type_mem kind
      |> Jsont.Object.mem "name" Jsont.string ~enc:(fun t -> t.name)
      |> Jsont.Object.opt_mem "sortAs" Jsont.string ~enc:(fun t -> t.sort_as)
      |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
          t.unknown)
      |> Jsont.Object.finish
  end

  type t = {
    name : string option;
    units : Org_unit.t list option;
    sort_as : string option;
    contexts : Jscontact_context.t list option;
    unknown : Jscontact_unknown.t;
  }

  let make ?name ?units ?sort_as ?contexts ?(unknown = Jscontact_unknown.empty)
      () =
    { name; units; sort_as; contexts; unknown }

  let equal a b =
    Option.equal String.equal a.name b.name
    && Option.equal (List.equal Org_unit.equal) a.units b.units
    && Option.equal String.equal a.sort_as b.sort_as
    && equal_contexts a.contexts b.contexts
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf o =
    let name ppf = function
      | None -> Format.pp_print_string ppf "-"
      | Some n -> Format.pp_print_string ppf n
    in
    let units ppf = function
      | None -> ()
      | Some us ->
          List.iter (fun u -> Format.fprintf ppf ",@ %a" Org_unit.pp u) us
    in
    Format.fprintf ppf "@[%a%a@]" name o.name units o.units

  let kind = "Organization"

  (* RFC 9553 Section 2.2.3: at least one of the name and units properties
     MUST be set, and units, if set, MUST contain at least one entry. *)
  let validate o =
    Jscontact_valid.(
      let* () =
        check
          (o.name <> None || o.units <> None)
          "neither name nor units is set, and at least one must be"
      in
      let* _ =
        match o.units with
        | None -> ok []
        | Some us ->
            let* () = check (us <> []) "units: is set but has no entry" in
            in_ "units" (list Org_unit.validate us)
      in
      let* _ = opt Jscontact_context.validate_set o.contexts in
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind o.unknown) in
      ok o)

  let ctor () name units sort_as contexts unknown =
    { name; units; sort_as; contexts; unknown }

  let jsont =
    Jsont.Object.map ~kind ctor
    |> Jscontact_json.type_mem kind
    |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun t -> t.name)
    |> Jsont.Object.opt_mem "units" (Jsont.list Org_unit.jsont) ~enc:(fun t ->
        t.units)
    |> Jsont.Object.opt_mem "sortAs" Jsont.string ~enc:(fun t -> t.sort_as)
    |> Jsont.Object.opt_mem "contexts" Jscontact_context.set_jsont
         ~enc:(fun t -> t.contexts)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end

module Title = struct
  module Kind = struct
    type t = [ `Title | `Role | `Vendor of string ]

    include Jscontact_enum.Make (struct
      type nonrec t = t

      let kind = "kind"

      let to_string = function
        | `Title -> "title"
        | `Role -> "role"
        | `Vendor s -> s

      let of_string = function
        | "title" -> `Title
        | "role" -> `Role
        | s -> `Vendor s

      let is_vendor = function `Vendor _ -> true | _ -> false
    end)
  end

  type t = {
    name : string;
    kind : Kind.t;
    organization_id : Jscontact_id.t option;
    unknown : Jscontact_unknown.t;
  }

  let default_kind : Kind.t = `Title

  let make ?(kind = default_kind) ?organization_id
      ?(unknown = Jscontact_unknown.empty) name =
    { name; kind; organization_id; unknown }

  let equal a b =
    String.equal a.name b.name && Kind.equal a.kind b.kind
    && Option.equal Jscontact_id.equal a.organization_id b.organization_id
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf t = Format.fprintf ppf "@[%s (%a)@]" t.name Kind.pp t.kind
  let kind = "Title"

  let validate t =
    Jscontact_valid.(
      let* _ = Kind.validate t.kind in
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind t.unknown) in
      ok t)

  let ctor () name kind organization_id unknown =
    { name; kind; organization_id; unknown }

  (* RFC 9553 Section 2.2.5: the kind property defaults to "title", so an
     absent member decodes to `Title and that value is not written back. *)
  let jsont =
    Jsont.Object.map ~kind ctor
    |> Jscontact_json.type_mem kind
    |> Jsont.Object.mem "name" Jsont.string ~enc:(fun t -> t.name)
    |> Jsont.Object.mem "kind" Kind.jsont ~dec_absent:(fun () -> default_kind)
         ~enc:(fun t -> t.kind)
         ~enc_omit:(fun k -> Kind.equal k default_kind)
    |> Jsont.Object.opt_mem "organizationId" Jscontact_id.jsont ~enc:(fun t ->
        t.organization_id)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end

module Speak_to_as = struct
  module Grammatical_gender = struct
    type t =
      [ `Animate
      | `Common
      | `Feminine
      | `Inanimate
      | `Masculine
      | `Neuter
      | `Vendor of string ]

    include Jscontact_enum.Make (struct
      type nonrec t = t

      let kind = "grammaticalGender"

      let to_string = function
        | `Animate -> "animate"
        | `Common -> "common"
        | `Feminine -> "feminine"
        | `Inanimate -> "inanimate"
        | `Masculine -> "masculine"
        | `Neuter -> "neuter"
        | `Vendor s -> s

      let of_string = function
        | "animate" -> `Animate
        | "common" -> `Common
        | "feminine" -> `Feminine
        | "inanimate" -> `Inanimate
        | "masculine" -> `Masculine
        | "neuter" -> `Neuter
        | s -> `Vendor s

      let is_vendor = function `Vendor _ -> true | _ -> false
    end)
  end

  module Pronouns = struct
    type t = {
      pronouns : string;
      contexts : Jscontact_context.t list option;
      pref : int option;
      unknown : Jscontact_unknown.t;
    }

    let make ?contexts ?pref ?(unknown = Jscontact_unknown.empty) pronouns =
      { pronouns; contexts; pref; unknown }

    let equal a b =
      String.equal a.pronouns b.pronouns
      && equal_contexts a.contexts b.contexts
      && Option.equal Int.equal a.pref b.pref
      && Jscontact_unknown.equal a.unknown b.unknown

    let pp ppf p =
      let ctx ppf = function
        | None -> ()
        | Some cs -> Format.fprintf ppf " (%a)" pp_contexts cs
      in
      Format.fprintf ppf "@[%s%a@]" p.pronouns ctx p.contexts

    let kind = "Pronouns"

    let validate p =
      Jscontact_valid.(
        let* _ = opt Jscontact_context.validate_set p.contexts in
        let* _ = opt Jscontact_pref.validate p.pref in
        let* _ =
          in_ kind (Jscontact_unknown.validate ~in_type:kind p.unknown)
        in
        ok p)

    let ctor () pronouns contexts pref unknown =
      { pronouns; contexts; pref; unknown }

    let jsont =
      Jsont.Object.map ~kind ctor
      |> Jscontact_json.type_mem kind
      |> Jsont.Object.mem "pronouns" Jsont.string ~enc:(fun t -> t.pronouns)
      |> Jsont.Object.opt_mem "contexts" Jscontact_context.set_jsont
           ~enc:(fun t -> t.contexts)
      |> Jsont.Object.opt_mem "pref" Jscontact_pref.jsont ~enc:(fun t -> t.pref)
      |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
          t.unknown)
      |> Jsont.Object.finish
  end

  type t = {
    grammatical_gender : Grammatical_gender.t option;
    pronouns : (Jscontact_id.t * Pronouns.t) list option;
    unknown : Jscontact_unknown.t;
  }

  let make ?grammatical_gender ?pronouns ?(unknown = Jscontact_unknown.empty) ()
      =
    { grammatical_gender; pronouns; unknown }

  let equal_entries a b =
    Jscontact_json.Map.equal ~key:Jscontact_id.compare Pronouns.equal a b

  let equal a b =
    Option.equal Grammatical_gender.equal a.grammatical_gender
      b.grammatical_gender
    && Option.equal equal_entries a.pronouns b.pronouns
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf s =
    let gender ppf = function
      | None -> Format.pp_print_string ppf "-"
      | Some g -> Grammatical_gender.pp ppf g
    in
    let pronouns ppf = function
      | None -> ()
      | Some ps ->
          List.iter (fun (_, p) -> Format.fprintf ppf ",@ %a" Pronouns.pp p) ps
    in
    Format.fprintf ppf "@[%a%a@]" gender s.grammatical_gender pronouns
      s.pronouns

  let kind = "SpeakToAs"

  (* RFC 9553 Section 2.2.4: at least one of the grammaticalGender and pronouns
     properties MUST be set. *)
  let validate s =
    Jscontact_valid.(
      let* () =
        check
          (s.grammatical_gender <> None || s.pronouns <> None)
          "neither grammaticalGender nor pronouns is set, and at least one \
           must be"
      in
      let* _ = opt Grammatical_gender.validate s.grammatical_gender in
      let* _ = in_ "pronouns" (opt (entries Pronouns.validate) s.pronouns) in
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind s.unknown) in
      ok s)

  let ctor () grammatical_gender pronouns unknown =
    { grammatical_gender; pronouns; unknown }

  let jsont =
    Jsont.Object.map ~kind ctor
    |> Jscontact_json.type_mem kind
    |> Jsont.Object.opt_mem "grammaticalGender" Grammatical_gender.jsont
         ~enc:(fun t -> t.grammatical_gender)
    |> Jsont.Object.opt_mem "pronouns" (Jscontact_json.Map.of_id Pronouns.jsont)
         ~enc:(fun t -> t.pronouns)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end
