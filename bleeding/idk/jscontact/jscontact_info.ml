(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Relation = struct
  module Kind = struct
    type t =
      [ `Acquaintance
      | `Agent
      | `Child
      | `Co_resident
      | `Co_worker
      | `Colleague
      | `Contact
      | `Crush
      | `Date
      | `Emergency
      | `Friend
      | `Kin
      | `Me
      | `Met
      | `Muse
      | `Neighbor
      | `Parent
      | `Sibling
      | `Spouse
      | `Sweetheart
      | `Vendor of string ]

    include Jscontact_enum.Make (struct
      type nonrec t = t

      let kind = "relation"

      let to_string = function
        | `Acquaintance -> "acquaintance"
        | `Agent -> "agent"
        | `Child -> "child"
        | `Co_resident -> "co-resident"
        | `Co_worker -> "co-worker"
        | `Colleague -> "colleague"
        | `Contact -> "contact"
        | `Crush -> "crush"
        | `Date -> "date"
        | `Emergency -> "emergency"
        | `Friend -> "friend"
        | `Kin -> "kin"
        | `Me -> "me"
        | `Met -> "met"
        | `Muse -> "muse"
        | `Neighbor -> "neighbor"
        | `Parent -> "parent"
        | `Sibling -> "sibling"
        | `Spouse -> "spouse"
        | `Sweetheart -> "sweetheart"
        | `Vendor s -> s

      let of_string = function
        | "acquaintance" -> `Acquaintance
        | "agent" -> `Agent
        | "child" -> `Child
        | "co-resident" -> `Co_resident
        | "co-worker" -> `Co_worker
        | "colleague" -> `Colleague
        | "contact" -> `Contact
        | "crush" -> `Crush
        | "date" -> `Date
        | "emergency" -> `Emergency
        | "friend" -> `Friend
        | "kin" -> `Kin
        | "me" -> `Me
        | "met" -> `Met
        | "muse" -> `Muse
        | "neighbor" -> `Neighbor
        | "parent" -> `Parent
        | "sibling" -> `Sibling
        | "spouse" -> `Spouse
        | "sweetheart" -> `Sweetheart
        | s -> `Vendor s

      let is_vendor = function `Vendor _ -> true | _ -> false
    end)
  end

  type t = { relation : Kind.t list; unknown : Jscontact_unknown.t }

  let make ?(relation = []) ?(unknown = Jscontact_unknown.empty) () =
    { relation; unknown }

  let equal a b =
    Kind.equal_set a.relation b.relation
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf r =
    Format.fprintf ppf "@[%a@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Kind.pp)
      r.relation

  let kind = "Relation"

  let validate r =
    Jscontact_valid.(
      let* _ = Kind.validate_set r.relation in
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind r.unknown) in
      ok r)

  let ctor relation unknown = { relation; unknown }

  let jsont =
    Jsont.Object.map ~kind (fun () -> ctor)
    |> Jscontact_json.type_mem kind
    |> Jsont.Object.mem "relation" Kind.set_jsont ~dec_absent:(fun () -> [])
         ~enc_omit:(fun l -> l = [])
         ~enc:(fun t -> t.relation)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end

module Anniversary = struct
  module Kind = struct
    type t = [ `Birth | `Death | `Wedding | `Vendor of string ]

    include Jscontact_enum.Make (struct
      type nonrec t = t

      let kind = "kind"

      let to_string = function
        | `Birth -> "birth"
        | `Death -> "death"
        | `Wedding -> "wedding"
        | `Vendor s -> s

      let of_string = function
        | "birth" -> `Birth
        | "death" -> `Death
        | "wedding" -> `Wedding
        | s -> `Vendor s

      let is_vendor = function `Vendor _ -> true | _ -> false
    end)
  end

  type t = {
    kind : Kind.t;
    date : Jscontact_date.t;
    place : Jscontact_address.t option;
    unknown : Jscontact_unknown.t;
  }

  let make ?place ?(unknown = Jscontact_unknown.empty) kind date =
    { kind; date; place; unknown }

  let equal a b =
    Kind.equal a.kind b.kind
    && Jscontact_date.equal a.date b.date
    && Option.equal Jscontact_address.equal a.place b.place
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf a =
    Format.fprintf ppf "@[%a %a@]" Kind.pp a.kind Jscontact_date.pp a.date

  let kind = "Anniversary"

  let validate a =
    Jscontact_valid.(
      let* _ = Kind.validate a.kind in
      let* _ = in_ "date" (Jscontact_date.validate a.date) in
      let* _ = in_ "place" (opt Jscontact_address.validate a.place) in
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind a.unknown) in
      ok a)

  let ctor kind date place unknown = { kind; date; place; unknown }

  let jsont =
    Jsont.Object.map ~kind (fun () -> ctor)
    |> Jscontact_json.type_mem kind
    |> Jsont.Object.mem "kind" Kind.jsont ~enc:(fun t -> t.kind)
    |> Jsont.Object.mem "date" Jscontact_date.jsont ~enc:(fun t -> t.date)
    |> Jsont.Object.opt_mem "place" Jscontact_address.jsont ~enc:(fun t ->
        t.place)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end

module Note = struct
  module Author = struct
    type t = {
      name : string option;
      uri : string option;
      unknown : Jscontact_unknown.t;
    }

    let make ?name ?uri ?(unknown = Jscontact_unknown.empty) () =
      { name; uri; unknown }

    let equal a b =
      Option.equal String.equal a.name b.name
      && Option.equal String.equal a.uri b.uri
      && Jscontact_unknown.equal a.unknown b.unknown

    let pp ppf a =
      match (a.name, a.uri) with
      | Some name, _ -> Format.pp_print_string ppf name
      | None, Some uri -> Format.pp_print_string ppf uri
      | None, None -> Format.pp_print_string ppf "<author>"

    let kind = "Author"

    let validate a =
      Jscontact_valid.(
        let* () =
          check
            (Option.is_some a.name || Option.is_some a.uri
            || not (Jscontact_unknown.is_empty a.unknown))
            "at least one property other than @type must be set"
        in
        let* _ = opt (Jscontact_uri.validate ~prop:"uri") a.uri in
        let* _ =
          in_ kind (Jscontact_unknown.validate ~in_type:kind a.unknown)
        in
        ok a)

    let ctor name uri unknown = { name; uri; unknown }

    let jsont =
      Jsont.Object.map ~kind (fun () -> ctor)
      |> Jscontact_json.type_mem kind
      |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun t -> t.name)
      |> Jsont.Object.opt_mem "uri" Jsont.string ~enc:(fun t -> t.uri)
      |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
          t.unknown)
      |> Jsont.Object.finish
  end

  type t = {
    note : string;
    created : Jscontact_date.Utc.t option;
    author : Author.t option;
    unknown : Jscontact_unknown.t;
  }

  let make ?created ?author ?(unknown = Jscontact_unknown.empty) note =
    { note; created; author; unknown }

  let equal a b =
    String.equal a.note b.note
    && Option.equal Jscontact_date.Utc.equal a.created b.created
    && Option.equal Author.equal a.author b.author
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf n = Format.pp_print_string ppf n.note
  let kind = "Note"

  let validate n =
    Jscontact_valid.(
      let* _ = in_ "author" (opt Author.validate n.author) in
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind n.unknown) in
      ok n)

  let ctor note created author unknown = { note; created; author; unknown }

  let jsont =
    Jsont.Object.map ~kind (fun () -> ctor)
    |> Jscontact_json.type_mem kind
    |> Jsont.Object.mem "note" Jsont.string ~enc:(fun t -> t.note)
    |> Jsont.Object.opt_mem "created" Jscontact_date.Utc.jsont ~enc:(fun t ->
        t.created)
    |> Jsont.Object.opt_mem "author" Author.jsont ~enc:(fun t -> t.author)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end

module Personal_info = struct
  module Kind = struct
    type t = [ `Expertise | `Hobby | `Interest | `Vendor of string ]

    include Jscontact_enum.Make (struct
      type nonrec t = t

      let kind = "kind"

      let to_string = function
        | `Expertise -> "expertise"
        | `Hobby -> "hobby"
        | `Interest -> "interest"
        | `Vendor s -> s

      let of_string = function
        | "expertise" -> `Expertise
        | "hobby" -> `Hobby
        | "interest" -> `Interest
        | s -> `Vendor s

      let is_vendor = function `Vendor _ -> true | _ -> false
    end)
  end

  module Level = struct
    type t = [ `High | `Medium | `Low | `Vendor of string ]

    include Jscontact_enum.Make (struct
      type nonrec t = t

      let kind = "level"

      let to_string = function
        | `High -> "high"
        | `Medium -> "medium"
        | `Low -> "low"
        | `Vendor s -> s

      let of_string = function
        | "high" -> `High
        | "medium" -> `Medium
        | "low" -> `Low
        | s -> `Vendor s

      let is_vendor = function `Vendor _ -> true | _ -> false
    end)
  end

  type t = {
    kind : Kind.t;
    value : string;
    level : Level.t option;
    list_as : int option;
    label : string option;
    unknown : Jscontact_unknown.t;
  }

  let make ?level ?list_as ?label ?(unknown = Jscontact_unknown.empty) kind
      value =
    { kind; value; level; list_as; label; unknown }

  let equal a b =
    Kind.equal a.kind b.kind
    && String.equal a.value b.value
    && Option.equal Level.equal a.level b.level
    && Option.equal Int.equal a.list_as b.list_as
    && Option.equal String.equal a.label b.label
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf i = Format.fprintf ppf "@[%a %S@]" Kind.pp i.kind i.value
  let kind = "PersonalInfo"

  let validate i =
    Jscontact_valid.(
      let* _ = Kind.validate i.kind in
      let* _ = in_ "level" (opt Level.validate i.level) in
      let* () =
        match i.list_as with
        | None -> ok ()
        | Some n -> check (n > 0) "listAs: %d is not greater than zero" n
      in
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind i.unknown) in
      ok i)

  let ctor kind value level list_as label unknown =
    { kind; value; level; list_as; label; unknown }

  let jsont =
    Jsont.Object.map ~kind (fun () -> ctor)
    |> Jscontact_json.type_mem kind
    |> Jsont.Object.mem "kind" Kind.jsont ~enc:(fun t -> t.kind)
    |> Jsont.Object.mem "value" Jsont.string ~enc:(fun t -> t.value)
    |> Jsont.Object.opt_mem "level" Level.jsont ~enc:(fun t -> t.level)
    |> Jsont.Object.opt_mem "listAs" (Jscontact_json.unsigned ~kind:"listAs")
         ~enc:(fun t -> t.list_as)
    |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun t -> t.label)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end
