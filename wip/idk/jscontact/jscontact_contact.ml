(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* RFC 5322 Section 3.4.1: addr-spec = local-part "@" domain. The check is
   loose: it asks for a non-empty local part and domain either side of the last
   commercial at, and for no whitespace or control character throughout. *)
let is_addr_spec s =
  match String.rindex_opt s '@' with
  | None | Some 0 -> false
  | Some i ->
      i < String.length s - 1 && String.for_all Jscontact_ascii.is_graphic s

module Email_address = struct
  type t = {
    address : string;
    contexts : Jscontact_context.t list option;
    pref : int option;
    label : string option;
    unknown : Jscontact_unknown.t;
  }

  let make ?contexts ?pref ?label ?(unknown = Jscontact_unknown.empty) address =
    { address; contexts; pref; label; unknown }

  let equal a b =
    let contexts =
      Option.equal Jscontact_context.equal_set a.contexts b.contexts
    in
    String.equal a.address b.address
    && contexts && a.pref = b.pref && a.label = b.label
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf t = Format.pp_print_string ppf t.address
  let kind = "EmailAddress"

  let validate t =
    Jscontact_valid.(
      let* _ =
        check (is_addr_spec t.address)
          "address: %S is not an addr-spec, which is a local part, a \
           commercial at and a domain"
          t.address
      in
      let* _ = opt Jscontact_context.validate_set t.contexts in
      let* _ = opt Jscontact_pref.validate t.pref in
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind t.unknown) in
      ok t)

  let ctor address contexts pref label unknown =
    { address; contexts; pref; label; unknown }

  let jsont =
    Jsont.Object.map ~kind (fun () -> ctor)
    |> Jscontact_json.type_mem kind
    |> Jsont.Object.mem "address" Jsont.string ~enc:(fun t -> t.address)
    |> Jsont.Object.opt_mem "contexts" Jscontact_context.set_jsont
         ~enc:(fun t -> t.contexts)
    |> Jsont.Object.opt_mem "pref" Jscontact_pref.jsont ~enc:(fun t -> t.pref)
    |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun t -> t.label)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end

module Online_service = struct
  type t = {
    service : string option;
    uri : string option;
    user : string option;
    contexts : Jscontact_context.t list option;
    pref : int option;
    label : string option;
    unknown : Jscontact_unknown.t;
  }

  let make ?service ?uri ?user ?contexts ?pref ?label
      ?(unknown = Jscontact_unknown.empty) () =
    { service; uri; user; contexts; pref; label; unknown }

  let equal a b =
    let contexts =
      Option.equal Jscontact_context.equal_set a.contexts b.contexts
    in
    a.service = b.service && a.uri = b.uri && a.user = b.user && contexts
    && a.pref = b.pref && a.label = b.label
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf t =
    match (t.uri, t.user) with
    | Some uri, _ -> Format.pp_print_string ppf uri
    | None, Some user -> Format.pp_print_string ppf user
    | None, None -> Format.pp_print_string ppf ""

  let kind = "OnlineService"

  let validate t =
    Jscontact_valid.(
      let* () =
        check
          (t.uri <> None || t.user <> None)
          "at least one of the uri and user properties must be set"
      in
      let* _ = opt (Jscontact_uri.validate ~prop:"uri") t.uri in
      let* _ = opt Jscontact_context.validate_set t.contexts in
      let* _ = opt Jscontact_pref.validate t.pref in
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind t.unknown) in
      ok t)

  let ctor service uri user contexts pref label unknown =
    { service; uri; user; contexts; pref; label; unknown }

  let jsont =
    Jsont.Object.map ~kind (fun () -> ctor)
    |> Jscontact_json.type_mem kind
    |> Jsont.Object.opt_mem "service" Jsont.string ~enc:(fun t -> t.service)
    |> Jsont.Object.opt_mem "uri" Jsont.string ~enc:(fun t -> t.uri)
    |> Jsont.Object.opt_mem "user" Jsont.string ~enc:(fun t -> t.user)
    |> Jsont.Object.opt_mem "contexts" Jscontact_context.set_jsont
         ~enc:(fun t -> t.contexts)
    |> Jsont.Object.opt_mem "pref" Jscontact_pref.jsont ~enc:(fun t -> t.pref)
    |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun t -> t.label)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end

module Phone = struct
  module Feature = struct
    type t =
      [ `Mobile
      | `Voice
      | `Text
      | `Video
      | `Main_number
      | `Textphone
      | `Fax
      | `Pager
      | `Vendor of string ]

    include Jscontact_enum.Make (struct
      type nonrec t = t

      let kind = "features"

      let to_string = function
        | `Mobile -> "mobile"
        | `Voice -> "voice"
        | `Text -> "text"
        | `Video -> "video"
        | `Main_number -> "main-number"
        | `Textphone -> "textphone"
        | `Fax -> "fax"
        | `Pager -> "pager"
        | `Vendor s -> s

      let of_string = function
        | "mobile" -> `Mobile
        | "voice" -> `Voice
        | "text" -> `Text
        | "video" -> `Video
        | "main-number" -> `Main_number
        | "textphone" -> `Textphone
        | "fax" -> `Fax
        | "pager" -> `Pager
        | s -> `Vendor s

      let is_vendor = function `Vendor _ -> true | _ -> false
    end)
  end

  type t = {
    number : string;
    features : Feature.t list option;
    contexts : Jscontact_context.t list option;
    pref : int option;
    label : string option;
    unknown : Jscontact_unknown.t;
  }

  let make ?features ?contexts ?pref ?label ?(unknown = Jscontact_unknown.empty)
      number =
    { number; features; contexts; pref; label; unknown }

  let equal a b =
    let features = Option.equal Feature.equal_set a.features b.features in
    let contexts =
      Option.equal Jscontact_context.equal_set a.contexts b.contexts
    in
    String.equal a.number b.number
    && features && contexts && a.pref = b.pref && a.label = b.label
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf t = Format.pp_print_string ppf t.number
  let kind = "Phone"

  let validate t =
    Jscontact_valid.(
      let* _ = opt (list Feature.validate) t.features in
      let* _ = opt Jscontact_context.validate_set t.contexts in
      let* _ = opt Jscontact_pref.validate t.pref in
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind t.unknown) in
      ok t)

  let ctor number features contexts pref label unknown =
    { number; features; contexts; pref; label; unknown }

  let jsont =
    Jsont.Object.map ~kind (fun () -> ctor)
    |> Jscontact_json.type_mem kind
    |> Jsont.Object.mem "number" Jsont.string ~enc:(fun t -> t.number)
    |> Jsont.Object.opt_mem "features" Feature.set_jsont ~enc:(fun t ->
        t.features)
    |> Jsont.Object.opt_mem "contexts" Jscontact_context.set_jsont
         ~enc:(fun t -> t.contexts)
    |> Jsont.Object.opt_mem "pref" Jscontact_pref.jsont ~enc:(fun t -> t.pref)
    |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun t -> t.label)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end

module Language_pref = struct
  type t = {
    language : string;
    contexts : Jscontact_context.t list option;
    pref : int option;
    unknown : Jscontact_unknown.t;
  }

  let make ?contexts ?pref ?(unknown = Jscontact_unknown.empty) language =
    { language; contexts; pref; unknown }

  let equal a b =
    let contexts =
      Option.equal Jscontact_context.equal_set a.contexts b.contexts
    in
    String.equal a.language b.language
    && contexts && a.pref = b.pref
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf t = Format.pp_print_string ppf t.language
  let kind = "LanguagePref"

  let validate t =
    Jscontact_valid.(
      let* _ = in_ "language" (Jscontact_language.validate t.language) in
      let* _ = opt Jscontact_context.validate_set t.contexts in
      let* _ = opt Jscontact_pref.validate t.pref in
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind t.unknown) in
      ok t)

  let ctor language contexts pref unknown =
    { language; contexts; pref; unknown }

  let jsont =
    Jsont.Object.map ~kind (fun () -> ctor)
    |> Jscontact_json.type_mem kind
    |> Jsont.Object.mem "language" Jsont.string ~enc:(fun t -> t.language)
    |> Jsont.Object.opt_mem "contexts" Jscontact_context.set_jsont
         ~enc:(fun t -> t.contexts)
    |> Jsont.Object.opt_mem "pref" Jscontact_pref.jsont ~enc:(fun t -> t.pref)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end
