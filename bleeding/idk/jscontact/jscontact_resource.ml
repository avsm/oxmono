(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'kind base = {
  kind : 'kind;
  uri : string option;
  media_type : string option;
  contexts : Jscontact_context.t list option;
  pref : int option;
  label : string option;
  unknown : Jscontact_unknown.t;
}

let make ?uri ?media_type ?contexts ?pref ?label
    ?(unknown = Jscontact_unknown.empty) kind =
  { kind; uri; media_type; contexts; pref; label; unknown }

let equal eq a b =
  eq a.kind b.kind
  && Option.equal String.equal a.uri b.uri
  && Option.equal String.equal a.media_type b.media_type
  && Option.equal Jscontact_context.equal_set a.contexts b.contexts
  && Option.equal Int.equal a.pref b.pref
  && Option.equal String.equal a.label b.label
  && Jscontact_unknown.equal a.unknown b.unknown

let pp pp_kind ppf r =
  Format.fprintf ppf "@[%a %s@]" pp_kind r.kind
    (Option.value r.uri ~default:"<no uri>")

let validate ~type_name ~kind r =
  Jscontact_valid.(
    let* _ = kind r.kind in
    let* _ =
      match r.uri with
      | Some uri -> Jscontact_uri.validate ~prop:"uri" uri
      | None ->
          Jscontact_valid.error "uri: is mandatory in a %s, per Section 1.4.4%s"
            type_name
            (if String.equal type_name "Media" then
               ", and a Media that carries a JMAP blobId in its place is not a \
                valid JSContact resource"
             else "")
    in
    let* _ = opt Jscontact_context.validate_set r.contexts in
    let* _ = opt Jscontact_pref.validate r.pref in
    let* _ =
      in_ type_name (Jscontact_unknown.validate ~in_type:type_name r.unknown)
    in
    ok r)

let mems ~base map =
  map
  |> Jsont.Object.opt_mem "uri" Jsont.string ~enc:(fun o -> (base o).uri)
  |> Jsont.Object.opt_mem "mediaType" Jsont.string ~enc:(fun o ->
      (base o).media_type)
  |> Jsont.Object.opt_mem "contexts" Jscontact_context.set_jsont ~enc:(fun o ->
      (base o).contexts)
  |> Jsont.Object.opt_mem "pref" Jscontact_pref.jsont ~enc:(fun o ->
      (base o).pref)
  |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun o -> (base o).label)

module Crypto_key = struct
  type t = string option base

  let type_name = "CryptoKey"

  let make ?kind ?uri ?media_type ?contexts ?pref ?label ?unknown () =
    make ?uri ?media_type ?contexts ?pref ?label ?unknown kind

  let equal a b = equal (Option.equal String.equal) a b

  let pp_kind ppf = function
    | None -> Format.pp_print_string ppf "-"
    | Some s -> Format.pp_print_string ppf s

  let pp ppf k = pp pp_kind ppf k
  let validate k = validate ~type_name ~kind:Jscontact_valid.ok k

  let ctor kind uri media_type contexts pref label unknown =
    { kind; uri; media_type; contexts; pref; label; unknown }

  let jsont =
    Jsont.Object.map ~kind:type_name (fun () -> ctor)
    |> Jscontact_json.type_mem type_name
    |> Jsont.Object.opt_mem "kind" Jsont.string ~enc:(fun t -> t.kind)
    |> mems ~base:Fun.id
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end

module Directory = struct
  module Kind = struct
    type t = [ `Directory | `Entry | `Vendor of string ]

    include Jscontact_enum.Make (struct
      type nonrec t = t

      let kind = "kind"

      let to_string = function
        | `Directory -> "directory"
        | `Entry -> "entry"
        | `Vendor s -> s

      let of_string = function
        | "directory" -> `Directory
        | "entry" -> `Entry
        | s -> `Vendor s

      let is_vendor = function `Vendor _ -> true | _ -> false
    end)
  end

  type t = {
    kind : Kind.t;
    uri : string option;
    media_type : string option;
    contexts : Jscontact_context.t list option;
    pref : int option;
    label : string option;
    list_as : int option;
    unknown : Jscontact_unknown.t;
  }

  let type_name = "Directory"

  let ctor kind uri media_type contexts pref label list_as unknown =
    { kind; uri; media_type; contexts; pref; label; list_as; unknown }

  let make ?uri ?media_type ?contexts ?pref ?label ?list_as
      ?(unknown = Jscontact_unknown.empty) kind =
    ctor kind uri media_type contexts pref label list_as unknown

  (* The Resource properties of a directory, so that the codec and the
     validation of Section 1.4.4 apply to it unchanged. Both records are named
     in full, so adding a property to either is a type error here rather than a
     property this projection silently drops. *)
  let to_base
      ({ kind; uri; media_type; contexts; pref; label; list_as = _; unknown } :
        t) : Kind.t base =
    { kind; uri; media_type; contexts; pref; label; unknown }

  let equal a b =
    equal Kind.equal (to_base a) (to_base b)
    && Option.equal Int.equal a.list_as b.list_as

  let pp ppf d = pp Kind.pp ppf (to_base d)

  let validate d =
    Jscontact_valid.(
      let* _ = validate ~type_name ~kind:Kind.validate (to_base d) in
      let* () =
        match d.list_as with
        | None -> ok ()
        | Some n -> check (n > 0) "listAs: %d is not higher than zero" n
      in
      ok d)

  let jsont =
    Jsont.Object.map ~kind:type_name (fun () -> ctor)
    |> Jscontact_json.type_mem type_name
    |> Jsont.Object.mem "kind" Kind.jsont ~enc:(fun t -> t.kind)
    |> mems ~base:to_base
    |> Jsont.Object.opt_mem "listAs" (Jscontact_json.unsigned ~kind:"listAs")
         ~enc:(fun t -> t.list_as)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end

module Link = struct
  module Kind = struct
    type t = [ `Contact | `Vendor of string ]

    include Jscontact_enum.Make (struct
      type nonrec t = t

      let kind = "kind"
      let to_string = function `Contact -> "contact" | `Vendor s -> s
      let of_string = function "contact" -> `Contact | s -> `Vendor s
      let is_vendor = function `Vendor _ -> true | _ -> false
    end)
  end

  type t = Kind.t option base

  let type_name = "Link"

  let make ?kind ?uri ?media_type ?contexts ?pref ?label ?unknown () =
    make ?uri ?media_type ?contexts ?pref ?label ?unknown kind

  let equal a b = equal (Option.equal Kind.equal) a b

  let pp_kind ppf = function
    | None -> Format.pp_print_string ppf "-"
    | Some k -> Kind.pp ppf k

  let pp ppf l = pp pp_kind ppf l

  let validate l =
    validate ~type_name ~kind:(Jscontact_valid.opt Kind.validate) l

  let ctor kind uri media_type contexts pref label unknown =
    { kind; uri; media_type; contexts; pref; label; unknown }

  let jsont =
    Jsont.Object.map ~kind:type_name (fun () -> ctor)
    |> Jscontact_json.type_mem type_name
    |> Jsont.Object.opt_mem "kind" Kind.jsont ~enc:(fun t -> t.kind)
    |> mems ~base:Fun.id
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end

module Media = struct
  module Kind = struct
    type t = [ `Photo | `Sound | `Logo | `Vendor of string ]

    include Jscontact_enum.Make (struct
      type nonrec t = t

      let kind = "kind"

      let to_string = function
        | `Photo -> "photo"
        | `Sound -> "sound"
        | `Logo -> "logo"
        | `Vendor s -> s

      let of_string = function
        | "photo" -> `Photo
        | "sound" -> `Sound
        | "logo" -> `Logo
        | s -> `Vendor s

      let is_vendor = function `Vendor _ -> true | _ -> false
    end)
  end

  type t = Kind.t base

  let type_name = "Media"

  let make ?uri ?media_type ?contexts ?pref ?label ?unknown kind =
    make ?uri ?media_type ?contexts ?pref ?label ?unknown kind

  let equal a b = equal Kind.equal a b
  let pp ppf m = pp Kind.pp ppf m
  let validate m = validate ~type_name ~kind:Kind.validate m

  let ctor kind uri media_type contexts pref label unknown =
    { kind; uri; media_type; contexts; pref; label; unknown }

  let jsont =
    Jsont.Object.map ~kind:type_name (fun () -> ctor)
    |> Jscontact_json.type_mem type_name
    |> Jsont.Object.mem "kind" Kind.jsont ~enc:(fun t -> t.kind)
    |> mems ~base:Fun.id
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end
