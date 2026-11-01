(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Resource = Jscontact_resource

module Kind = struct
  type t = [ `Calendar | `Free_busy | `Vendor of string ]

  include Jscontact_enum.Make (struct
    type nonrec t = t

    let kind = "kind"

    let to_string = function
      | `Calendar -> "calendar"
      | `Free_busy -> "freeBusy"
      | `Vendor s -> s

    let of_string = function
      | "calendar" -> `Calendar
      | "freeBusy" -> `Free_busy
      | s -> `Vendor s

    let is_vendor = function `Vendor _ -> true | _ -> false
  end)
end

type t = Kind.t Resource.base

let type_name = "Calendar"

let make ?uri ?media_type ?contexts ?pref ?label ?unknown kind =
  Resource.make ?uri ?media_type ?contexts ?pref ?label ?unknown kind

let equal a b = Resource.equal Kind.equal a b
let pp ppf c = Resource.pp Kind.pp ppf c
let validate c = Resource.validate ~type_name ~kind:Kind.validate c

let ctor kind uri media_type contexts pref label unknown =
  { Resource.kind; uri; media_type; contexts; pref; label; unknown }

let jsont =
  Jsont.Object.map ~kind:type_name (fun () -> ctor)
  |> Jscontact_json.type_mem type_name
  |> Jsont.Object.mem "kind" Kind.jsont ~enc:(fun (t : t) -> t.Resource.kind)
  |> Resource.mems ~base:Fun.id
  |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun (t : t) ->
      t.Resource.unknown)
  |> Jsont.Object.finish

module Scheduling_address = struct
  type t = {
    uri : string;
    contexts : Jscontact_context.t list option;
    pref : int option;
    label : string option;
    unknown : Jscontact_unknown.t;
  }

  let type_name = "SchedulingAddress"

  let make ?contexts ?pref ?label ?(unknown = Jscontact_unknown.empty) uri =
    { uri; contexts; pref; label; unknown }

  let equal a b =
    String.equal a.uri b.uri
    && Option.equal Jscontact_context.equal_set a.contexts b.contexts
    && Option.equal Int.equal a.pref b.pref
    && Option.equal String.equal a.label b.label
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf a = Format.pp_print_string ppf a.uri

  let validate a =
    Jscontact_valid.(
      let* _ = Jscontact_uri.validate ~prop:"uri" a.uri in
      let* _ = opt Jscontact_context.validate_set a.contexts in
      let* _ = opt Jscontact_pref.validate a.pref in
      let* _ =
        in_ type_name (Jscontact_unknown.validate ~in_type:type_name a.unknown)
      in
      ok a)

  let ctor uri contexts pref label unknown =
    { uri; contexts; pref; label; unknown }

  let jsont =
    Jsont.Object.map ~kind:type_name (fun () -> ctor)
    |> Jscontact_json.type_mem type_name
    |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun t -> t.uri)
    |> Jsont.Object.opt_mem "contexts" Jscontact_context.set_jsont
         ~enc:(fun t -> t.contexts)
    |> Jsont.Object.opt_mem "pref" Jscontact_pref.jsont ~enc:(fun t -> t.pref)
    |> Jsont.Object.opt_mem "label" Jsont.string ~enc:(fun t -> t.label)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)
    |> Jsont.Object.finish
end
