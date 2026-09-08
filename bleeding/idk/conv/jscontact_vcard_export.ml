(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Jscontact
module P = Vcard.Property
module Jcard = Jscontact_vcard_jcard

let ( let* ) = Result.bind

type t = {
  mutable props : P.t list;
  targets : (string * string) list;
  mutable groups : int;
  org_groups : (string * string) list;
  taken : (string, unit) Hashtbl.t;
  mutable error : string option;
}

let emit t p = t.props <- p :: t.props

(* An enumerated value carries any spelling in from the wire but only a well
   formed one back out, so encoding a JSPROP can fail. The conversion is a
   result, so the failure is kept here and returned by [convert_card] rather
   than raised. *)
let encoded t codec v =
  match Jsont.Json.encode codec v with
  | Ok j -> Some j
  | Error msg ->
      if t.error = None then t.error <- Some msg;
      None

let param name values = Vcard.Param.v name values

(* A phonetic variant shares the ALTID of the property it pronounces, RFC
   9554 Section 4.6. *)
let base_target target =
  match String.index_opt target '#' with
  | Some i -> String.sub target 0 i
  | None -> target

let altid t target =
  Option.map
    (fun a -> param "ALTID" [ a ])
    (List.assoc_opt (base_target target) t.targets)

(* The vCardParams of an object, RFC 9555 Section 2.15.2, and the group among
   them, Section 2.3.9. *)
let saved_params (u : Unknown.t) =
  match Unknown.find u "vCardParams" with
  | Some (Jsont.Object (mems, _)) ->
      List.fold_left
        (fun (group, params) ((k, _), v) ->
          match (k, v) with
          | "group", Jsont.String (g, _) -> (Some g, params)
          | _, Jsont.String (s, _) -> (group, params @ [ param k [ s ] ])
          | _, Jsont.Array (vs, _) ->
              let vs =
                List.filter_map
                  (function Jsont.String (s, _) -> Some s | _ -> None)
                  vs
              in
              (group, params @ [ param k vs ])
          | _ -> (group, params))
        (None, []) mems
  | _ -> (None, [])

let saved_name (u : Unknown.t) =
  match Unknown.find u "vCardName" with
  | Some (Jsont.String (s, _)) -> Some s
  | _ -> None

let context_type = function
  | `Private -> "home"
  | `Work -> "work"
  | `Billing -> "billing"
  | `Delivery -> "delivery"
  | `Vendor s -> s

let opt name = function Some v -> [ param name [ v ] ] | None -> []

let opt_int name = function
  | Some v -> [ param name [ string_of_int v ] ]
  | None -> []

let contexts = function
  | Some (_ :: _ as cs) -> [ param "TYPE" (List.map context_type cs) ]
  | _ -> []

let rec new_group t =
  t.groups <- t.groups + 1;
  let g = Printf.sprintf "item%d" t.groups in
  if Hashtbl.mem t.taken (String.lowercase_ascii g) then new_group t else g

(* Every property built from a map entry carries its Id as PROP-ID, RFC 9555
   Section 3.1, its saved parameters and its group. A label becomes an
   X-ABLABEL in the same group, Section 2.11.11. *)
let property t ?id ?language ?target ?label ?(params = []) ~unknown name value =
  let group, saved = saved_params unknown in
  let group =
    match (group, label) with None, Some _ -> Some (new_group t) | g, _ -> g
  in
  let params =
    opt "PROP-ID" (Option.map Id.to_string id)
    @ params @ saved
    @ Option.to_list (Option.bind target (altid t))
    @ opt "LANGUAGE" language
  in
  emit t (P.v ?group ~params name value);
  Option.iter (fun l -> emit t (P.of_text ?group "X-ABLABEL" l)) label

let text s = Vcard.Text.escape s

let utc_string (u : Date.Utc.t) =
  Vcard.Date.Timestamp.to_string (Vcard.Date.Timestamp.of_ptime u)

(* RFC 9555 Section 2.2.2 reversed. *)
let date_string = function
  | Date.Timestamp ts -> utc_string ts.utc
  | Date.Partial d ->
      Vcard.Date.Cal_date.to_string
        { Vcard.Date.Cal_date.year = d.year; month = d.month; day = d.day }

let json_string v =
  match Jsont_bytesrw.encode_string Jsont.json v with
  | Ok s -> s
  | Error _ -> "null"

(* RFC 6901 Section 3: "~" is "~0" and "/" is "~1" in a reference token. *)
let escape_token s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '~' -> Buffer.add_string b "~0"
      | '/' -> Buffer.add_string b "~1"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* RFC 9555 Section 3.2.1: a JSContact property with no vCard counterpart is
   a JSPROP whose JSPTR points at it. *)
let jsprop t pointer value =
  emit t
    (P.v
       ~params:[ param "JSPTR" [ pointer ]; param "VALUE" [ "text" ] ]
       "JSPROP"
       (text (json_string value)))

let unknown_members t prefix (u : Unknown.t) =
  List.iter
    (fun (name, v) ->
      if not (List.mem name [ "vCardProps"; "vCardParams"; "vCardName" ]) then
        jsprop t (prefix ^ escape_token name) v)
    (Unknown.to_list u)

let map_prefix map id = Printf.sprintf "%s/%s/" map (Id.to_string id)

(* Builds a structured value from components, recording for each component
   its position for JSCOMPS. [slots] maps a kind to its component index and
   to any second index that mirrors it for older readers. *)
let structured ~slots ~size components =
  let cells = Array.make size [] in
  let entries =
    List.map
      (fun (kind, value) ->
        match slots kind with
        | None -> `Separator value
        | Some (i, mirror) ->
            let j = List.length cells.(i) in
            cells.(i) <- cells.(i) @ [ value ];
            Option.iter (fun m -> cells.(m) <- cells.(m) @ [ value ]) mirror;
            `Position (i, j))
      components
  in
  (Array.to_list cells, entries)

let jscomps ~default_separator entries =
  let entries =
    List.map
      (function
        | `Separator s -> Vcard.Jscomps.Separator s
        | `Position (i, j) -> Vcard.Jscomps.Position (i, j))
      entries
  in
  param "JSCOMPS"
    [ Vcard.Jscomps.to_string { Vcard.Jscomps.default_separator; entries } ]

(* RFC 9555 Section 2.5.5, Table 1, reversed. A surname2 also goes to the
   family names and a generation to the honorific suffixes. *)
let n_slot = function
  | `Surname -> Some (0, None)
  | `Given -> Some (1, None)
  | `Given2 -> Some (2, None)
  | `Title -> Some (3, None)
  | `Credential -> Some (4, None)
  | `Surname2 -> Some (5, Some 0)
  | `Generation -> Some (6, Some 4)
  | `Separator | `Vendor _ -> None

let n_index k = match n_slot k with Some (i, _) -> i | None -> 7

(* RFC 9555 Section 2.6.1, Table 2, reversed. The street address, extended
   address and post office box components are filled for older readers. *)
let adr_slot = function
  | `Post_office_box -> Some (0, None)
  | `Room -> Some (7, Some 1)
  | `Apartment -> Some (8, Some 1)
  | `Floor -> Some (9, Some 1)
  | `Building -> Some (12, Some 1)
  | `Number -> Some (10, Some 2)
  | `Name -> Some (11, Some 2)
  | `Block -> Some (13, Some 2)
  | `Direction -> Some (17, Some 2)
  | `Landmark -> Some (16, Some 2)
  | `Subdistrict -> Some (14, Some 2)
  | `District -> Some (15, Some 2)
  | `Locality -> Some (3, None)
  | `Region -> Some (4, None)
  | `Postcode -> Some (5, None)
  | `Country -> Some (6, None)
  | `Separator | `Vendor _ -> None

let n_structured (n : Name.t) ~phonetic =
  let components = Option.value ~default:[] n.components in
  let value (c : Name.Component.t) =
    if phonetic then Option.value ~default:"" c.phonetic else c.value
  in
  structured ~slots:n_slot ~size:7
    (List.map (fun (c : Name.Component.t) -> (c.kind, value c)) components)

let phonetic_params ~system ~script =
  opt "PHONETIC"
    (Some
       (match system with Some s -> Phonetic.to_string s | None -> "script"))
  @ opt "SCRIPT" script

let name_params (n : Name.t) ~phonetic =
  let cells, entries = n_structured n ~phonetic in
  let params =
    (if phonetic then
       phonetic_params ~system:n.phonetic_system ~script:n.phonetic_script
     else [])
    @ (if n.is_ordered then
         [ jscomps ~default_separator:n.default_separator entries ]
       else [])
    @
    match n.sort_as with
    | Some (_ :: _ as sort) when not phonetic ->
        (* RFC 9554 Section 4.5 extends SORT-AS to the seven N components, so a
           separator or vendor kind has no cell and is left out. *)
        let sort = List.filter (fun (k, _) -> n_slot k <> None) sort in
        let hi = List.fold_left (fun m (k, _) -> max m (n_index k)) 0 sort in
        let cells =
          List.init (hi + 1) (fun i ->
              match List.find_opt (fun (k, _) -> n_index k = i) sort with
              | Some (_, v) -> v
              | None -> "")
        in
        if sort = [] then [] else [ param "SORT-AS" cells ]
    | _ -> []
  in
  (Vcard.Text.structured_to_string cells, params)

let has_phonetics (n : Name.t) =
  n.phonetic_system <> None || n.phonetic_script <> None
  || List.exists
       (fun (c : Name.Component.t) -> c.phonetic <> None)
       (Option.value ~default:[] n.components)

let emit_n t ?language ~targets (n : Name.t) =
  let has tg = List.mem tg targets in
  (match n.components with
  | Some _ when has "N" ->
      let value, params = name_params n ~phonetic:false in
      property t ?language ~target:"N" ~params ~unknown:n.unknown "N" value
  | _ -> ());
  if has "N#phonetic" && has_phonetics n && n.components <> None then
    let value, params = name_params n ~phonetic:true in
    property t ?language ~target:"N#phonetic" ~params ~unknown:Unknown.empty "N"
      value

let full_name (n : Name.t) =
  match n.full with
  | Some f -> Some f
  | None -> (
      match n.components with
      | None -> None
      | Some cs ->
          let sep = Option.value ~default:" " n.default_separator in
          let rec join = function
            | [] -> ""
            | [ (c : Name.Component.t) ] -> c.value
            | (c : Name.Component.t) :: ((d : Name.Component.t) :: _ as rest) ->
                let s =
                  if c.kind = `Separator || d.kind = `Separator then "" else sep
                in
                c.value ^ s ^ join rest
          in
          Some (join cs))

(* RFC 9555 Section 3.1: FN is the full name, or is derived and marked, or
   is empty. *)
let emit_fn t ?language (n : Name.t option) =
  match n with
  | Some { full = Some f; _ } ->
      (* The unknown members of a Name came from N, so they stay on N. *)
      property t ?language ~target:"FN" ~unknown:Unknown.empty "FN" (text f)
  | n -> (
      match Option.bind n full_name with
      | Some full ->
          property t ?language ~target:"FN"
            ~params:[ param "DERIVED" [ "TRUE" ] ]
            ~unknown:Unknown.empty "FN" (text full)
      | None ->
          (* Section 3.1: with nothing to derive from, FN is empty and is not
             marked derived. *)
          property t ?language ~target:"FN" ~unknown:Unknown.empty "FN" "")

let offset_of_zone z =
  match Scanf.sscanf_opt z "Etc/GMT%d" (fun h -> h) with
  | Some h -> Some (Vcard.Date.Utc_offset.to_string (-h * 60))
  | None -> if z = "Etc/UTC" then Some "+0000" else None

let emit_adr t ?language ~targets id (a : Address.t) =
  let target = "addresses/" ^ Id.to_string id in
  let has tg = List.mem tg targets in
  let components = Option.value ~default:[] a.components in
  let kinds =
    List.map (fun (c : Address.Component.t) -> (c.kind, c.value)) components
  in
  let cells, entries = structured ~slots:adr_slot ~size:18 kinds in
  let empty =
    components = [] && a.full = None && a.coordinates = None
    && a.time_zone = None && a.country_code = None
  in
  match saved_name a.unknown with
  | Some (("geo" | "tz") as name) when components = [] ->
      (* A stand-alone GEO or TZ property, RFC 9555 Section 2.8. *)
      let value, value_param =
        if name = "geo" then (Option.value ~default:"" a.coordinates, [])
        else
          match Option.bind a.time_zone offset_of_zone with
          | Some o -> (o, [ param "VALUE" [ "utc-offset" ] ])
          | None -> (Option.value ~default:"" a.time_zone, [])
      in
      property t ~id ?language ~target
        ~params:(contexts a.contexts @ opt_int "PREF" a.pref @ value_param)
        ~unknown:a.unknown
        (String.uppercase_ascii name)
        value
  | _ when empty -> ()
  | _ ->
      (if has target then
         let tz =
           opt "TZ"
             (Option.map
                (fun z -> Option.value (offset_of_zone z) ~default:z)
                a.time_zone)
         in
         let params =
           opt "LABEL" (Option.map Vcard.Text.escape a.full)
           @ opt "GEO" a.coordinates @ tz @ opt "CC" a.country_code
           @ contexts a.contexts @ opt_int "PREF" a.pref
           @
           if a.is_ordered then
             [ jscomps ~default_separator:a.default_separator entries ]
           else []
         in
         property t ~id ?language ~target ~params ~unknown:a.unknown "ADR"
           (Vcard.Text.structured_to_string cells));
      if
        has (target ^ "#phonetic")
        && List.exists
             (fun (c : Address.Component.t) -> c.phonetic <> None)
             components
      then
        let kinds =
          List.map
            (fun (c : Address.Component.t) ->
              (c.kind, Option.value ~default:"" c.phonetic))
            components
        in
        let cells, _ = structured ~slots:adr_slot ~size:18 kinds in
        property t ~id ?language ~target:(target ^ "#phonetic")
          ~params:
            (phonetic_params ~system:a.phonetic_system ~script:a.phonetic_script)
          ~unknown:Unknown.empty "ADR"
          (Vcard.Text.structured_to_string cells)

let feature_type = function
  | `Mobile -> "cell"
  | `Voice -> "voice"
  | `Text -> "text"
  | `Video -> "video"
  | `Main_number -> "main-number"
  | `Textphone -> "textphone"
  | `Fax -> "fax"
  | `Pager -> "pager"
  | `Vendor s -> s

let emit_resource t ?language ~name id (r : 'k Resource.base) =
  let params =
    opt "MEDIATYPE" r.media_type @ contexts r.contexts @ opt_int "PREF" r.pref
  in
  property t ~id ?language ~params ?label:r.label ~unknown:r.unknown name
    (Option.value ~default:"" r.uri)

let with_group (u : Unknown.t) group =
  match (group, fst (saved_params u)) with
  | Some g, None ->
      Unknown.add u "vCardParams"
        (Jsont.Json.object'
           [ (("group", Jsont.Meta.none), Jsont.Json.string g) ])
  | _ -> u

let emit_title t ?language id (ti : Org.Title.t) =
  let group =
    Option.bind ti.organization_id (fun oid ->
        List.assoc_opt (Id.to_string oid) t.org_groups)
  in
  let name = match ti.kind with `Role -> "ROLE" | _ -> "TITLE" in
  property t ~id ?language
    ~target:("titles/" ^ Id.to_string id)
    ~unknown:(with_group ti.unknown group)
    name (text ti.name)

let emit_org t ?language id (o : Org.Organization.t) =
  let units = Option.value ~default:[] o.units in
  let cells =
    [ Option.value ~default:"" o.name ]
    @ List.map (fun (u : Org.Organization.Org_unit.t) -> u.name) units
  in
  let sort =
    o.sort_as
    :: List.map (fun (u : Org.Organization.Org_unit.t) -> u.sort_as) units
  in
  let rec trim = function
    | [] -> []
    | None :: rest -> ( match trim rest with [] -> [] | l -> None :: l)
    | Some v :: rest -> Some v :: trim rest
  in
  let params =
    (match trim sort with
      | [] -> []
      | sort -> [ param "SORT-AS" (List.map (Option.value ~default:"") sort) ])
    @ contexts o.contexts
  in
  property t ~id ?language
    ~target:("organizations/" ^ Id.to_string id)
    ~params
    ~unknown:
      (with_group o.unknown (List.assoc_opt (Id.to_string id) t.org_groups))
    "ORG"
    (Vcard.Text.structured_to_string (List.map (fun c -> [ c ]) cells))

let emit_note t ?language id (n : Info.Note.t) =
  let params =
    opt "AUTHOR" (Option.bind n.author (fun a -> a.uri))
    @ opt "AUTHOR-NAME" (Option.bind n.author (fun a -> a.name))
    @ opt "CREATED" (Option.map utc_string n.created)
  in
  property t ~id ?language
    ~target:("notes/" ^ Id.to_string id)
    ~params ~unknown:n.unknown "NOTE" (text n.note)

let emit_nickname t ?language id (n : Name.Nickname.t) =
  property t ~id ?language
    ~target:("nicknames/" ^ Id.to_string id)
    ~params:(contexts n.contexts @ opt_int "PREF" n.pref)
    ~unknown:n.unknown "NICKNAME" (text n.name)

let emit_pronouns t ?language id (p : Org.Speak_to_as.Pronouns.t) =
  property t ~id ?language
    ~target:("speakToAs/pronouns/" ^ Id.to_string id)
    ~params:(contexts p.contexts @ opt_int "PREF" p.pref)
    ~unknown:p.unknown "PRONOUNS" (text p.pronouns)

let emit_gender t ?language ~unknown g =
  property t ?language ~target:"speakToAs/grammaticalGender" ~unknown
    "GRAMGENDER"
    (Org.Speak_to_as.Grammatical_gender.to_string g)

let level_param kind (l : Info.Personal_info.Level.t) =
  let s = Info.Personal_info.Level.to_string l in
  if kind = `Expertise then
    match s with
    | "low" -> "beginner"
    | "medium" -> "average"
    | "high" -> "expert"
    | s -> s
  else s

let emit_personal t ?language id (i : Info.Personal_info.t) =
  let name =
    match i.kind with
    | `Expertise -> "EXPERTISE"
    | `Hobby -> "HOBBY"
    | `Interest -> "INTEREST"
    | `Vendor s -> String.uppercase_ascii s
  in
  let params =
    opt "LEVEL" (Option.map (level_param i.kind) i.level)
    @ opt_int "INDEX" i.list_as
  in
  property t ~id ?language
    ~target:("personalInfo/" ^ Id.to_string id)
    ~params ?label:i.label ~unknown:i.unknown name (text i.value)

let emit_place t ?language id (a : Info.Anniversary.t) =
  let name =
    match a.kind with
    | `Birth -> Some "BIRTHPLACE"
    | `Death -> Some "DEATHPLACE"
    | _ -> None
  in
  match (name, a.place) with
  | Some name, Some place -> (
      let target = "anniversaries/" ^ Id.to_string id ^ "/place" in
      match (place.full, place.coordinates) with
      | Some full, _ ->
          property t ~id ?language ~target ~unknown:place.unknown name
            (text full)
      | None, Some geo ->
          property t ~id ?language ~target
            ~params:[ param "VALUE" [ "uri" ] ]
            ~unknown:place.unknown name geo
      | None, None -> ())
  | _ -> ()

let emit_email t ?language id (e : Contact.Email_address.t) =
  property t ~id ?language
    ~target:("emails/" ^ Id.to_string id)
    ~params:(contexts e.contexts @ opt_int "PREF" e.pref)
    ?label:e.label ~unknown:e.unknown "EMAIL" (text e.address)

let emit_service t ?language id (s : Contact.Online_service.t) =
  let name =
    match saved_name s.unknown with
    | Some "impp" -> "IMPP"
    | _ -> "SOCIALPROFILE"
  in
  let value, value_param =
    match (s.uri, s.user) with
    | Some u, _ -> (u, [])
    | None, Some user -> (text user, [ param "VALUE" [ "text" ] ])
    | None, None -> ("", [])
  in
  let params =
    opt "SERVICE-TYPE" s.service
    @ (if s.uri <> None then opt "USERNAME" s.user else [])
    @ contexts s.contexts @ opt_int "PREF" s.pref @ value_param
  in
  property t ~id ?language
    ~target:("onlineServices/" ^ Id.to_string id)
    ~params ?label:s.label ~unknown:s.unknown name value

let emit_phone t ?language id (p : Contact.Phone.t) =
  let types =
    List.map context_type (Option.value ~default:[] p.contexts)
    @ List.map feature_type (Option.value ~default:[] p.features)
  in
  let is_uri = Uri.is_valid p.number in
  let params =
    (match types with [] -> [] | ts -> [ param "TYPE" ts ])
    @ opt_int "PREF" p.pref
    @ if is_uri then [ param "VALUE" [ "uri" ] ] else []
  in
  property t ~id ?language
    ~target:("phones/" ^ Id.to_string id)
    ~params ?label:p.label ~unknown:p.unknown "TEL"
    (if is_uri then p.number else text p.number)

let emit_language_pref t ?language id (l : Contact.Language_pref.t) =
  property t ~id ?language
    ~target:("preferredLanguages/" ^ Id.to_string id)
    ~params:(contexts l.contexts @ opt_int "PREF" l.pref)
    ~unknown:l.unknown "LANG" l.language

let emit_calendar t ?language id (r : Calendar.t) =
  match r.kind with
  | `Vendor _ ->
      Option.iter
        (jsprop t ("calendars/" ^ Id.to_string id))
        (encoded t Calendar.jsont r)
  | kind ->
      emit_resource t ?language
        ~name:(match kind with `Free_busy -> "FBURL" | _ -> "CALURI")
        id r

let emit_scheduling t ?language id (s : Calendar.Scheduling_address.t) =
  property t ~id ?language
    ~target:("schedulingAddresses/" ^ Id.to_string id)
    ~params:(contexts s.contexts @ opt_int "PREF" s.pref)
    ?label:s.label ~unknown:s.unknown "CALADRURI" s.uri

let emit_key t ?language id (r : Resource.Crypto_key.t) =
  emit_resource t ?language ~name:"KEY" id r

let emit_directory t ?language id (d : Resource.Directory.t) =
  match d.kind with
  | `Vendor _ ->
      Option.iter
        (jsprop t ("directories/" ^ Id.to_string id))
        (encoded t Resource.Directory.jsont d)
  | `Entry ->
      (* RFC 9555 Section 2.4.3 converts PREF and MEDIATYPE alone for SOURCE,
         so a context or list position has no parameter and is a JSPROP. *)
      let prefix = map_prefix "directories" id in
      property t ~id ?language
        ~params:(opt "MEDIATYPE" d.media_type @ opt_int "PREF" d.pref)
        ?label:d.label ~unknown:d.unknown "SOURCE"
        (Option.value ~default:"" d.uri);
      Option.iter
        (fun cs ->
          Option.iter
            (jsprop t (prefix ^ "contexts"))
            (encoded t Context.set_jsont cs))
        d.contexts;
      Option.iter
        (fun n ->
          jsprop t (prefix ^ "listAs") (Jsont.Json.number (float_of_int n)))
        d.list_as
  | `Directory ->
      let params =
        opt "MEDIATYPE" d.media_type
        @ contexts d.contexts @ opt_int "PREF" d.pref
        @ opt_int "INDEX" d.list_as
      in
      property t ~id ?language ~params ?label:d.label ~unknown:d.unknown
        "ORG-DIRECTORY"
        (Option.value ~default:"" d.uri)

let emit_link t ?language id (l : Resource.Link.t) =
  (match l.kind with
  | Some (`Vendor _ as k) ->
      jsprop t
        (map_prefix "links" id ^ "kind")
        (Jsont.Json.string (Resource.Link.Kind.to_string k))
  | _ -> ());
  emit_resource t ?language
    ~name:(match l.kind with Some `Contact -> "CONTACT-URI" | _ -> "URL")
    id l

let emit_media t ?language id (m : Resource.Media.t) =
  match m.kind with
  | `Vendor _ ->
      Option.iter
        (jsprop t ("media/" ^ Id.to_string id))
        (encoded t Resource.Media.jsont m)
  | kind ->
      emit_resource t ?language
        ~name:
          (match kind with
          | `Photo -> "PHOTO"
          | `Logo -> "LOGO"
          | `Sound -> "SOUND"
          | `Vendor _ -> "PHOTO")
        id m

let emit_anniversary t id (a : Info.Anniversary.t) =
  let name =
    match a.kind with
    | `Birth -> Some "BDAY"
    | `Death -> Some "DEATHDATE"
    | `Wedding -> Some "ANNIVERSARY"
    | `Vendor _ -> None
  in
  let empty =
    match a.date with
    | Date.Partial { year = None; month = None; day = None; _ } -> true
    | _ -> false
  in
  match name with
  | None ->
      Option.iter
        (jsprop t ("anniversaries/" ^ Id.to_string id))
        (encoded t Info.Anniversary.jsont a)
  | Some name ->
      (if not empty then
         let params =
           match a.date with
           | Date.Partial { calendar_scale = Some cs; _ } ->
               [ param "CALSCALE" [ cs ] ]
           | _ -> []
         in
         property t ~id ~params ~unknown:a.unknown name (date_string a.date));
      emit_place t id a

(* The property a localization patch key localizes, as the target of the base
   property it belongs to, or [None] if the key names no converted property. *)
let target_of_key key =
  let parts = String.split_on_char '/' key in
  match parts with
  | [ "name"; "full" ] -> Some "FN"
  | "name" :: ("phoneticSystem" | "phoneticScript") :: _ -> Some "N#phonetic"
  | [ "name"; "components"; _; "phonetic" ] -> Some "N#phonetic"
  | "name" :: _ -> Some "N"
  | [ "addresses"; id; "components"; _; "phonetic" ] ->
      Some ("addresses/" ^ id ^ "#phonetic")
  | "addresses" :: id :: ("phoneticSystem" | "phoneticScript") :: _ ->
      Some ("addresses/" ^ id ^ "#phonetic")
  | "anniversaries" :: id :: "place" :: _ ->
      Some ("anniversaries/" ^ id ^ "/place")
  | "speakToAs" :: "pronouns" :: id :: _ -> Some ("speakToAs/pronouns/" ^ id)
  | [ "speakToAs"; "grammaticalGender" ] -> Some "speakToAs/grammaticalGender"
  | map :: id :: _
    when List.mem map
           [
             "addresses";
             "titles";
             "notes";
             "nicknames";
             "organizations";
             "personalInfo";
             "emails";
             "onlineServices";
             "phones";
             "preferredLanguages";
             "calendars";
             "schedulingAddresses";
             "cryptoKeys";
             "directories";
             "links";
             "media";
           ] ->
      Some (map ^ "/" ^ id)
  | _ -> None

(* The targets a Card localizes or pronounces, each with its ALTID. *)
let targets_of (c : Card.t) =
  let localized =
    List.concat_map
      (fun (_, patch) ->
        List.filter_map (fun (k, _) -> target_of_key k) (Patch.to_list patch))
      (Option.value ~default:[] c.localizations)
  in
  let phonetic =
    (match c.name with
      | Some n when has_phonetics n -> [ "N#phonetic" ]
      | _ -> [])
    @ List.filter_map
        (fun (id, (a : Address.t)) ->
          if
            List.exists
              (fun (x : Address.Component.t) -> x.phonetic <> None)
              (Option.value ~default:[] a.components)
          then Some ("addresses/" ^ Id.to_string id ^ "#phonetic")
          else None)
        (Option.value ~default:[] c.addresses)
  in
  let keys =
    List.sort_uniq String.compare (List.map base_target (localized @ phonetic))
  in
  List.mapi (fun i k -> (k, string_of_int (i + 1))) keys

(* Emits the properties of [c] for the targets in [targets], in [language]. *)
let emit_targets t ?language (c : Card.t) targets =
  let has tg = List.mem tg targets in
  let each map f =
    List.iter (fun (id, v) -> f id v) (Option.value ~default:[] map)
  in
  if has "FN" then emit_fn t ?language c.name;
  Option.iter (fun n -> emit_n t ?language ~targets n) c.name;
  let in_map map id = has (map ^ "/" ^ id) in
  let select map = List.filter (fun (id, _) -> in_map map (Id.to_string id)) in
  each (Option.map (select "nicknames") c.nicknames) (emit_nickname t ?language);
  each
    (Option.map (select "organizations") c.organizations)
    (emit_org t ?language);
  Option.iter
    (fun (s : Org.Speak_to_as.t) ->
      if has "speakToAs/grammaticalGender" then
        Option.iter
          (emit_gender t ?language ~unknown:s.unknown)
          s.grammatical_gender;
      each
        (Option.map (select "speakToAs/pronouns") s.pronouns)
        (emit_pronouns t ?language))
    c.speak_to_as;
  each (Option.map (select "titles") c.titles) (emit_title t ?language);
  each (Option.map (select "emails") c.emails) (emit_email t ?language);
  each
    (Option.map (select "onlineServices") c.online_services)
    (emit_service t ?language);
  each (Option.map (select "phones") c.phones) (emit_phone t ?language);
  each
    (Option.map (select "preferredLanguages") c.preferred_languages)
    (emit_language_pref t ?language);
  each (Option.map (select "calendars") c.calendars) (emit_calendar t ?language);
  each
    (Option.map (select "schedulingAddresses") c.scheduling_addresses)
    (emit_scheduling t ?language);
  List.iter
    (fun (id, a) ->
      let tg = "addresses/" ^ Id.to_string id in
      if has tg || has (tg ^ "#phonetic") then
        emit_adr t ?language ~targets id a)
    (Option.value ~default:[] c.addresses);
  each (Option.map (select "cryptoKeys") c.crypto_keys) (emit_key t ?language);
  each
    (Option.map (select "directories") c.directories)
    (emit_directory t ?language);
  each (Option.map (select "links") c.links) (emit_link t ?language);
  each (Option.map (select "media") c.media) (emit_media t ?language);
  (* A base Card writes each place beside its date, in emit_anniversary. *)
  if language <> None then
    List.iter
      (fun (id, a) ->
        if has ("anniversaries/" ^ Id.to_string id ^ "/place") then
          emit_place t ?language id a)
      (Option.value ~default:[] c.anniversaries);
  each (Option.map (select "notes") c.notes) (emit_note t ?language);
  each
    (Option.map (select "personalInfo") c.personal_info)
    (emit_personal t ?language)

let map_unknowns emit map f entries =
  List.iter (fun (id, v) -> emit (map_prefix map id) (f v)) entries

let components_unknowns emit prefix unknowns =
  List.iteri
    (fun i u -> emit (Printf.sprintf "%scomponents/%d/" prefix i) u)
    unknowns

(* Every Unknown.t the Card holds, with the JSON Pointer prefix that reaches
   it. Used to emit the JSPROP properties and to collect the groups a previous
   conversion preserved. *)
let iter_unknowns emit (c : Card.t) =
  let v = Option.value ~default:[] in
  emit "" c.unknown;
  Option.iter
    (fun (n : Name.t) ->
      emit "name/" n.unknown;
      components_unknowns emit "name/"
        (List.map (fun (x : Name.Component.t) -> x.unknown) (v n.components)))
    c.name;
  map_unknowns emit "nicknames"
    (fun (x : Name.Nickname.t) -> x.unknown)
    (v c.nicknames);
  List.iter
    (fun (id, (o : Org.Organization.t)) ->
      let prefix = map_prefix "organizations" id in
      emit prefix o.unknown;
      List.iteri
        (fun i (u : Org.Organization.Org_unit.t) ->
          emit (Printf.sprintf "%sunits/%d/" prefix i) u.unknown)
        (v o.units))
    (v c.organizations);
  Option.iter
    (fun (s : Org.Speak_to_as.t) ->
      emit "speakToAs/" s.unknown;
      map_unknowns emit "speakToAs/pronouns"
        (fun (x : Org.Speak_to_as.Pronouns.t) -> x.unknown)
        (v s.pronouns))
    c.speak_to_as;
  map_unknowns emit "titles" (fun (x : Org.Title.t) -> x.unknown) (v c.titles);
  map_unknowns emit "emails"
    (fun (x : Contact.Email_address.t) -> x.unknown)
    (v c.emails);
  map_unknowns emit "onlineServices"
    (fun (x : Contact.Online_service.t) -> x.unknown)
    (v c.online_services);
  map_unknowns emit "phones"
    (fun (x : Contact.Phone.t) -> x.unknown)
    (v c.phones);
  map_unknowns emit "preferredLanguages"
    (fun (x : Contact.Language_pref.t) -> x.unknown)
    (v c.preferred_languages);
  map_unknowns emit "calendars"
    (fun (x : Calendar.t) -> x.unknown)
    (v c.calendars);
  map_unknowns emit "schedulingAddresses"
    (fun (x : Calendar.Scheduling_address.t) -> x.unknown)
    (v c.scheduling_addresses);
  List.iter
    (fun (id, (a : Address.t)) ->
      let prefix = map_prefix "addresses" id in
      emit prefix a.unknown;
      components_unknowns emit prefix
        (List.map (fun (x : Address.Component.t) -> x.unknown) (v a.components)))
    (v c.addresses);
  map_unknowns emit "cryptoKeys"
    (fun (x : Resource.Crypto_key.t) -> x.unknown)
    (v c.crypto_keys);
  map_unknowns emit "directories"
    (fun (x : Resource.Directory.t) -> x.unknown)
    (v c.directories);
  map_unknowns emit "links" (fun (x : Resource.Link.t) -> x.unknown) (v c.links);
  map_unknowns emit "media"
    (fun (x : Resource.Media.t) -> x.unknown)
    (v c.media);
  List.iter
    (fun (id, (a : Info.Anniversary.t)) ->
      let prefix = map_prefix "anniversaries" id in
      emit prefix a.unknown;
      (match a.date with
      | Date.Partial d -> emit (prefix ^ "date/") d.unknown
      | Date.Timestamp ts -> emit (prefix ^ "date/") ts.unknown);
      Option.iter
        (fun (p : Address.t) ->
          emit (prefix ^ "place/") p.unknown;
          components_unknowns emit (prefix ^ "place/")
            (List.map
               (fun (x : Address.Component.t) -> x.unknown)
               (v p.components)))
        a.place)
    (v c.anniversaries);
  List.iter
    (fun (id, (n : Info.Note.t)) ->
      let prefix = map_prefix "notes" id in
      emit prefix n.unknown;
      Option.iter
        (fun (a : Info.Note.Author.t) -> emit (prefix ^ "author/") a.unknown)
        n.author)
    (v c.notes);
  map_unknowns emit "personalInfo"
    (fun (x : Info.Personal_info.t) -> x.unknown)
    (v c.personal_info);
  List.iter
    (fun (key, (r : Info.Relation.t)) ->
      emit ("relatedTo/" ^ escape_token key ^ "/") r.unknown)
    (v c.related_to)

let emit_unknowns t c = iter_unknowns (unknown_members t) c

(* A group restored from a preserved vCardParams must not be handed out again
   by new_group, or two unrelated properties would read as related. *)
let taken_groups c =
  let h = Hashtbl.create 8 in
  iter_unknowns
    (fun _ u ->
      match fst (saved_params u) with
      | Some g -> Hashtbl.replace h (String.lowercase_ascii g) ()
      | None -> ())
    c;
  h

let all_targets (c : Card.t) =
  let v = Option.value ~default:[] in
  let ids map entries =
    List.map (fun (id, _) -> map ^ "/" ^ Id.to_string id) entries
  in
  [ "FN"; "N"; "N#phonetic"; "speakToAs/grammaticalGender" ]
  @ ids "nicknames" (v c.nicknames)
  @ ids "organizations" (v c.organizations)
  @ ids "speakToAs/pronouns"
      (v (Option.bind c.speak_to_as (fun s -> s.pronouns)))
  @ ids "titles" (v c.titles)
  @ ids "emails" (v c.emails)
  @ ids "onlineServices" (v c.online_services)
  @ ids "phones" (v c.phones)
  @ ids "preferredLanguages" (v c.preferred_languages)
  @ ids "calendars" (v c.calendars)
  @ ids "schedulingAddresses" (v c.scheduling_addresses)
  @ ids "addresses" (v c.addresses)
  @ List.map (fun tg -> tg ^ "#phonetic") (ids "addresses" (v c.addresses))
  @ ids "cryptoKeys" (v c.crypto_keys)
  @ ids "directories" (v c.directories)
  @ ids "links" (v c.links)
  @ ids "media" (v c.media)
  @ List.map (fun tg -> tg ^ "/place") (ids "anniversaries" (v c.anniversaries))
  @ ids "notes" (v c.notes)
  @ ids "personalInfo" (v c.personal_info)

let convert_card (c : Card.t) =
  let t =
    {
      props = [];
      targets = targets_of c;
      groups = 0;
      org_groups = [];
      taken = taken_groups c;
      error = None;
    }
  in
  (* An organization a title refers to shares a group with it, RFC 9555
     Section 2.9.6. *)
  let org_groups =
    List.filter_map
      (fun (id, (o : Org.Organization.t)) ->
        let referenced =
          List.exists
            (fun (_, (ti : Org.Title.t)) -> ti.organization_id = Some id)
            (Option.value ~default:[] c.titles)
        in
        if referenced then
          Some
            ( Id.to_string id,
              match fst (saved_params o.unknown) with
              | Some g -> g
              | None -> new_group t )
        else None)
      (Option.value ~default:[] c.organizations)
  in
  let t = { t with org_groups } in
  if c.kind <> `Individual then
    property t ~unknown:Unknown.empty "KIND" (Card.Kind.to_string c.kind);
  Option.iter
    (fun l -> property t ~unknown:Unknown.empty "LANGUAGE" l)
    c.language;
  emit_targets t c (all_targets c);
  List.iter
    (fun (id, a) -> emit_anniversary t id a)
    (Option.value ~default:[] c.anniversaries);
  Option.iter
    (fun ks ->
      if ks <> [] then
        property t ~unknown:Unknown.empty "CATEGORIES"
          (Vcard.Text.list_to_string ks))
    c.keywords;
  Option.iter
    (fun u -> property t ~unknown:Unknown.empty "CREATED" (utc_string u))
    c.created;
  Option.iter
    (fun u -> property t ~unknown:Unknown.empty "REV" (utc_string u))
    c.updated;
  Option.iter
    (fun p -> property t ~unknown:Unknown.empty "PRODID" (text p))
    c.prod_id;
  (match c.members with
  | Some [] -> jsprop t "members" (Jsont.Json.object' [])
  | Some ms ->
      List.iter (fun m -> property t ~unknown:Unknown.empty "MEMBER" m) ms
  | None -> ());
  Option.iter
    (List.iter (fun (key, (r : Info.Relation.t)) ->
         let is_uri = Uri.is_valid key in
         let params =
           (match r.relation with
             | [] -> []
             | ks -> [ param "TYPE" (List.map Info.Relation.Kind.to_string ks) ])
           @ if is_uri then [] else [ param "VALUE" [ "text" ] ]
         in
         property t ~params ~unknown:r.unknown "RELATED"
           (if is_uri then key else text key)))
    c.related_to;
  if c.uid <> "" then property t ~unknown:Unknown.empty "UID" c.uid;
  (* Localizations, RFC 9555 Section 2.3.11 reversed. Each patch key that
     names a converted property yields that property in the language, and
     any other key a JSPROP into the localizations. *)
  let* () =
    List.fold_left
      (fun acc (language, patch) ->
        let* () = acc in
        let keys = Patch.to_list patch in
        let targets =
          List.sort_uniq String.compare
            (List.filter_map (fun (k, _) -> target_of_key k) keys)
        in
        let* localized =
          match Card.localize c ~language with
          | Ok (Some lc) -> Ok lc
          | Ok None -> Ok c
          | Error msg ->
              Error (Printf.sprintf "localization %s: %s" language msg)
        in
        emit_targets t ~language localized targets;
        List.iter
          (fun (k, entry) ->
            if target_of_key k = None then
              let value =
                match entry with
                | Patch.Set v -> v
                | Patch.Remove -> Jsont.Json.null ()
              in
              jsprop t
                (Printf.sprintf "localizations/%s/%s" (escape_token language) k)
                value)
          keys;
        Ok ())
      (Ok ())
      (Option.value ~default:[] c.localizations)
  in
  emit_unknowns t c;
  (* vCardProps, RFC 9555 Section 2.15.1 reversed. *)
  let* () =
    match Unknown.find c.unknown "vCardProps" with
    | Some (Jsont.Array (props, _)) ->
        List.fold_left
          (fun acc j ->
            let* () = acc in
            let* p = Jcard.to_property j in
            emit t p;
            Ok ())
          (Ok ()) props
    | _ -> Ok ()
  in
  match t.error with
  | Some msg -> Error msg
  | None -> Ok (Vcard.v (List.rev t.props))
