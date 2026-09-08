(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Jscontact
module P = Vcard.Property
module Jcard = Jscontact_vcard_jcard

let ( let* ) = Result.bind
let str s = Jsont.Json.string s
let mem k v = ((k, Jsont.Meta.none), v)

(* The state of one conversion. Maps are accumulated in reverse and the
   anniversaries are keyed by kind so that a *PLACE merges into the *DATE of
   the same kind. *)
type t = {
  card : Vcard.t;
  labels : (string * string) list;
  org_groups : (string * Id.t) list;
  counters : (string, int) Hashtbl.t;
  used : (string * string, unit) Hashtbl.t;
  reserved : (string, unit) Hashtbl.t;
  mutable language : string option;
  mutable kind : Card.Kind.t option;
  mutable members : string list;
  mutable prod_id : string option;
  mutable related : (string * Info.Relation.t) list;
  mutable created : Date.Utc.t option;
  mutable updated : Date.Utc.t option;
  mutable uid : string option;
  mutable full : string option;
  mutable components : Name.Component.t list option;
  mutable positions : (int * int) option list;
  mutable used_labels : string list;
  mutable speak_unknown : Unknown.t;
  dominant : string option;
  mutable is_ordered : bool;
  mutable default_separator : string option;
  mutable sort_as : (Name.Component.Kind.t * string) list option;
  mutable phonetic_script : string option;
  mutable phonetic_system : Phonetic.t option;
  mutable name_unknown : Unknown.t;
  mutable nicknames : (Id.t * Name.Nickname.t) list;
  mutable organizations : (Id.t * Org.Organization.t) list;
  mutable gender : Org.Speak_to_as.Grammatical_gender.t option;
  mutable pronouns : (Id.t * Org.Speak_to_as.Pronouns.t) list;
  mutable titles : (Id.t * Org.Title.t) list;
  mutable emails : (Id.t * Contact.Email_address.t) list;
  mutable services : (Id.t * Contact.Online_service.t) list;
  mutable phones : (Id.t * Contact.Phone.t) list;
  mutable languages : (Id.t * Contact.Language_pref.t) list;
  mutable calendars : (Id.t * Calendar.t) list;
  mutable scheduling : (Id.t * Calendar.Scheduling_address.t) list;
  mutable addresses : (Id.t * Address.t) list;
  mutable keys : (Id.t * Resource.Crypto_key.t) list;
  mutable directories : (Id.t * Resource.Directory.t) list;
  mutable links : (Id.t * Resource.Link.t) list;
  mutable media : (Id.t * Resource.Media.t) list;
  mutable anniversaries : (Id.t * Info.Anniversary.t) list;
  mutable keywords : string list;
  mutable notes : (Id.t * Info.Note.t) list;
  mutable personal : (Id.t * Info.Personal_info.t) list;
  mutable localizations : (string * (string * Patch.entry) list) list;
  mutable props : Jsont.json list;
  mutable jsprops : (string * Patch.entry) list;
}

(* An Id keys a JSON object, so it must be unique within its map. RFC 9555
   Section 2.3.18 makes a PROP-ID the Id of its object, so every PROP-ID the
   card carries is reserved before conversion starts and a generated key steps
   over it. A generated key also steps over one already spent in its own map. *)
let reserved_ids card =
  let h = Hashtbl.create 16 in
  List.iter
    (fun p ->
      match
        Option.bind (P.prop_id p) (fun s -> Result.to_option (Id.of_string s))
      with
      | Some id -> Hashtbl.replace h (Id.to_string id) ()
      | None -> ())
    (Vcard.properties card);
  h

let take used map id =
  Hashtbl.replace used (map, Id.to_string id) ();
  id

let rec fresh counters used reserved map =
  let n = 1 + Option.value ~default:0 (Hashtbl.find_opt counters map) in
  Hashtbl.replace counters map n;
  let s = Printf.sprintf "k%d" n in
  if Hashtbl.mem reserved s || Hashtbl.mem used (map, s) then
    fresh counters used reserved map
  else take used map (Id.v s)

let pick counters used reserved map p =
  match
    Option.bind (P.prop_id p) (fun s -> Result.to_option (Id.of_string s))
  with
  | Some id when not (Hashtbl.mem used (map, Id.to_string id)) ->
      take used map id
  | _ -> fresh counters used reserved map

let next t map = fresh t.counters t.used t.reserved map
let id t map p = pick t.counters t.used t.reserved map p

(* RFC 9554 Section 4.6: the PHONETIC parameter marks an N or ADR as the
   pronunciation of the same-named property with the same ALTID. *)
let is_phonetic p = P.find_first p "PHONETIC" <> None

(* RFC 9555 Section 2.3.11 and 2.5.2. The dominant language of a vCard is
   its LANGUAGE property, or else the LANGUAGE parameter most properties
   carry. *)
let dominant card =
  match Vcard.find card "LANGUAGE" with
  | Some p -> Some (String.lowercase_ascii (P.text p))
  | None -> (
      let langs =
        List.filter_map
          (fun p -> Option.map String.lowercase_ascii (P.language p))
          (Vcard.properties card)
      in
      let counts =
        List.fold_left
          (fun acc l ->
            if List.mem_assoc l acc then acc
            else (l, List.length (List.filter (String.equal l) langs)) :: acc)
          [] langs
      in
      match List.rev counts with
      | [] -> None
      | c :: cs ->
          Some
            (fst
               (List.fold_left
                  (fun (bl, bn) (l, n) -> if n > bn then (l, n) else (bl, bn))
                  c cs)))

(* The instances a property is an alternative of. Instances share an ALTID,
   or for a property a card holds at most once, such as N or FN, they need
   not. A phonetic instance relates to the non-phonetic ones. *)
let siblings_in card p =
  let same_altid q =
    match (P.altid p, P.altid q) with
    | Some a, Some b -> String.equal a b
    | None, None ->
        Vcard.Registry.cardinality (P.name p) <> Vcard.Registry.Many
        || P.name p = "FN"
    | _ -> false
  in
  List.filter
    (fun q -> P.name q = P.name p && same_altid q && not (is_phonetic q))
    (Vcard.properties card)

let siblings t p = siblings_in t.card p

(* Within a group of alternatives, the instance without a LANGUAGE is the
   base, and every such instance of a property a card holds many times is
   one, RFC 9555 Section 2.3.11. When every instance has a LANGUAGE, the
   instance in the dominant language is the base, and otherwise the first. A
   phonetic instance is a base if its language is that of the base it relates
   to. *)
let is_base t p =
  let lower = Option.map String.lowercase_ascii in
  let fewest_params = function
    | [] -> None
    | q :: qs ->
        Some
          (List.fold_left
             (fun best q ->
               if List.length (P.params q) < List.length (P.params best) then q
               else best)
             q qs)
  in
  let base_of group =
    match List.filter (fun q -> P.language q = None) group with
    | q :: _ when P.name q <> "FN" -> Some q
    | _ :: _ as untagged -> fewest_params untagged
    | [] -> (
        match group with
        | [] -> None
        | _ -> (
            match
              List.find_opt (fun q -> lower (P.language q) = t.dominant) group
            with
            | Some q -> Some q
            | None -> ( match group with q :: _ -> Some q | [] -> None)))
  in
  let group = siblings t p in
  if is_phonetic p then
    match base_of group with
    | Some base ->
        P.language p = None || lower (P.language p) = lower (P.language base)
    | None -> P.language p = None || lower (P.language p) = t.dominant
  else
    match base_of group with
    | Some q ->
        q == p
        || P.language p = None
           && Vcard.Registry.cardinality (P.name p) = Vcard.Registry.Many
    | None -> true

(* The parameters no rule converts go to vCardParams, RFC 9555 Section 2.15.2,
   and the group with them, Section 2.3.9. *)
let vcard_params ~consumed ?(unknown = Unknown.empty) p =
  let consumed = "PROP-ID" :: "ALTID" :: consumed in
  let rest =
    List.filter
      (fun x -> not (List.mem (Vcard.Param.name x) consumed))
      (P.params p)
  in
  match (rest, P.group p) with
  | [], None -> unknown
  | _ ->
      let params =
        List.map
          (fun x ->
            let name = String.lowercase_ascii (Vcard.Param.name x) in
            match Vcard.Param.values x with
            | [ v ] -> mem name (str v)
            | vs -> mem name (Jsont.Json.list (List.map str vs)))
          rest
      in
      let group =
        match P.group p with Some g -> [ mem "group" (str g) ] | None -> []
      in
      Unknown.add unknown "vCardParams" (Jsont.Json.object' (group @ params))

let with_name unknown name = Unknown.add unknown "vCardName" (str name)

(* RFC 9555 Section 2.3.22: "home" and "work" are the private and work
   contexts. Any other TYPE value stays a parameter. *)
let contexts p =
  let cs =
    List.filter_map
      (function
        | "home" -> Some `Private
        | "work" -> Some `Work
        | "billing" when P.name p = "ADR" -> Some `Billing
        | "delivery" when P.name p = "ADR" -> Some `Delivery
        | _ -> None)
      (P.types p)
  in
  match cs with [] -> None | cs -> Some (List.sort_uniq Context.compare cs)

let other_types ~known p =
  match List.filter (fun x -> not (List.mem x known)) (P.types p) with
  | [] -> []
  | rest -> [ Vcard.Param.v "TYPE" rest ]

let context_types = [ "home"; "work" ]

(* TYPE values that converted to contexts are consumed. Any other value is
   kept as a TYPE parameter in vCardParams by re-adding it. *)
let leftover_types ?(known = context_types) p =
  P.params p |> List.filter (fun x -> Vcard.Param.name x <> "TYPE") |> fun ps ->
  ps @ other_types ~known p

let params_of t ~consumed ?known ?unknown p =
  let p' =
    P.v ?group:(P.group p) ~params:(leftover_types ?known p) (P.name p)
      (P.value p)
  in
  (* A LANGUAGE that is the Card's own or that names a localization says
     nothing more, RFC 9555 Section 2.3.11, and any other stays a parameter. *)
  let consumed =
    if
      Option.map String.lowercase_ascii (P.language p)
      = Option.map String.lowercase_ascii t.language
      || not (is_base t p)
    then "LANGUAGE" :: consumed
    else consumed
  in
  vcard_params ~consumed ?unknown p'

let label t p =
  match P.group p with
  | Some g -> (
      let g = String.lowercase_ascii g in
      match List.assoc_opt g t.labels with
      | Some l ->
          t.used_labels <- g :: t.used_labels;
          Some l
      | None -> None)
  | None -> None

(* A property whose type is reset to text is unescaped, and a URI is kept as
   it is. *)
let text_or_uri p =
  if Vcard.Value_type.equal (P.value_type p) Vcard.Value_type.Text then P.text p
  else P.value p

let pref p = P.pref p

let utc_of_timestamp ts =
  Option.bind (Vcard.Date.Timestamp.to_ptime ts) (fun _ ->
      Vcard.Date.Timestamp.to_ptime ts)

let timestamp_value p =
  match P.timestamp p with Ok ts -> utc_of_timestamp ts | Error _ -> None

(* A second alternative for the same language and path cannot be a patch,
   since a PatchObject binds a key once, so it stays a vCard property. *)
(* A language tag is matched regardless of case, RFC 5646 Section 2.1.1, and
   keeps the spelling of its first occurrence. *)
let push_loc t lang path entry =
  let same l =
    String.equal (String.lowercase_ascii l) (String.lowercase_ascii lang)
  in
  let lang, existing =
    match List.find_opt (fun (l, _) -> same l) t.localizations with
    | Some (l, entries) -> (l, entries)
    | None -> (lang, [])
  in
  if List.mem_assoc path existing then false
  else (
    t.localizations <-
      (lang, existing @ [ (path, entry) ])
      :: List.filter (fun (l, _) -> not (same l)) t.localizations;
    true)

let to_json codec v =
  match Jsont.Json.encode codec v with
  | Ok j -> j
  | Error _ -> Jsont.Json.null ()

let unknown_property t p = t.props <- Jcard.of_property p :: t.props

(* RFC 9555 Section 2.2.2 and 2.5.1: a timestamp is a Timestamp, a date with
   a year or with both a month and a day is a PartialDate, and anything else
   is not a JSContact date. *)
let anniversary_date p =
  match P.value_type p with
  | Vcard.Value_type.Text | Vcard.Value_type.Uri | Vcard.Value_type.Other _ ->
      None
  | _ -> (
      match Vcard.Date.of_string (P.value p) with
      | Ok (Vcard.Date.Date_time _) -> (
          match Vcard.Date.Timestamp.of_string (P.value p) with
          | Ok ts ->
              Option.map
                (fun utc -> Date.Timestamp (Date.Timestamp.make utc))
                (utc_of_timestamp ts)
          | Error _ -> None)
      | Ok (Vcard.Date.Date { year; month; day }) ->
          if year <> None || (month <> None && day <> None) then
            Some
              (Date.Partial
                 (Date.Partial_date.make ?year ?month ?day
                    ?calendar_scale:
                      (Option.map String.lowercase_ascii
                         (P.find_first p "CALSCALE"))
                    ()))
          else None
      | _ -> None)

(* RFC 9555 Section 2.5.5, Table 1. A value in the secondary surname component
   is not repeated from the family names, nor a generation from the suffixes. *)
let name_components (n : Vcard.N.t) =
  let minus xs ys = List.filter (fun x -> not (List.mem x ys)) xs in
  let kind k vs = List.map (fun v -> Name.Component.make k v) vs in
  kind `Surname (minus n.family n.surname2)
  @ kind `Given n.given @ kind `Given2 n.additional @ kind `Title n.prefixes
  @ kind `Credential (minus n.suffixes n.generation)
  @ kind `Surname2 n.surname2
  @ kind `Generation n.generation

(* The N component kinds by position, for JSCOMPS. *)
let n_kind_at i =
  match i with
  | 0 -> Some `Surname
  | 1 -> Some `Given
  | 2 -> Some `Given2
  | 3 -> Some `Title
  | 4 -> Some `Credential
  | 5 -> Some `Surname2
  | 6 -> Some `Generation
  | _ -> None

let adr_kind_at (a : Vcard.Adr.t) i =
  let ext = Vcard.Adr.has_extended a in
  match i with
  | 0 -> Some `Post_office_box
  | 1 when not ext -> Some `Apartment
  | 2 when not ext -> Some `Name
  | 3 -> Some `Locality
  | 4 -> Some `Region
  | 5 -> Some `Postcode
  | 6 -> Some `Country
  | 7 -> Some `Room
  | 8 -> Some `Apartment
  | 9 -> Some `Floor
  | 10 -> Some `Number
  | 11 -> Some `Name
  | 12 -> Some `Building
  | 13 -> Some `Block
  | 14 -> Some `Subdistrict
  | 15 -> Some `District
  | 16 -> Some `Landmark
  | 17 -> Some `Direction
  | _ -> None

(* RFC 9555 Section 3.3.1: order the components by the JSCOMPS positions and
   insert its separators. [kind_at] names the kind of a structured value
   position and [structured] the values. The parameter is valid only if every
   position exists and the positionals number the deduplicated component
   values, and the result is [None] otherwise, in which case the parameter is
   treated as absent. *)
let ordered ~kind_at ~make ~values structured (j : Vcard.Jscomps.t) =
  let positionals =
    List.length
      (List.filter
         (function Vcard.Jscomps.Position _ -> true | _ -> false)
         j.entries)
  in
  if positionals <> values then None
  else
    let rec go = function
      | [] -> Some []
      | Vcard.Jscomps.Separator s :: rest ->
          Option.map (fun cs -> make `Separator s :: cs) (go rest)
      | Vcard.Jscomps.Position (i, k) :: rest -> (
          match (kind_at i, List.nth_opt structured i) with
          | Some kind, Some values -> (
              match List.nth_opt values k with
              | Some v -> Option.map (fun cs -> make kind v :: cs) (go rest)
              | None -> None)
          | _ -> None)
    in
    go j.entries

let jscomps p =
  match P.find_first p "JSCOMPS" with
  | None -> None
  | Some s -> Result.to_option (Vcard.Jscomps.of_string s)

(* The position of each component in the structured value, for a phonetic N
   whose values sit at the same positions. *)
let positions_of ~slot structured cs =
  List.map
    (fun (kind, value) ->
      match slot kind with
      | None -> None
      | Some i -> (
          match List.nth_opt structured i with
          | None -> None
          | Some vs ->
              let rec find j = function
                | [] -> None
                | v :: _ when String.equal v value -> Some (i, j)
                | _ :: vs -> find (j + 1) vs
              in
              find 0 vs))
    cs

let n_slot = function
  | `Surname -> Some 0
  | `Given -> Some 1
  | `Given2 -> Some 2
  | `Title -> Some 3
  | `Credential -> Some 4
  | `Surname2 -> Some 5
  | `Generation -> Some 6
  | `Separator | `Vendor _ -> None

(* The number of values an N holds once a surname2 repeated in the family
   names and a generation repeated in the suffixes count once. *)
let n_values (n : Vcard.N.t) =
  List.length (List.concat (Vcard.N.to_components n))
  - List.length (List.filter (fun v -> List.mem v n.surname2) n.family)
  - List.length (List.filter (fun v -> List.mem v n.generation) n.suffixes)

let adr_values (a : Vcard.Adr.t) =
  List.length (List.concat (Vcard.Adr.to_components a))
  - if Vcard.Adr.has_extended a then List.length a.street else 0

let name_from_n p =
  let n = Vcard.N.of_value (P.value p) in
  let structured = Vcard.N.to_components n in
  let make kind v = Name.Component.make kind v in
  let positions cs =
    positions_of ~slot:n_slot structured
      (List.map (fun (c : Name.Component.t) -> (c.kind, c.value)) cs)
  in
  match jscomps p with
  | Some j -> (
      match
        ordered ~kind_at:n_kind_at ~make ~values:(n_values n) structured j
      with
      | Some cs ->
          let positions =
            List.map
              (function
                | Vcard.Jscomps.Position (i, k) -> Some (i, k)
                | Vcard.Jscomps.Separator _ -> None)
              j.entries
          in
          (cs, positions, true, j.default_separator)
      | None ->
          let cs = name_components n in
          (cs, positions cs, false, None))
  | None ->
      let cs = name_components n in
      (cs, positions cs, false, None)

let adr_components (a : Vcard.Adr.t) =
  let ext = Vcard.Adr.has_extended a in
  let kind k vs = List.map (fun v -> Address.Component.make k v) vs in
  kind `Post_office_box a.po_box
  @ (if ext then [] else kind `Apartment a.extended)
  @ (if ext then [] else kind `Name a.street)
  @ kind `Locality a.locality @ kind `Region a.region
  @ kind `Postcode a.postal_code
  @ kind `Country a.country @ kind `Room a.room
  @ kind `Apartment a.apartment
  @ kind `Floor a.floor
  @ kind `Number a.street_number
  @ kind `Name a.street_name @ kind `Building a.building @ kind `Block a.block
  @ kind `Subdistrict a.subdistrict
  @ kind `District a.district @ kind `Landmark a.landmark
  @ kind `Direction a.direction

(* RFC 9555 Section 2.8.2: a UTC offset with zero minutes and an hour offset
   from -12 to +14 is an Etc/GMT zone, whose sign is reversed. *)
let time_zone_of_offset o =
  if o mod 60 <> 0 then None
  else
    let h = o / 60 in
    if h < -12 || h > 14 then None
    else if h = 0 then Some "Etc/UTC"
    else Some (Printf.sprintf "Etc/GMT%+d" (-h))

let tz_value p =
  match P.value_type p with
  | Vcard.Value_type.Text -> Some (P.text p)
  | Vcard.Value_type.Utc_offset -> (
      match P.utc_offset p with
      | Ok o -> time_zone_of_offset o
      | Error _ -> None)
  | _ -> None

let address_of_adr t p =
  let a = Vcard.Adr.of_value (P.value p) in
  let structured = Vcard.Adr.to_components a in
  let make kind v = Address.Component.make kind v in
  let components, is_ordered, default_separator =
    match
      Option.bind (jscomps p)
        (ordered ~kind_at:(adr_kind_at a) ~make ~values:(adr_values a)
           structured)
    with
    | Some cs ->
        (cs, true, Option.bind (jscomps p) (fun j -> j.default_separator))
    | None -> (adr_components a, false, None)
  in
  let components = match components with [] -> None | cs -> Some cs in
  let tz =
    Option.map
      (fun s ->
        match Vcard.Date.Utc_offset.of_string s with
        | Ok o -> Option.value (time_zone_of_offset o) ~default:s
        | Error _ -> s)
      (P.find_first p "TZ")
  in
  let phonetic_system =
    match P.find_first p "PHONETIC" with
    | Some s when String.lowercase_ascii s <> "script" ->
        Some (Phonetic.of_string (String.lowercase_ascii s))
    | _ -> None
  in
  let consumed =
    [
      "LABEL";
      "GEO";
      "TZ";
      "CC";
      "PREF";
      "JSCOMPS";
      "PHONETIC";
      "SCRIPT";
      "VALUE";
    ]
  in
  Address.make ?components ~is_ordered ?default_separator
    ?full:(Option.map Vcard.Text.unescape (P.find_first p "LABEL"))
    ?coordinates:(P.find_first p "GEO") ?time_zone:tz
    ?country_code:(P.find_first p "CC") ?contexts:(contexts p) ?pref:(pref p)
    ?phonetic_script:(P.find_first p "SCRIPT") ?phonetic_system
    ~unknown:
      (params_of t ~consumed
         ~known:("billing" :: "delivery" :: context_types)
         p)
    ()
  |> fun a ->
  (* An Address has no label property, but the LABEL parameter is consumed
     here so that it is not carried through as an unknown member. *)
  ignore (label t p : string option);
  a

let adr_slot ~ext = function
  | `Post_office_box -> Some 0
  | `Apartment -> Some (if ext then 8 else 1)
  | `Name -> Some (if ext then 11 else 2)
  | `Locality -> Some 3
  | `Region -> Some 4
  | `Postcode -> Some 5
  | `Country -> Some 6
  | `Room -> Some 7
  | `Floor -> Some 9
  | `Number -> Some 10
  | `Building -> Some 12
  | `Block -> Some 13
  | `Subdistrict -> Some 14
  | `District -> Some 15
  | `Landmark -> Some 16
  | `Direction -> Some 17
  | `Separator | `Vendor _ -> None

(* The values of a phonetic ADR at the positions of the components of the
   base ADR [base], per RFC 9554 Section 4.6. *)
let phonetic_values_adr ~base (components : Address.Component.t list) p =
  let base_adr = Vcard.Adr.of_value (P.value base) in
  let ext = Vcard.Adr.has_extended base_adr in
  let positions =
    positions_of ~slot:(adr_slot ~ext)
      (Vcard.Adr.to_components base_adr)
      (List.map (fun (c : Address.Component.t) -> (c.kind, c.value)) components)
  in
  let structured = Vcard.Adr.to_components (Vcard.Adr.of_value (P.value p)) in
  List.map
    (function
      | Some (i, j) ->
          Option.value ~default:""
            (Option.bind (List.nth_opt structured i) (fun vs ->
                 List.nth_opt vs j))
      | None -> "")
    positions

let with_phonetics (a : Address.t) p values =
  let phonetic_system =
    match P.find_first p "PHONETIC" with
    | Some s when String.lowercase_ascii s <> "script" ->
        Some (Phonetic.of_string (String.lowercase_ascii s))
    | _ -> a.phonetic_system
  in
  let phonetic_script =
    match P.find_first p "SCRIPT" with
    | Some s -> Some s
    | None -> a.phonetic_script
  in
  let components =
    Option.map
      (List.mapi (fun i (c : Address.Component.t) ->
           match List.nth_opt values i with
           | Some v when v <> "" -> { c with phonetic = Some v }
           | _ -> c))
      a.components
  in
  { a with components; phonetic_system; phonetic_script }

let phonetic_patches ~prefix p (components : string list) =
  let system =
    match P.find_first p "PHONETIC" with
    | Some s when String.lowercase_ascii s <> "script" ->
        [
          ( prefix ^ "/phoneticSystem",
            Patch.Set (str (String.lowercase_ascii s)) );
        ]
    | _ -> []
  in
  let script =
    match P.find_first p "SCRIPT" with
    | Some s -> [ (prefix ^ "/phoneticScript", Patch.Set (str s)) ]
    | None -> []
  in
  system @ script
  @ List.concat
      (List.mapi
         (fun i v ->
           if v = "" then []
           else
             [
               ( Printf.sprintf "%s/components/%d/phonetic" prefix i,
                 Patch.Set (str v) );
             ])
         components)

(* The values of a phonetic N at the positions of the base components. *)
let phonetic_values_n t p =
  let structured = Vcard.N.to_components (Vcard.N.of_value (P.value p)) in
  List.map
    (function
      | Some (i, j) ->
          Option.value ~default:""
            (Option.bind (List.nth_opt structured i) (fun vs ->
                 List.nth_opt vs j))
      | None -> "")
    t.positions

let with_phonetics_n t p =
  let values = phonetic_values_n t p in
  (match P.find_first p "PHONETIC" with
  | Some s when String.lowercase_ascii s <> "script" ->
      t.phonetic_system <- Some (Phonetic.of_string (String.lowercase_ascii s))
  | _ -> ());
  (match P.find_first p "SCRIPT" with
  | Some s -> t.phonetic_script <- Some s
  | None -> ());
  match t.components with
  | None -> ()
  | Some cs ->
      t.components <-
        Some
          (List.mapi
             (fun i (c : Name.Component.t) ->
               match List.nth_opt values i with
               | Some v when v <> "" -> { c with phonetic = Some v }
               | _ -> c)
             cs)

(* The Card takes the dominant language when the name carries it or when a
   localization exists, so that the base instances are known to be in it. *)
let note_language t p =
  match (P.language p, t.language) with
  | Some l, None when Some (String.lowercase_ascii l) = t.dominant ->
      t.language <- Some l
  | _ -> ()

let relation_kinds p =
  List.map
    (fun s -> Info.Relation.Kind.of_string (String.lowercase_ascii s))
    (P.types p)

let level_of p kind =
  match Option.map String.lowercase_ascii (P.find_first p "LEVEL") with
  | None -> None
  | Some l ->
      let l =
        if kind = `Expertise then
          match l with
          | "beginner" -> "low"
          | "average" -> "medium"
          | "expert" -> "high"
          | l -> l
        else l
      in
      Some (Info.Personal_info.Level.of_string l)

let index p = Option.bind (P.find_first p "INDEX") int_of_string_opt

let resource_params t ?(extra = []) p =
  params_of t ~consumed:("MEDIATYPE" :: "PREF" :: "VALUE" :: extra) p

let anniversary t kind ~date ~place ~unknown p =
  let existing =
    List.find_opt
      (fun (_, (a : Info.Anniversary.t)) -> a.kind = kind)
      t.anniversaries
  in
  match existing with
  | Some (id, a) ->
      let a =
        {
          a with
          date = Option.value date ~default:a.date;
          place = (match place with Some p -> Some p | None -> a.place);
          unknown = (if date <> None then unknown else a.unknown);
        }
      in
      t.anniversaries <-
        List.map
          (fun (i, x) -> if Id.equal i id then (i, a) else (i, x))
          t.anniversaries
  | None ->
      let id = id t "anniversaries" p in
      let unknown = if date <> None then unknown else Unknown.empty in
      let date =
        Option.value date ~default:(Date.Partial (Date.Partial_date.make ()))
      in
      t.anniversaries <-
        (id, Info.Anniversary.make ?place ~unknown kind date) :: t.anniversaries

let place_of t p =
  let unknown = params_of t ~consumed:[ "VALUE" ] p in
  if Vcard.Value_type.equal (P.value_type p) Vcard.Value_type.Uri then
    if String.starts_with ~prefix:"geo:" (String.lowercase_ascii (P.value p))
    then Some (Address.make ~coordinates:(P.value p) ~unknown ())
    else None
  else Some (Address.make ~full:(P.text p) ~unknown ())

let map_path map id sub = String.concat "/" ([ map; Id.to_string id ] @ sub)

(* Converts one property. A property that is a language alternative of a base
   instance becomes a localization patch at the path the base converted to,
   which [paths] records as the base is converted. *)
let convert t (paths : (string * string, string) Hashtbl.t) p =
  let name = P.name p in
  let key q = (P.name q, Option.value ~default:"" (P.altid q)) in
  (* An alternative in the Card's own language localizes nothing. *)
  let localize path json =
    match P.language p with
    | Some lang
      when Option.map String.lowercase_ascii t.language
           <> Some (String.lowercase_ascii lang) ->
        if not (push_loc t lang path (Patch.Set json)) then unknown_property t p
    | _ -> unknown_property t p
  in
  let base = is_base t p in
  let record path = Hashtbl.replace paths (key p) path in
  (* Adds [obj] to a map as a base instance, or localizes the base instance
     it is an alternative of. *)
  let place ~map ~codec obj add =
    if base then (
      let id = id t map p in
      add id;
      record (map_path map id []))
    else
      Option.iter
        (fun path -> localize path (to_json codec obj))
        (Hashtbl.find_opt paths (key p))
  in
  let alternate_path () = Hashtbl.find_opt paths (key p) in
  match name with
  | "KIND" ->
      t.kind <- Some (Card.Kind.of_string (String.lowercase_ascii (P.text p)))
  | "SOURCE" ->
      let d =
        Resource.Directory.make ~uri:(P.uri p) ?media_type:(P.media_type p)
          ?pref:(pref p) ?label:(label t p) ~unknown:(resource_params t p)
          `Entry
      in
      place ~map:"directories" ~codec:Resource.Directory.jsont d (fun id ->
          t.directories <- (id, d) :: t.directories)
  | "FN" ->
      let derived =
        Option.map String.lowercase_ascii (P.find_first p "DERIVED")
        = Some "true"
      in
      (* Section 3.1 writes an empty FN when a Card has no name to put there,
         so an empty value is no name rather than a name that is empty. *)
      if derived || P.text p = "" then ()
      else if base then
        if t.full = None then (
          t.full <- Some (P.text p);
          note_language t p;
          record "name/full")
        else unknown_property t p
      else localize "name/full" (str (P.text p))
  | "N" when is_phonetic p ->
      if base && t.components <> None then with_phonetics_n t p
      else if base then unknown_property t p
      else
        List.iter
          (fun (path, entry) ->
            match P.language p with
            | Some l -> ignore (push_loc t l path entry)
            | None -> ())
          (phonetic_patches ~prefix:"name" p (phonetic_values_n t p))
  | "N" ->
      let cs, positions, is_ordered, default_separator = name_from_n p in
      if base then
        if t.components = None then (
          t.components <- Some cs;
          t.positions <- positions;
          t.is_ordered <- is_ordered;
          t.default_separator <- default_separator;
          note_language t p;
          record "name";
          let sort = P.sort_as p in
          if sort <> [] then
            t.sort_as <-
              Some
                (List.filter_map
                   (fun (i, v) ->
                     if v = "" then None
                     else Option.map (fun k -> (k, v)) (n_kind_at i))
                   (List.mapi (fun i v -> (i, v)) sort));
          t.name_unknown <-
            params_of t
              ~consumed:[ "SORT-AS"; "JSCOMPS"; "VALUE" ]
              ~unknown:t.name_unknown p)
        else unknown_property t p
      else
        localize "name/components"
          (to_json (Jsont.list Name.Component.jsont) cs)
  | "NICKNAME" ->
      (* Each value of the list is a Nickname of its own, so the PROP-ID names
         the first and its siblings are numbered. *)
      List.iteri
        (fun i nick ->
          let key_i =
            (name, Option.value ~default:"" (P.altid p) ^ "#" ^ string_of_int i)
          in
          if base then (
            let id = if i = 0 then id t "nicknames" p else next t "nicknames" in
            t.nicknames <-
              ( id,
                Name.Nickname.make ?contexts:(contexts p) ?pref:(pref p)
                  ~unknown:(params_of t ~consumed:[ "PREF"; "VALUE" ] p)
                  nick )
              :: t.nicknames;
            Hashtbl.replace paths key_i (map_path "nicknames" id [ "name" ]))
          else
            Option.iter
              (fun path -> localize path (str nick))
              (Hashtbl.find_opt paths key_i))
        (P.text_list p)
  | "PHOTO" | "LOGO" | "SOUND" ->
      let kind =
        match name with "PHOTO" -> `Photo | "LOGO" -> `Logo | _ -> `Sound
      in
      let m =
        Resource.Media.make ~uri:(P.uri p) ?media_type:(P.media_type p)
          ?contexts:(contexts p) ?pref:(pref p) ?label:(label t p)
          ~unknown:(resource_params t p) kind
      in
      place ~map:"media" ~codec:Resource.Media.jsont m (fun id ->
          t.media <- (id, m) :: t.media)
  | "BDAY" | "ANNIVERSARY" | "DEATHDATE" -> (
      let kind =
        match name with
        | "BDAY" -> `Birth
        | "ANNIVERSARY" -> `Wedding
        | _ -> `Death
      in
      match anniversary_date p with
      | Some date ->
          anniversary t kind ~date:(Some date) ~place:None
            ~unknown:(params_of t ~consumed:[ "CALSCALE"; "VALUE" ] p)
            p
      | None -> unknown_property t p)
  | "BIRTHPLACE" | "DEATHPLACE" -> (
      let kind = if name = "BIRTHPLACE" then `Birth else `Death in
      match place_of t p with
      | None -> unknown_property t p
      | Some place ->
          (* place_of reads a geo: URI as coordinates and anything else as a
             full address, so the property this patches must follow it. An
             alternative need not have the shape of its base. *)
          let coords = place.Address.coordinates <> None in
          let sub = if coords then "coordinates" else "full" in
          let value = if coords then P.value p else P.text p in
          if base then (
            anniversary t kind ~date:None ~place:(Some place)
              ~unknown:Unknown.empty p;
            note_language t p;
            let id, _ =
              List.find
                (fun (_, (a : Info.Anniversary.t)) -> a.kind = kind)
                t.anniversaries
            in
            record (map_path "anniversaries" id [ "place"; sub ]))
          else
            Option.iter
              (fun path ->
                let path =
                  match String.rindex_opt path '/' with
                  | Some i -> String.sub path 0 (i + 1) ^ sub
                  | None -> path
                in
                localize path (str value))
              (alternate_path ()))
  | "GENDER" | "XML" | "CLIENTPIDMAP" | "VERSION" -> unknown_property t p
  | "GRAMGENDER" ->
      if base then (
        t.speak_unknown <-
          params_of t ~consumed:[ "VALUE" ] ~unknown:t.speak_unknown p;
        t.gender <-
          Some
            (Org.Speak_to_as.Grammatical_gender.of_string
               (String.lowercase_ascii (P.text p)));
        note_language t p;
        record "speakToAs/grammaticalGender")
      else
        localize "speakToAs/grammaticalGender"
          (str (String.lowercase_ascii (P.text p)))
  | "PRONOUNS" ->
      if base then (
        let id = id t "pronouns" p in
        t.pronouns <-
          ( id,
            Org.Speak_to_as.Pronouns.make ?contexts:(contexts p) ?pref:(pref p)
              ~unknown:(params_of t ~consumed:[ "PREF" ] p)
              (P.text p) )
          :: t.pronouns;
        note_language t p;
        record (map_path "speakToAs/pronouns" id [ "pronouns" ]))
      else
        Option.iter
          (fun path -> localize path (str (P.text p)))
          (alternate_path ())
  | "ADR" when is_phonetic p -> (
      let base_prop = List.find_opt (fun q -> is_base t q) (siblings t p) in
      let base_address () =
        match alternate_path () with
        | Some path -> (
            match String.split_on_char '/' path with
            | [ "addresses"; id ] ->
                List.find_opt (fun (i, _) -> Id.to_string i = id) t.addresses
            | _ -> None)
        | None -> None
      in
      match (base_prop, base_address ()) with
      | Some base_prop, Some (id, a) ->
          let values =
            phonetic_values_adr ~base:base_prop
              (Option.value ~default:[] a.components)
              p
          in
          if base then
            t.addresses <-
              List.map
                (fun (i, x) ->
                  if Id.equal i id then (i, with_phonetics x p values)
                  else (i, x))
                t.addresses
          else
            List.iter
              (fun (k, e) ->
                match P.language p with
                | Some l -> ignore (push_loc t l k e)
                | None -> ())
              (phonetic_patches
                 ~prefix:("addresses/" ^ Id.to_string id)
                 p values)
      | _ -> unknown_property t p)
  | "ADR" ->
      if base then (
        let id = id t "addresses" p in
        t.addresses <- (id, address_of_adr t p) :: t.addresses;
        note_language t p;
        record (map_path "addresses" id []))
      else
        Option.iter
          (fun path ->
            localize path (to_json Address.jsont (address_of_adr t p)))
          (alternate_path ())
  | "EMAIL" ->
      let e =
        Contact.Email_address.make ?contexts:(contexts p) ?pref:(pref p)
          ?label:(label t p)
          ~unknown:(params_of t ~consumed:[ "PREF"; "VALUE" ] p)
          (P.text p)
      in
      place ~map:"emails" ~codec:Contact.Email_address.jsont e (fun id ->
          t.emails <- (id, e) :: t.emails)
  | "IMPP" | "SOCIALPROFILE" ->
      let is_text =
        Vcard.Value_type.equal (P.value_type p) Vcard.Value_type.Text
      in
      let uri, user =
        if name = "SOCIALPROFILE" && is_text then (None, Some (P.text p))
        else (Some (P.uri p), None)
      in
      let user =
        match P.find_first p "USERNAME" with Some u -> Some u | None -> user
      in
      let unknown =
        params_of t
          ~consumed:[ "SERVICE-TYPE"; "USERNAME"; "PREF"; "MEDIATYPE"; "VALUE" ]
          p
      in
      let unknown =
        if name = "IMPP" then with_name unknown "impp" else unknown
      in
      let o =
        Contact.Online_service.make
          ?service:(P.find_first p "SERVICE-TYPE")
          ?uri ?user ?contexts:(contexts p) ?pref:(pref p) ?label:(label t p)
          ~unknown ()
      in
      place ~map:"onlineServices" ~codec:Contact.Online_service.jsont o
        (fun id -> t.services <- (id, o) :: t.services)
  | "LANG" ->
      let l =
        Contact.Language_pref.make ?contexts:(contexts p) ?pref:(pref p)
          ~unknown:(params_of t ~consumed:[ "PREF"; "VALUE" ] p)
          (P.text p)
      in
      place ~map:"preferredLanguages" ~codec:Contact.Language_pref.jsont l
        (fun id -> t.languages <- (id, l) :: t.languages)
  | "LANGUAGE" -> t.language <- Some (P.text p)
  | "TEL" ->
      let features =
        List.filter_map
          (function
            | "cell" -> Some `Mobile
            | "voice" -> Some `Voice
            | "text" -> Some `Text
            | "video" -> Some `Video
            | "main-number" -> Some `Main_number
            | "textphone" -> Some `Textphone
            | "fax" -> Some `Fax
            | "pager" -> Some `Pager
            | _ -> None)
          (P.types p)
      in
      let features =
        match List.sort_uniq Contact.Phone.Feature.compare features with
        | [] -> None
        | fs -> Some fs
      in
      let known =
        context_types
        @ [
            "cell";
            "voice";
            "text";
            "video";
            "main-number";
            "textphone";
            "fax";
            "pager";
          ]
      in
      let ph =
        Contact.Phone.make ?features ?contexts:(contexts p) ?pref:(pref p)
          ?label:(label t p)
          ~unknown:
            (params_of t ~consumed:[ "PREF"; "VALUE"; "MEDIATYPE" ] ~known p)
          (if Vcard.Value_type.equal (P.value_type p) Vcard.Value_type.Uri then
             P.uri p
           else P.text p)
      in
      place ~map:"phones" ~codec:Contact.Phone.jsont ph (fun id ->
          t.phones <- (id, ph) :: t.phones)
  | "GEO" | "TZ" -> (
      (* RFC 9555 Section 2.8.3: the property joins the Address of its group,
         or the first Address without a group, and otherwise stands alone. *)
      let value = if name = "GEO" then Some (P.uri p) else tz_value p in
      match value with
      | None -> unknown_property t p
      | Some value -> (
          let update (a : Address.t) =
            if name = "GEO" then { a with coordinates = Some value }
            else { a with time_zone = Some value }
          in
          let group_of (a : Address.t) =
            match Unknown.find a.unknown "vCardParams" with
            | Some (Jsont.Object (mems, _)) -> (
                match Jsont.Json.find_mem "group" mems with
                | Some (_, Jsont.String (g, _)) ->
                    Some (String.lowercase_ascii g)
                | _ -> None)
            | _ -> None
          in
          let lacks (a : Address.t) =
            if name = "GEO" then a.coordinates = None else a.time_zone = None
          in
          let same_group (_, (a : Address.t)) =
            lacks a
            && (P.group p <> None || a.components <> None)
            && Option.map String.lowercase_ascii (P.group p) = group_of a
          in
          match List.find_opt same_group (List.rev t.addresses) with
          | Some (id, a) ->
              t.addresses <-
                List.map
                  (fun (i, x) ->
                    if Id.equal i id then (i, update a) else (i, x))
                  t.addresses
          | None ->
              let id = id t "addresses" p in
              let unknown =
                with_name
                  (params_of t ~consumed:[ "PREF"; "VALUE"; "MEDIATYPE" ] p)
                  (String.lowercase_ascii name)
              in
              t.addresses <-
                ( id,
                  update
                    (Address.make ?contexts:(contexts p) ?pref:(pref p) ~unknown
                       ()) )
                :: t.addresses))
  | "CONTACT-URI" | "URL" ->
      let l =
        Resource.Link.make
          ?kind:(if name = "CONTACT-URI" then Some `Contact else None)
          ~uri:(P.uri p) ?media_type:(P.media_type p) ?contexts:(contexts p)
          ?pref:(pref p) ?label:(label t p) ~unknown:(resource_params t p) ()
      in
      place ~map:"links" ~codec:Resource.Link.jsont l (fun id ->
          t.links <- (id, l) :: t.links)
  | "MEMBER" -> t.members <- P.uri p :: t.members
  | "ORG" ->
      let cs = List.map (String.concat ",") (P.structured p) in
      let sort = P.sort_as p in
      let org_name = match cs with "" :: _ | [] -> None | n :: _ -> Some n in
      let units =
        match cs with
        | [] | [ _ ] -> None
        | _ :: us ->
            Some
              (List.mapi
                 (fun i u ->
                   Org.Organization.Org_unit.make
                     ?sort_as:
                       (match List.nth_opt sort (i + 1) with
                       | Some "" | None -> None
                       | s -> s)
                     u)
                 us)
      in
      let sort_as = match sort with "" :: _ | [] -> None | s :: _ -> Some s in
      let org =
        Org.Organization.make ?name:org_name ?units ?sort_as
          ?contexts:(contexts p)
          ~unknown:(params_of t ~consumed:[ "SORT-AS"; "PREF"; "VALUE" ] p)
          ()
      in
      if base then (
        let id =
          match
            Option.bind (P.group p) (fun g ->
                List.assoc_opt (String.lowercase_ascii g) t.org_groups)
          with
          | Some id -> id
          | None -> id t "organizations" p
        in
        t.organizations <- (id, org) :: t.organizations;
        note_language t p;
        record (map_path "organizations" id []))
      else
        Option.iter
          (fun path -> localize path (to_json Org.Organization.jsont org))
          (alternate_path ())
  | "RELATED" ->
      let key =
        if Vcard.Value_type.equal (P.value_type p) Vcard.Value_type.Text then
          P.text p
        else P.uri p
      in
      let unknown =
        params_of t ~consumed:[ "VALUE"; "MEDIATYPE" ] ~known:[] p
      in
      let unknown =
        if Vcard.Value_type.equal (P.value_type p) Vcard.Value_type.Text then
          with_name unknown "related"
        else unknown
      in
      let kinds = relation_kinds p in
      (* TYPE values became the relation set, so vCardParams keeps none. *)
      let unknown =
        params_of t ~consumed:[ "VALUE"; "MEDIATYPE"; "TYPE" ] p |> fun u ->
        match Unknown.find unknown "vCardName" with
        | Some n -> Unknown.add u "vCardName" n
        | None -> u
      in
      t.related <-
        (key, Info.Relation.make ~relation:kinds ~unknown ()) :: t.related
  | "TITLE" | "ROLE" ->
      let kind = if name = "TITLE" then `Title else `Role in
      if base then (
        let id = id t "titles" p in
        let organization_id =
          Option.bind (P.group p) (fun g ->
              List.assoc_opt (String.lowercase_ascii g) t.org_groups)
        in
        t.titles <-
          ( id,
            Org.Title.make ~kind ?organization_id
              ~unknown:(params_of t ~consumed:[ "PREF"; "VALUE" ] p)
              (P.text p) )
          :: t.titles;
        note_language t p;
        record (map_path "titles" id [ "name" ]))
      else
        Option.iter
          (fun path -> localize path (str (P.text p)))
          (alternate_path ())
  | "EXPERTISE" | "HOBBY" | "INTEREST" ->
      let kind =
        match name with
        | "EXPERTISE" -> `Expertise
        | "HOBBY" -> `Hobby
        | _ -> `Interest
      in
      if base then (
        let id = id t "personalInfo" p in
        t.personal <-
          ( id,
            Info.Personal_info.make ?level:(level_of p kind) ?list_as:(index p)
              ?label:(label t p)
              ~unknown:(params_of t ~consumed:[ "LEVEL"; "INDEX"; "VALUE" ] p)
              kind (P.text p) )
          :: t.personal;
        note_language t p;
        record (map_path "personalInfo" id [ "value" ]))
      else
        Option.iter
          (fun path -> localize path (str (P.text p)))
          (alternate_path ())
  | "ORG-DIRECTORY" ->
      let d =
        Resource.Directory.make ~uri:(P.uri p) ?media_type:(P.media_type p)
          ?contexts:(contexts p) ?pref:(pref p) ?label:(label t p)
          ?list_as:(index p)
          ~unknown:(resource_params t ~extra:[ "INDEX" ] p)
          `Directory
      in
      place ~map:"directories" ~codec:Resource.Directory.jsont d (fun id ->
          t.directories <- (id, d) :: t.directories)
  | "CATEGORIES" -> t.keywords <- List.rev_append (P.text_list p) t.keywords
  | "CREATED" -> (
      match timestamp_value p with
      | Some u -> t.created <- Some u
      | None -> unknown_property t p)
  | "REV" -> (
      match timestamp_value p with
      | Some u -> t.updated <- Some u
      | None -> unknown_property t p)
  | "NOTE" ->
      if base then (
        let id = id t "notes" p in
        let author =
          match (P.find_first p "AUTHOR-NAME", P.find_first p "AUTHOR") with
          | None, None -> None
          | name, uri -> Some (Info.Note.Author.make ?name ?uri ())
        in
        let created =
          Option.bind (P.find_first p "CREATED") (fun s ->
              Result.to_option (Vcard.Date.Timestamp.of_string s))
        in
        let created = Option.bind created utc_of_timestamp in
        t.notes <-
          ( id,
            Info.Note.make ?created ?author
              ~unknown:
                (params_of t
                   ~consumed:
                     [ "AUTHOR"; "AUTHOR-NAME"; "CREATED"; "PREF"; "VALUE" ]
                   p)
              (P.text p) )
          :: t.notes;
        note_language t p;
        record (map_path "notes" id [ "note" ]))
      else
        Option.iter
          (fun path -> localize path (str (P.text p)))
          (alternate_path ())
  | "PRODID" -> t.prod_id <- Some (P.text p)
  | "UID" -> t.uid <- Some (text_or_uri p)
  | "KEY" ->
      let k =
        Resource.Crypto_key.make ~uri:(text_or_uri p)
          ?media_type:(P.media_type p) ?contexts:(contexts p) ?pref:(pref p)
          ?label:(label t p) ~unknown:(resource_params t p) ()
      in
      place ~map:"cryptoKeys" ~codec:Resource.Crypto_key.jsont k (fun id ->
          t.keys <- (id, k) :: t.keys)
  | "CALADRURI" ->
      let sa =
        Calendar.Scheduling_address.make ?contexts:(contexts p) ?pref:(pref p)
          ?label:(label t p)
          ~unknown:(params_of t ~consumed:[ "PREF"; "VALUE"; "MEDIATYPE" ] p)
          (P.uri p)
      in
      place ~map:"schedulingAddresses" ~codec:Calendar.Scheduling_address.jsont
        sa (fun id -> t.scheduling <- (id, sa) :: t.scheduling)
  | "CALURI" | "FBURL" ->
      let c =
        Calendar.make ~uri:(P.uri p) ?media_type:(P.media_type p)
          ?contexts:(contexts p) ?pref:(pref p) ?label:(label t p)
          ~unknown:(resource_params t p)
          (if name = "CALURI" then `Calendar else `Free_busy)
      in
      place ~map:"calendars" ~codec:Calendar.jsont c (fun id ->
          t.calendars <- (id, c) :: t.calendars)
  | "X-ABLABEL" -> (
      match P.group p with
      | Some g when List.mem (String.lowercase_ascii g) t.used_labels -> ()
      | _ -> unknown_property t p)
  | "JSPROP" -> (
      match P.find_first p "JSPTR" with
      | None -> unknown_property t p
      | Some ptr -> (
          let ptr =
            if String.starts_with ~prefix:"/" ptr then
              String.sub ptr 1 (String.length ptr - 1)
            else ptr
          in
          match Jsont_bytesrw.decode_string Jsont.json (P.text p) with
          | Ok (Jsont.Null _) -> t.jsprops <- (ptr, Patch.Remove) :: t.jsprops
          | Ok v -> t.jsprops <- (ptr, Patch.Set v) :: t.jsprops
          | Error _ -> unknown_property t p))
  | _ -> unknown_property t p

let labels card =
  List.filter_map
    (fun p ->
      match (P.name p, P.group p) with
      | "X-ABLABEL", Some g -> Some (String.lowercase_ascii g, P.text p)
      | _ -> None)
    (Vcard.properties card)

(* RFC 9555 Section 2.9.6: a TITLE or ROLE in a group with exactly one ORG
   refers to it. The ORG ids are assigned first so that both agree. *)
let org_groups counters used reserved card =
  let orgs = List.filter (fun p -> P.name p = "ORG") (Vcard.properties card) in
  List.filter_map
    (fun p ->
      match P.group p with
      | None -> None
      | Some g ->
          let g = String.lowercase_ascii g in
          if
            List.length
              (List.filter
                 (fun q ->
                   Option.map String.lowercase_ascii (P.group q) = Some g)
                 orgs)
            = 1
          then Some (g, pick counters used reserved "organizations" p)
          else None)
    orgs

let derived_uid card =
  let d =
    Bytes.of_string (Digest.to_hex (Digest.string (Vcard.to_string card)))
  in
  Bytes.set d 12 '3';
  Bytes.set d 16 "89ab".[Char.code (Bytes.get d 16) land 3];
  let d = Bytes.to_string d in
  Printf.sprintf "urn:uuid:%s-%s-%s-%s-%s" (String.sub d 0 8) (String.sub d 8 4)
    (String.sub d 12 4) (String.sub d 16 4) (String.sub d 20 12)

(* RFC 9555 Section 2.7.4 and Section 2.3.11, Figures 3 and 4: the Card's
   language is its LANGUAGE property, or else the dominant language, and that
   only when a property in it has no alternative without a LANGUAGE
   parameter. *)
let card_language card =
  match Vcard.find card "LANGUAGE" with
  | Some p -> Some (P.text p)
  | None -> (
      let carried_by_base p =
        P.language p <> None
        && List.for_all (fun q -> P.language q <> None) (siblings_in card p)
      in
      match dominant card with
      | Some d ->
          Option.bind
            (List.find_opt
               (fun p ->
                 Option.map String.lowercase_ascii (P.language p) = Some d
                 && carried_by_base p)
               (Vcard.properties card))
            P.language
      | None -> None)

(* RFC 9553 Section 1.4.3: no patch key may be a prefix of another. An entry
   that sets an object absorbs the entries below it, such as the phonetics of
   the components of a localized address. *)
let fold_prefixes entries =
  let below key (k, _) = String.starts_with ~prefix:(key ^ "/") k in
  let strip key (k, v) =
    let n = String.length key + 1 in
    (String.sub k n (String.length k - n), v)
  in
  let rec go acc = function
    | [] -> List.rev acc
    | ((key, Patch.Set obj) as e) :: rest -> (
        let inside = List.filter (below key) (List.rev_append acc rest) in
        let without = List.filter (Fun.negate (below key)) in
        match inside with
        | [] -> go (e :: acc) rest
        | _ -> (
            match Patch.apply (Patch.v (List.map (strip key) inside)) obj with
            | Ok obj -> go ((key, Patch.Set obj) :: without acc) (without rest)
            | Error _ -> go (e :: acc) rest))
    | e :: rest -> go (e :: acc) rest
  in
  go [] entries

(* RFC 5646 Section 2.1.1: a tag is case insensitive, and by convention its
   language is lowercase, a script is titlecase and a region is uppercase. *)
let canonical_tag tag =
  match String.split_on_char '-' tag with
  | [] -> tag
  | language :: subtags ->
      let rec go acc = function
        | [] -> List.rev acc
        | sub :: rest when String.length sub = 1 ->
            List.rev_append acc
              (String.lowercase_ascii sub
              :: List.map String.lowercase_ascii rest)
        | sub :: rest ->
            let sub =
              match String.length sub with
              | 4 when String.for_all (fun c -> not (c >= '0' && c <= '9')) sub
                ->
                  String.capitalize_ascii (String.lowercase_ascii sub)
              | 2 -> String.uppercase_ascii sub
              | _ -> String.lowercase_ascii sub
            in
            go (sub :: acc) rest
      in
      String.concat "-" (String.lowercase_ascii language :: go [] subtags)

let opt_list = function [] -> None | l -> Some (List.rev l)

let convert_card ?uid card =
  let counters = Hashtbl.create 16 in
  let used = Hashtbl.create 16 in
  let reserved = reserved_ids card in
  let t =
    {
      card;
      labels = labels card;
      org_groups = org_groups counters used reserved card;
      counters;
      used;
      reserved;
      language = card_language card;
      kind = None;
      members = [];
      prod_id = None;
      related = [];
      created = None;
      updated = None;
      uid = None;
      full = None;
      components = None;
      positions = [];
      used_labels = [];
      speak_unknown = Unknown.empty;
      is_ordered = false;
      default_separator = None;
      dominant = dominant card;
      sort_as = None;
      phonetic_script = None;
      phonetic_system = None;
      name_unknown = Unknown.empty;
      nicknames = [];
      organizations = [];
      gender = None;
      pronouns = [];
      titles = [];
      emails = [];
      services = [];
      phones = [];
      languages = [];
      calendars = [];
      scheduling = [];
      addresses = [];
      keys = [];
      directories = [];
      links = [];
      media = [];
      anniversaries = [];
      keywords = [];
      notes = [];
      personal = [];
      localizations = [];
      props = [];
      jsprops = [];
    }
  in
  let paths = Hashtbl.create 16 in
  (* Base instances are converted before their alternatives, since an
     alternative patches the path its base recorded. *)
  let props = Vcard.properties card in
  let labels, props = List.partition (fun p -> P.name p = "X-ABLABEL") props in
  let bases, alternates = List.partition (is_base t) props in
  let phonetic, plain = List.partition is_phonetic bases in
  List.iter (convert t paths) plain;
  List.iter (convert t paths) phonetic;
  List.iter (convert t paths) alternates;
  List.iter (convert t paths) labels;
  let name =
    if t.full = None && t.components = None && t.phonetic_system = None then
      None
    else
      Some
        (Name.make ?components:t.components ~is_ordered:t.is_ordered
           ?default_separator:t.default_separator ?full:t.full
           ?sort_as:t.sort_as ?phonetic_script:t.phonetic_script
           ?phonetic_system:t.phonetic_system ~unknown:t.name_unknown ())
  in
  let speak_to_as =
    if t.gender = None && t.pronouns = [] then None
    else
      Some
        (Org.Speak_to_as.make ?grammatical_gender:t.gender
           ?pronouns:(opt_list t.pronouns) ~unknown:t.speak_unknown ())
  in
  let localizations =
    match t.localizations with
    | [] -> None
    | ls ->
        Some
          (List.rev_map
             (fun (lang, entries) ->
               (canonical_tag lang, Patch.v (fold_prefixes entries)))
             ls)
  in
  let unknown =
    match t.props with
    | [] -> Unknown.empty
    | ps ->
        Unknown.add Unknown.empty "vCardProps" (Jsont.Json.list (List.rev ps))
  in
  let uid =
    match (t.uid, uid) with
    | Some u, _ -> u
    | None, Some f -> f ()
    | None, None -> derived_uid card
  in
  let c =
    Card.make ?created:t.created ?kind:t.kind
      ?language:(Option.map canonical_tag t.language)
      ?members:(opt_list t.members) ?prod_id:t.prod_id
      ?related_to:(opt_list t.related) ?updated:t.updated ?name
      ?nicknames:(opt_list t.nicknames)
      ?organizations:(opt_list t.organizations) ?speak_to_as
      ?titles:(opt_list t.titles) ?emails:(opt_list t.emails)
      ?online_services:(opt_list t.services) ?phones:(opt_list t.phones)
      ?preferred_languages:(opt_list t.languages)
      ?calendars:(opt_list t.calendars)
      ?scheduling_addresses:(opt_list t.scheduling)
      ?addresses:(opt_list t.addresses) ?crypto_keys:(opt_list t.keys)
      ?directories:(opt_list t.directories) ?links:(opt_list t.links)
      ?media:(opt_list t.media) ?localizations
      ?anniversaries:(opt_list t.anniversaries) ?keywords:(opt_list t.keywords)
      ?notes:(opt_list t.notes) ?personal_info:(opt_list t.personal) ~unknown
      uid
  in
  (* RFC 9555 Section 3.2.1: the JSPROP properties form a PatchObject applied
     after everything else. *)
  match t.jsprops with
  | [] -> Ok c
  | entries ->
      let* patch = Patch.of_list (List.rev entries) in
      let* json = Jsont.Json.encode Card.jsont c in
      let* json = Patch.apply patch json in
      Jsont.Json.decode Card.jsont json
