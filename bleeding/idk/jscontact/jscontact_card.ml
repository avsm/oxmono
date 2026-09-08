(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Kind = struct
  type t =
    [ `Individual
    | `Group
    | `Org
    | `Location
    | `Device
    | `Application
    | `Vendor of string ]

  include Jscontact_enum.Make (struct
    type nonrec t = t

    let kind = "kind"

    let to_string = function
      | `Individual -> "individual"
      | `Group -> "group"
      | `Org -> "org"
      | `Location -> "location"
      | `Device -> "device"
      | `Application -> "application"
      | `Vendor s -> s

    let of_string = function
      | "individual" -> `Individual
      | "group" -> `Group
      | "org" -> `Org
      | "location" -> `Location
      | "device" -> `Device
      | "application" -> `Application
      | s -> `Vendor s

    let is_vendor = function `Vendor _ -> true | _ -> false
  end)
end

type t = {
  version : string;
  created : Jscontact_date.Utc.t option;
  kind : Kind.t;
  language : string option;
  members : string list option;
  prod_id : string option;
  related_to : (string * Jscontact_info.Relation.t) list option;
  uid : string;
  updated : Jscontact_date.Utc.t option;
  name : Jscontact_name.t option;
  nicknames : (Jscontact_id.t * Jscontact_name.Nickname.t) list option;
  organizations : (Jscontact_id.t * Jscontact_org.Organization.t) list option;
  speak_to_as : Jscontact_org.Speak_to_as.t option;
  titles : (Jscontact_id.t * Jscontact_org.Title.t) list option;
  emails : (Jscontact_id.t * Jscontact_contact.Email_address.t) list option;
  online_services :
    (Jscontact_id.t * Jscontact_contact.Online_service.t) list option;
  phones : (Jscontact_id.t * Jscontact_contact.Phone.t) list option;
  preferred_languages :
    (Jscontact_id.t * Jscontact_contact.Language_pref.t) list option;
  calendars : (Jscontact_id.t * Jscontact_calendar.t) list option;
  scheduling_addresses :
    (Jscontact_id.t * Jscontact_calendar.Scheduling_address.t) list option;
  addresses : (Jscontact_id.t * Jscontact_address.t) list option;
  crypto_keys : (Jscontact_id.t * Jscontact_resource.Crypto_key.t) list option;
  directories : (Jscontact_id.t * Jscontact_resource.Directory.t) list option;
  links : (Jscontact_id.t * Jscontact_resource.Link.t) list option;
  media : (Jscontact_id.t * Jscontact_resource.Media.t) list option;
  localizations : (string * Jscontact_patch.t) list option;
  anniversaries : (Jscontact_id.t * Jscontact_info.Anniversary.t) list option;
  keywords : string list option;
  notes : (Jscontact_id.t * Jscontact_info.Note.t) list option;
  personal_info : (Jscontact_id.t * Jscontact_info.Personal_info.t) list option;
  unknown : Jscontact_unknown.t;
}

let version_1_0 = "1.0"

let make ?(version = version_1_0) ?created ?(kind = `Individual) ?language
    ?members ?prod_id ?related_to ?updated ?name ?nicknames ?organizations
    ?speak_to_as ?titles ?emails ?online_services ?phones ?preferred_languages
    ?calendars ?scheduling_addresses ?addresses ?crypto_keys ?directories ?links
    ?media ?localizations ?anniversaries ?keywords ?notes ?personal_info
    ?(unknown = Jscontact_unknown.empty) uid =
  {
    version;
    created;
    kind;
    language;
    members;
    prod_id;
    related_to;
    uid;
    updated;
    name;
    nicknames;
    organizations;
    speak_to_as;
    titles;
    emails;
    online_services;
    phones;
    preferred_languages;
    calendars;
    scheduling_addresses;
    addresses;
    crypto_keys;
    directories;
    links;
    media;
    localizations;
    anniversaries;
    keywords;
    notes;
    personal_info;
    unknown;
  }

(* Structural equality would compare the Jsont.Meta.t of every unknown member,
   that is the source location and whitespace a decode recorded, and the order
   of the members within it. Each contained type knows how to compare itself,
   so the comparison is spelled out property by property. *)
let eq_ids eq =
  Option.equal (Jscontact_json.Map.equal ~key:Jscontact_id.compare eq)

let eq_keyed eq = Option.equal (Jscontact_json.Map.equal ~key:String.compare eq)

(* RFC 9553 Section 1.4.1 types [members] and [keywords] String[Boolean], an
   unordered set, so their equality ignores order as the Id maps do. *)
let eq_strings a b =
  Option.equal (fun a b ->
      List.equal String.equal
        (List.sort String.compare a)
        (List.sort String.compare b)) a b

let equal a b =
  String.equal a.version b.version
  && Option.equal Jscontact_date.Utc.equal a.created b.created
  && Kind.equal a.kind b.kind
  && Option.equal String.equal a.language b.language
  && eq_strings a.members b.members
  && Option.equal String.equal a.prod_id b.prod_id
  && eq_keyed Jscontact_info.Relation.equal a.related_to b.related_to
  && String.equal a.uid b.uid
  && Option.equal Jscontact_date.Utc.equal a.updated b.updated
  && Option.equal Jscontact_name.equal a.name b.name
  && eq_ids Jscontact_name.Nickname.equal a.nicknames b.nicknames
  && eq_ids Jscontact_org.Organization.equal a.organizations b.organizations
  && Option.equal Jscontact_org.Speak_to_as.equal a.speak_to_as b.speak_to_as
  && eq_ids Jscontact_org.Title.equal a.titles b.titles
  && eq_ids Jscontact_contact.Email_address.equal a.emails b.emails
  && eq_ids Jscontact_contact.Online_service.equal a.online_services
       b.online_services
  && eq_ids Jscontact_contact.Phone.equal a.phones b.phones
  && eq_ids Jscontact_contact.Language_pref.equal a.preferred_languages
       b.preferred_languages
  && eq_ids Jscontact_calendar.equal a.calendars b.calendars
  && eq_ids Jscontact_calendar.Scheduling_address.equal a.scheduling_addresses
       b.scheduling_addresses
  && eq_ids Jscontact_address.equal a.addresses b.addresses
  && eq_ids Jscontact_resource.Crypto_key.equal a.crypto_keys b.crypto_keys
  && eq_ids Jscontact_resource.Directory.equal a.directories b.directories
  && eq_ids Jscontact_resource.Link.equal a.links b.links
  && eq_ids Jscontact_resource.Media.equal a.media b.media
  && eq_keyed Jscontact_patch.equal a.localizations b.localizations
  && eq_ids Jscontact_info.Anniversary.equal a.anniversaries b.anniversaries
  && eq_strings a.keywords b.keywords
  && eq_ids Jscontact_info.Note.equal a.notes b.notes
  && eq_ids Jscontact_info.Personal_info.equal a.personal_info b.personal_info
  && Jscontact_unknown.equal a.unknown b.unknown

let pp ppf c = Format.fprintf ppf "@[%s (%a)@]" c.uid Kind.pp c.kind

let ctor version created kind language members prod_id related_to uid updated
    name nicknames organizations speak_to_as titles emails online_services
    phones preferred_languages calendars scheduling_addresses addresses
    crypto_keys directories links media localizations anniversaries keywords
    notes personal_info unknown =
  {
    version;
    created;
    kind;
    language;
    members;
    prod_id;
    related_to;
    uid;
    updated;
    name;
    nicknames;
    organizations;
    speak_to_as;
    titles;
    emails;
    online_services;
    phones;
    preferred_languages;
    calendars;
    scheduling_addresses;
    addresses;
    crypto_keys;
    directories;
    links;
    media;
    localizations;
    anniversaries;
    keywords;
    notes;
    personal_info;
    unknown;
  }

let id_map = Jscontact_json.Map.of_id

(* The members between version and uid, and those after uid, are shared by the
   whole and the partial codec below. *)
let mems_before_uid m =
  m
  |> Jsont.Object.opt_mem "created" Jscontact_date.Utc.jsont ~enc:(fun t ->
      t.created)
  |> Jsont.Object.mem "kind" Kind.jsont ~dec_absent:(fun () -> `Individual)
       ~enc:(fun t -> t.kind)
       ~enc_omit:(fun k -> Kind.equal k `Individual)
  |> Jsont.Object.opt_mem "language" Jsont.string ~enc:(fun t -> t.language)
  |> Jsont.Object.opt_mem "members"
       (Jscontact_json.Map.string_set ~kind:"members") ~enc:(fun t -> t.members)
  |> Jsont.Object.opt_mem "prodId" Jsont.string ~enc:(fun t -> t.prod_id)
  |> Jsont.Object.opt_mem "relatedTo"
       (Jscontact_json.Map.of_string Jscontact_info.Relation.jsont)
       ~enc:(fun t -> t.related_to)

let mems_after_uid m =
  m
  |> Jsont.Object.opt_mem "updated" Jscontact_date.Utc.jsont ~enc:(fun t ->
      t.updated)
  |> Jsont.Object.opt_mem "name" Jscontact_name.jsont ~enc:(fun t -> t.name)
  |> Jsont.Object.opt_mem "nicknames" (id_map Jscontact_name.Nickname.jsont)
       ~enc:(fun t -> t.nicknames)
  |> Jsont.Object.opt_mem "organizations"
       (id_map Jscontact_org.Organization.jsont) ~enc:(fun t -> t.organizations)
  |> Jsont.Object.opt_mem "speakToAs" Jscontact_org.Speak_to_as.jsont
       ~enc:(fun t -> t.speak_to_as)
  |> Jsont.Object.opt_mem "titles" (id_map Jscontact_org.Title.jsont)
       ~enc:(fun t -> t.titles)
  |> Jsont.Object.opt_mem "emails"
       (id_map Jscontact_contact.Email_address.jsont) ~enc:(fun t -> t.emails)
  |> Jsont.Object.opt_mem "onlineServices"
       (id_map Jscontact_contact.Online_service.jsont) ~enc:(fun t ->
         t.online_services)
  |> Jsont.Object.opt_mem "phones" (id_map Jscontact_contact.Phone.jsont)
       ~enc:(fun t -> t.phones)
  |> Jsont.Object.opt_mem "preferredLanguages"
       (id_map Jscontact_contact.Language_pref.jsont) ~enc:(fun t ->
         t.preferred_languages)
  |> Jsont.Object.opt_mem "calendars" (id_map Jscontact_calendar.jsont)
       ~enc:(fun t -> t.calendars)
  |> Jsont.Object.opt_mem "schedulingAddresses"
       (id_map Jscontact_calendar.Scheduling_address.jsont) ~enc:(fun t ->
         t.scheduling_addresses)
  |> Jsont.Object.opt_mem "addresses" (id_map Jscontact_address.jsont)
       ~enc:(fun t -> t.addresses)
  |> Jsont.Object.opt_mem "cryptoKeys"
       (id_map Jscontact_resource.Crypto_key.jsont) ~enc:(fun t ->
         t.crypto_keys)
  |> Jsont.Object.opt_mem "directories"
       (id_map Jscontact_resource.Directory.jsont) ~enc:(fun t -> t.directories)
  |> Jsont.Object.opt_mem "links" (id_map Jscontact_resource.Link.jsont)
       ~enc:(fun t -> t.links)
  |> Jsont.Object.opt_mem "media" (id_map Jscontact_resource.Media.jsont)
       ~enc:(fun t -> t.media)
  |> Jsont.Object.opt_mem "localizations"
       (Jscontact_json.Map.of_string Jscontact_patch.jsont) ~enc:(fun t ->
         t.localizations)
  |> Jsont.Object.opt_mem "anniversaries"
       (id_map Jscontact_info.Anniversary.jsont) ~enc:(fun t -> t.anniversaries)
  |> Jsont.Object.opt_mem "keywords"
       (Jscontact_json.Map.string_set ~kind:"keywords") ~enc:(fun t ->
         t.keywords)
  |> Jsont.Object.opt_mem "notes" (id_map Jscontact_info.Note.jsont)
       ~enc:(fun t -> t.notes)
  |> Jsont.Object.opt_mem "personalInfo"
       (id_map Jscontact_info.Personal_info.jsont) ~enc:(fun t ->
         t.personal_info)
  |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t -> t.unknown)

let jsont =
  Jsont.Object.map ~kind:"Card" (fun () -> ctor)
  |> Jscontact_json.type_mem_required "Card"
  |> Jsont.Object.mem "version" Jsont.string ~enc:(fun t -> t.version)
  |> mems_before_uid
  |> Jsont.Object.mem "uid" Jsont.string ~enc:(fun t -> t.uid)
  |> mems_after_uid |> Jsont.Object.finish

(* RFC 8620 Section 5.1 lets a JMAP /get name the properties it wants, and a
   server then returns those alone. A ContactCard fetched that way carries
   neither @type nor version nor uid, though Section 2.1 of RFC 9553 makes all
   three mandatory in a whole Card. *)
let partial_jsont =
  Jsont.Object.map ~kind:"Card" (fun () -> ctor)
  |> Jscontact_json.type_mem_partial "Card"
  |> Jsont.Object.mem "version" Jsont.string ~dec_absent:(fun () -> version_1_0)
       ~enc:(fun t -> t.version)
  |> mems_before_uid
  |> Jsont.Object.mem "uid" Jsont.string ~dec_absent:(fun () -> "")
       ~enc:(fun t -> t.uid)
       ~enc_omit:(fun uid -> String.equal uid "")
  |> mems_after_uid |> Jsont.Object.finish

(* Section 1.9.1: jsversion = 1*DIGIT "." 1*DIGIT. Section 2.1.2 requires the
   value to be an IANA-registered version, and Table 1 registers major version
   1 alone. Section 1.9 makes a differing minor version additive and a
   differing major version backwards incompatible, so a later minor version is
   accepted and a later major one is not. *)
let validate_version v =
  let digits s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s in
  match String.split_on_char '.' v with
  | [ major; minor ] when digits major && digits minor ->
      if String.equal major "1" then Ok v
      else
        Jscontact_valid.error
          "version: %S names JSContact major version %s, and RFC 9553 \
           registers major version 1 alone"
          v major
  | _ ->
      Jscontact_valid.error
        "version: %S is not a JSContact version, which is a major and a minor \
         number separated by a full stop"
        v

(* Section 2.7.1: "A patch MUST NOT target the localizations property." *)
let check_targets (tag, patch) =
  let targets_localizations (key, _) =
    String.equal key "localizations"
    || String.starts_with ~prefix:"localizations/" key
  in
  Jscontact_valid.check
    (not (List.exists targets_localizations (Jscontact_patch.to_list patch)))
    "localizations: the patch for %S targets the localizations property" tag

let validate_localization (tag, patch) =
  Jscontact_valid.(
    let* _ = in_ "localizations" (Jscontact_language.validate tag) in
    let* () = check_targets (tag, patch) in
    let* _ = in_ "localizations" (Jscontact_patch.validate patch) in
    ok (tag, patch))

let validate c =
  Jscontact_valid.(
    let* _ = validate_version c.version in
    let* () = check (c.uid <> "") "uid: a Card uid must not be empty" in
    let* () =
      match c.prod_id with
      | Some "" -> error "prodId: must be at least one character long"
      | _ -> ok ()
    in
    let* _ = Kind.validate c.kind in
    let* () =
      match c.members with
      | Some _ when not (Kind.equal c.kind `Group) ->
          error "members: is set but kind is %a, not \"group\"" Kind.pp c.kind
      | _ -> ok ()
    in
    let* _ = in_ "language" (opt Jscontact_language.validate c.language) in
    let* _ =
      in_ "localizations"
        (opt (fun l -> list validate_localization l) c.localizations)
    in
    let* _ =
      in_ "relatedTo"
        (opt (string_entries Jscontact_info.Relation.validate) c.related_to)
    in
    let* _ = in_ "name" (opt Jscontact_name.validate c.name) in
    let* _ =
      in_ "nicknames"
        (opt (entries Jscontact_name.Nickname.validate) c.nicknames)
    in
    let* _ =
      in_ "organizations"
        (opt (entries Jscontact_org.Organization.validate) c.organizations)
    in
    let* _ =
      in_ "speakToAs" (opt Jscontact_org.Speak_to_as.validate c.speak_to_as)
    in
    let* _ =
      in_ "titles" (opt (entries Jscontact_org.Title.validate) c.titles)
    in
    let* _ =
      in_ "emails"
        (opt (entries Jscontact_contact.Email_address.validate) c.emails)
    in
    let* _ =
      in_ "onlineServices"
        (opt
           (entries Jscontact_contact.Online_service.validate)
           c.online_services)
    in
    let* _ =
      in_ "phones" (opt (entries Jscontact_contact.Phone.validate) c.phones)
    in
    let* _ =
      in_ "preferredLanguages"
        (opt
           (entries Jscontact_contact.Language_pref.validate)
           c.preferred_languages)
    in
    let* _ =
      in_ "calendars" (opt (entries Jscontact_calendar.validate) c.calendars)
    in
    let* _ =
      in_ "schedulingAddresses"
        (opt
           (entries Jscontact_calendar.Scheduling_address.validate)
           c.scheduling_addresses)
    in
    let* _ =
      in_ "addresses" (opt (entries Jscontact_address.validate) c.addresses)
    in
    let* _ =
      in_ "cryptoKeys"
        (opt (entries Jscontact_resource.Crypto_key.validate) c.crypto_keys)
    in
    let* _ =
      in_ "directories"
        (opt (entries Jscontact_resource.Directory.validate) c.directories)
    in
    let* _ =
      in_ "links" (opt (entries Jscontact_resource.Link.validate) c.links)
    in
    let* _ =
      in_ "media" (opt (entries Jscontact_resource.Media.validate) c.media)
    in
    let* _ =
      in_ "anniversaries"
        (opt (entries Jscontact_info.Anniversary.validate) c.anniversaries)
    in
    let* _ = in_ "notes" (opt (entries Jscontact_info.Note.validate) c.notes) in
    let* _ =
      in_ "personalInfo"
        (opt (entries Jscontact_info.Personal_info.validate) c.personal_info)
    in
    let* _ =
      in_ "Card" (Jscontact_unknown.validate ~in_type:"Card" c.unknown)
    in
    ok c)

let localize c ~language =
  match c.localizations with
  | None -> Ok None
  | Some locs -> (
      match List.assoc_opt language locs with
      | None -> Ok None
      | Some patch -> (
          (* Checked here as well as in validate, since a caller may localize
             a Card it never validated. Patch.apply checks the patch itself. *)
          match check_targets (language, patch) with
          | Error msg -> Error msg
          | Ok _ -> (
              let base = { c with localizations = None } in
              match Jsont.Json.encode jsont base with
              | Error msg -> Error msg
              | Ok json -> (
                  match Jscontact_patch.apply patch json with
                  | Error msg -> Error msg
                  | Ok json -> (
                      match Jsont.Json.decode jsont json with
                      | Error msg -> Error msg
                      | Ok c -> Ok (Some { c with language = Some language }))))
          ))
