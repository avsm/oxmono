(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let registered =
  [
    "address";
    "addresses";
    "anniversaries";
    "author";
    "calendars";
    "calendarScale";
    "components";
    "contexts";
    "coordinates";
    "countryCode";
    "created";
    "cryptoKeys";
    "date";
    "day";
    "defaultSeparator";
    "directories";
    "emails";
    "features";
    "full";
    "grammaticalGender";
    "isOrdered";
    "keywords";
    "kind";
    "label";
    "language";
    "level";
    "links";
    "listAs";
    "localizations";
    "media";
    "mediaType";
    "members";
    "month";
    "name";
    "nicknames";
    "note";
    "notes";
    "number";
    "onlineServices";
    "organizationId";
    "organizations";
    "personalInfo";
    "phones";
    "phonetic";
    "phoneticScript";
    "phoneticSystem";
    "place";
    "pref";
    "preferredLanguages";
    "prodId";
    "pronouns";
    "relatedTo";
    "relation";
    "schedulingAddresses";
    "service";
    "sortAs";
    "speakToAs";
    "timeZone";
    "titles";
    "@type";
    "uid";
    "units";
    "updated";
    "uri";
    "user";
    "utc";
    "value";
    "vCardName";
    "vCardParams";
    "vCardProps";
    "version";
    "year";
  ]

let folded_names = List.map String.lowercase_ascii registered
let reserved = [ "extra" ]

let reserved_in =
  [ ("Card", [ "id"; "addressBookIds" ]); ("Media", [ "blobId" ]) ]

let reserved_for in_type =
  match in_type with
  | None -> reserved
  | Some t -> reserved @ Option.value ~default:[] (List.assoc_opt t reserved_in)

let is_reserved ?in_type s = List.mem s (reserved_for in_type)
let is_registered s = List.mem s registered

let is_registrable_name s =
  let len = String.length s in
  if len = 0 then false
  else
    let is_char c =
      (c >= 'A' && c <= 'Z')
      || (c >= 'a' && c <= 'z')
      || (c >= '0' && c <= '9')
      || c = '@'
    in
    let first = s.[0] in
    ((first >= 'a' && first <= 'z') || first = '@') && String.for_all is_char s

let validate_property_name ?in_type s =
  let folded = String.lowercase_ascii s in
  if is_reserved ?in_type s then
    Jscontact_valid.error
      "property name %S is reserved by RFC 9553 Section 1.7.3 and must not be \
       set on any JSContact object"
      s
  else if is_registered s || Jscontact_vendor.is_extension s then Ok s
  else if
    List.mem folded folded_names
    || List.exists
         (fun r -> String.equal folded (String.lowercase_ascii r))
         (reserved_for in_type)
  then
    Jscontact_valid.error
      "property name %S differs only in case from a name RFC 9553 registers or \
       reserves, which Section 1.7.1 makes invalid"
      s
  else if is_registrable_name s then Ok s
  else
    Jscontact_valid.error
      "property name %S is neither an IANA-registrable name nor a \
       vendor-specific one"
      s
