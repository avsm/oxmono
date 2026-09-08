(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type cardinality = One | At_most_one | One_or_more | Many

type entry = {
  name : string;
  value_type : Vcard_value_type.t;
  cardinality : cardinality;
  reference : string;
}

let e name value_type cardinality reference =
  { name; value_type; cardinality; reference }

open Vcard_value_type

let all =
  [
    e "BEGIN" Text One "RFC 6350, Section 6.1.1";
    e "END" Text One "RFC 6350, Section 6.1.2";
    e "SOURCE" Uri Many "RFC 6350, Section 6.1.3";
    e "KIND" Text At_most_one "RFC 6350, Section 6.1.4";
    e "XML" Text Many "RFC 6350, Section 6.1.5";
    e "FN" Text One_or_more "RFC 6350, Section 6.2.1";
    e "N" Text At_most_one "RFC 6350, Section 6.2.2";
    e "NICKNAME" Text Many "RFC 6350, Section 6.2.3";
    e "PHOTO" Uri Many "RFC 6350, Section 6.2.4";
    e "BDAY" Date_and_or_time At_most_one "RFC 6350, Section 6.2.5";
    e "ANNIVERSARY" Date_and_or_time At_most_one "RFC 6350, Section 6.2.6";
    e "GENDER" Text At_most_one "RFC 6350, Section 6.2.7";
    e "ADR" Text Many "RFC 6350, Section 6.3.1";
    e "TEL" Text Many "RFC 6350, Section 6.4.1";
    e "EMAIL" Text Many "RFC 6350, Section 6.4.2";
    e "IMPP" Uri Many "RFC 6350, Section 6.4.3";
    e "LANG" Language_tag Many "RFC 6350, Section 6.4.4";
    e "TZ" Text Many "RFC 6350, Section 6.5.1";
    e "GEO" Uri Many "RFC 6350, Section 6.5.2";
    e "TITLE" Text Many "RFC 6350, Section 6.6.1";
    e "ROLE" Text Many "RFC 6350, Section 6.6.2";
    e "LOGO" Uri Many "RFC 6350, Section 6.6.3";
    e "ORG" Text Many "RFC 6350, Section 6.6.4";
    e "MEMBER" Uri Many "RFC 6350, Section 6.6.5";
    e "RELATED" Uri Many "RFC 6350, Section 6.6.6";
    e "CATEGORIES" Text Many "RFC 6350, Section 6.7.1";
    e "NOTE" Text Many "RFC 6350, Section 6.7.2";
    e "PRODID" Text At_most_one "RFC 6350, Section 6.7.3";
    e "REV" Timestamp At_most_one "RFC 6350, Section 6.7.4";
    e "SOUND" Uri Many "RFC 6350, Section 6.7.5";
    e "UID" Uri At_most_one "RFC 6350, Section 6.7.6";
    e "CLIENTPIDMAP" Text Many "RFC 6350, Section 6.7.7";
    e "URL" Uri Many "RFC 6350, Section 6.7.8";
    e "VERSION" Text One "RFC 6350, Section 6.7.9";
    e "KEY" Uri Many "RFC 6350, Section 6.8.1";
    e "FBURL" Uri Many "RFC 6350, Section 6.9.1";
    e "CALADRURI" Uri Many "RFC 6350, Section 6.9.2";
    e "CALURI" Uri Many "RFC 6350, Section 6.9.3";
    e "BIRTHPLACE" Text At_most_one "RFC 6474, Section 2.1";
    e "DEATHPLACE" Text At_most_one "RFC 6474, Section 2.2";
    e "DEATHDATE" Date_and_or_time At_most_one "RFC 6474, Section 2.3";
    e "EXPERTISE" Text Many "RFC 6715, Section 2.1";
    e "HOBBY" Text Many "RFC 6715, Section 2.2";
    e "INTEREST" Text Many "RFC 6715, Section 2.3";
    e "ORG-DIRECTORY" Uri Many "RFC 6715, Section 2.4";
    e "CONTACT-URI" Uri Many "RFC 8605, Section 2.1";
    e "CREATED" Timestamp At_most_one "RFC 9554, Section 3.1";
    e "GRAMGENDER" Text Many "RFC 9554, Section 3.2";
    e "LANGUAGE" Language_tag At_most_one "RFC 9554, Section 3.3";
    e "PRONOUNS" Text Many "RFC 9554, Section 3.4";
    e "SOCIALPROFILE" Uri Many "RFC 9554, Section 3.5";
    e "JSPROP" Text Many "RFC 9555, Section 3.2.1";
  ]

let find name =
  let name = String.uppercase_ascii name in
  List.find_opt (fun e -> String.equal e.name name) all

let value_type name =
  match find name with Some e -> e.value_type | None -> Text

let cardinality name =
  match find name with Some e -> e.cardinality | None -> Many
