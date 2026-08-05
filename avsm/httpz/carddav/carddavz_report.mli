(** CardDAV REPORT handling.

    Supports the two report types from
    {{:https://datatracker.ietf.org/doc/html/rfc6352}RFC 6352}:
    - [addressbook-query] — filter + property request
    - [addressbook-multiget] — specific href list *)

(** {1 Types} *)

type text_match = {
  collation : string;
  match_type : string;
  value : string;
}

type prop_filter = {
  name : string;
  text_match : text_match option;
}

type address_filter = {
  prop_filters : prop_filter list;
}

type report =
  | Addressbook_query of {
      filter : address_filter;
      props : Webdavz.Xml.fqname list;
    }
  | Addressbook_multiget of {
      hrefs : string list;
      props : Webdavz.Xml.fqname list;
    }

(** {1 Parsing} *)

val parse_report : string -> report option
(** [parse_report xml] parses a REPORT request body. *)

(** {1 Filtering} *)

val vcard_matches_filter : address_filter -> Carddavz_vcard.t -> bool
(** [vcard_matches_filter filter vcard] returns [true] if the vCard
    matches all prop-filters. *)
