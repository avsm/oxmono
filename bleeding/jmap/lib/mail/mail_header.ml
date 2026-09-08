(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { name : string; value : string }

let jsont =
  let kind = "EmailHeader" in
  Jsont.Object.map ~kind (fun name value -> { name; value })
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun t -> t.name)
  |> Jsont.Object.mem "value" Jsont.string ~enc:(fun t -> t.value)
  |> Jsont.Object.finish

type address_header =
  [ `From
  | `Sender
  | `Reply_to
  | `To
  | `Cc
  | `Bcc
  | `Resent_from
  | `Resent_sender
  | `Resent_reply_to
  | `Resent_to
  | `Resent_cc
  | `Resent_bcc ]

type message_id_header =
  [ `Message_id | `In_reply_to | `References | `Resent_message_id ]

type date_header = [ `Date | `Resent_date ]

type url_header =
  [ `List_help
  | `List_unsubscribe
  | `List_subscribe
  | `List_post
  | `List_owner
  | `List_archive ]

type text_header = [ `Subject | `Comments | `Keywords ]
type trace_header = [ `Received | `Return_path ]

type standard_header =
  [ address_header
  | message_id_header
  | date_header
  | url_header
  | text_header
  | trace_header ]

type custom_header = [ `Custom of string ]
type any_header = [ standard_header | custom_header ]

let standard_header_to_string : [< standard_header ] -> string = function
  | `From -> "From"
  | `Sender -> "Sender"
  | `Reply_to -> "Reply-To"
  | `To -> "To"
  | `Cc -> "Cc"
  | `Bcc -> "Bcc"
  | `Resent_from -> "Resent-From"
  | `Resent_sender -> "Resent-Sender"
  | `Resent_reply_to -> "Resent-Reply-To"
  | `Resent_to -> "Resent-To"
  | `Resent_cc -> "Resent-Cc"
  | `Resent_bcc -> "Resent-Bcc"
  | `Message_id -> "Message-ID"
  | `In_reply_to -> "In-Reply-To"
  | `References -> "References"
  | `Resent_message_id -> "Resent-Message-ID"
  | `Date -> "Date"
  | `Resent_date -> "Resent-Date"
  | `List_help -> "List-Help"
  | `List_unsubscribe -> "List-Unsubscribe"
  | `List_subscribe -> "List-Subscribe"
  | `List_post -> "List-Post"
  | `List_owner -> "List-Owner"
  | `List_archive -> "List-Archive"
  | `Subject -> "Subject"
  | `Comments -> "Comments"
  | `Keywords -> "Keywords"
  | `Received -> "Received"
  | `Return_path -> "Return-Path"

let standard_header_of_string s : standard_header option =
  match String.lowercase_ascii s with
  | "from" -> Some `From
  | "sender" -> Some `Sender
  | "reply-to" -> Some `Reply_to
  | "to" -> Some `To
  | "cc" -> Some `Cc
  | "bcc" -> Some `Bcc
  | "resent-from" -> Some `Resent_from
  | "resent-sender" -> Some `Resent_sender
  | "resent-reply-to" -> Some `Resent_reply_to
  | "resent-to" -> Some `Resent_to
  | "resent-cc" -> Some `Resent_cc
  | "resent-bcc" -> Some `Resent_bcc
  | "message-id" -> Some `Message_id
  | "in-reply-to" -> Some `In_reply_to
  | "references" -> Some `References
  | "resent-message-id" -> Some `Resent_message_id
  | "date" -> Some `Date
  | "resent-date" -> Some `Resent_date
  | "list-help" -> Some `List_help
  | "list-unsubscribe" -> Some `List_unsubscribe
  | "list-subscribe" -> Some `List_subscribe
  | "list-post" -> Some `List_post
  | "list-owner" -> Some `List_owner
  | "list-archive" -> Some `List_archive
  | "subject" -> Some `Subject
  | "comments" -> Some `Comments
  | "keywords" -> Some `Keywords
  | "received" -> Some `Received
  | "return-path" -> Some `Return_path
  | _ -> None

let any_header_to_string : [< any_header ] -> string = function
  | `Custom s -> s
  | #standard_header as h -> standard_header_to_string h

type form =
  [ `Raw
  | `Text
  | `Addresses
  | `Grouped_addresses
  | `Message_ids
  | `Date
  | `Urls ]

let form_to_string : [< form ] -> string = function
  | `Raw -> ""
  | `Text -> "asText"
  | `Addresses -> "asAddresses"
  | `Grouped_addresses -> "asGroupedAddresses"
  | `Message_ids -> "asMessageIds"
  | `Date -> "asDate"
  | `Urls -> "asURLs"

let form_of_string s : form option =
  match s with
  | "" -> Some `Raw
  | "asText" -> Some `Text
  | "asAddresses" -> Some `Addresses
  | "asGroupedAddresses" -> Some `Grouped_addresses
  | "asMessageIds" -> Some `Message_ids
  | "asDate" -> Some `Date
  | "asURLs" -> Some `Urls
  | _ -> None

type header_property =
  | Raw of { name : string; all : bool }
  | Text of { header : [ text_header | custom_header ]; all : bool }
  | Addresses of { header : [ address_header | custom_header ]; all : bool }
  | Grouped_addresses of {
      header : [ address_header | custom_header ];
      all : bool;
    }
  | Message_ids of {
      header : [ message_id_header | custom_header ];
      all : bool;
    }
  | Date of { header : [ date_header | custom_header ]; all : bool }
  | Urls of { header : [ url_header | custom_header ]; all : bool }

let header_name_of_property : header_property -> string = function
  | Raw { name; _ } -> name
  | Text { header; _ } -> any_header_to_string (header :> any_header)
  | Addresses { header; _ } -> any_header_to_string (header :> any_header)
  | Grouped_addresses { header; _ } ->
      any_header_to_string (header :> any_header)
  | Message_ids { header; _ } -> any_header_to_string (header :> any_header)
  | Date { header; _ } -> any_header_to_string (header :> any_header)
  | Urls { header; _ } -> any_header_to_string (header :> any_header)

let header_property_all : header_property -> bool = function
  | Raw { all; _ }
  | Text { all; _ }
  | Addresses { all; _ }
  | Grouped_addresses { all; _ }
  | Message_ids { all; _ }
  | Date { all; _ }
  | Urls { all; _ } ->
      all

let header_property_form : header_property -> form = function
  | Raw _ -> `Raw
  | Text _ -> `Text
  | Addresses _ -> `Addresses
  | Grouped_addresses _ -> `Grouped_addresses
  | Message_ids _ -> `Message_ids
  | Date _ -> `Date
  | Urls _ -> `Urls

(* The match below enumerates every {!form} constructor, and within each form
   every {!standard_header} category, without a catch-all: adding a form or a
   header category is therefore a compile error here rather than a silent
   rejection of a valid combination. *)
let form_allows_standard_header (form : form) (h : standard_header) =
  match form with
  (* Section 4.1.2.1: the raw form is defined for every header field. *)
  | `Raw -> true
  (* Section 4.1.2.2: asText - Subject, Comments, Keywords. *)
  | `Text -> (
      match h with
      | #text_header -> true
      | #address_header
      | #message_id_header
      | #date_header
      | #url_header
      | #trace_header ->
          false)
  (* Section 4.1.2.3 (asAddresses) and 4.1.2.4 (asGroupedAddresses) share
     the RFC 5322 originator, destination and resent address fields. *)
  | `Addresses | `Grouped_addresses -> (
      match h with
      | #address_header -> true
      | #text_header
      | #message_id_header
      | #date_header
      | #url_header
      | #trace_header ->
          false)
  (* Section 4.1.2.5: asMessageIds - Message-ID, In-Reply-To, References,
     Resent-Message-ID. *)
  | `Message_ids -> (
      match h with
      | #message_id_header -> true
      | #text_header
      | #address_header
      | #date_header
      | #url_header
      | #trace_header ->
          false)
  (* Section 4.1.2.6: asDate - Date, Resent-Date. *)
  | `Date -> (
      match h with
      | #date_header -> true
      | #text_header
      | #address_header
      | #message_id_header
      | #url_header
      | #trace_header ->
          false)
  (* Section 4.1.2.7: asURLs - the RFC 2369 List-* fields. *)
  | `Urls -> (
      match h with
      | #url_header -> true
      | #text_header
      | #address_header
      | #message_id_header
      | #date_header
      | #trace_header ->
          false)

let form_allows_header (form : form) (name : string) =
  match standard_header_of_string name with
  | None -> true
  | Some h -> form_allows_standard_header form h

(* RFC 8621 Section 4.1.3: "{header-field-name}" means any series of one or
   more printable ASCII characters (i.e., characters that have values between
   33 and 126, inclusive), except for colon (:). *)
let valid_header_name name =
  let allowed c = c >= '\x21' && c <= '\x7e' && not (Char.equal c ':') in
  String.length name > 0 && String.for_all allowed name

let form_name (form : form) =
  match form with `Raw -> "raw" | f -> form_to_string f

let check_name fn name =
  if not (valid_header_name name) then
    invalid_arg
      (Printf.sprintf
         "Mail_header.%s: %S is not a header field name; RFC 8621 Section \
          4.1.3 requires one or more printable ASCII characters, 33 to 126, \
          other than colon"
         fn name)

let check fn form name =
  check_name fn name;
  if not (form_allows_header form name) then
    invalid_arg
      (Printf.sprintf "Mail_header.%s: the %s form is not allowed for header %S"
         fn (form_name form) name)

let check_header fn form header = check fn form (any_header_to_string header)

let header_property_to_string prop =
  let name = header_name_of_property prop in
  let form = header_property_form prop in
  check "header_property_to_string" form name;
  let form_suffix = match form_to_string form with "" -> "" | s -> ":" ^ s in
  let all_suffix = if header_property_all prop then ":all" else "" in
  "header:" ^ name ^ form_suffix ^ all_suffix

let header_property_of_form (form : form) ~name ~all : header_property option =
  let std = standard_header_of_string name in
  match std with
  | Some h when not (form_allows_standard_header form h) -> None
  | Some _ | None -> (
      match form with
      | `Raw -> Some (Raw { name; all })
      | `Text ->
          let header : [ text_header | custom_header ] =
            match std with Some (#text_header as h) -> h | _ -> `Custom name
          in
          Some (Text { header; all })
      | `Addresses ->
          let header : [ address_header | custom_header ] =
            match std with
            | Some (#address_header as h) -> h
            | _ -> `Custom name
          in
          Some (Addresses { header; all })
      | `Grouped_addresses ->
          let header : [ address_header | custom_header ] =
            match std with
            | Some (#address_header as h) -> h
            | _ -> `Custom name
          in
          Some (Grouped_addresses { header; all })
      | `Message_ids ->
          let header : [ message_id_header | custom_header ] =
            match std with
            | Some (#message_id_header as h) -> h
            | _ -> `Custom name
          in
          Some (Message_ids { header; all })
      | `Date ->
          let header : [ date_header | custom_header ] =
            match std with Some (#date_header as h) -> h | _ -> `Custom name
          in
          Some (Date { header; all })
      | `Urls ->
          let header : [ url_header | custom_header ] =
            match std with Some (#url_header as h) -> h | _ -> `Custom name
          in
          Some (Urls { header; all }))

(* The raw form is asked for by leaving the suffix out, so an empty one is a
   malformed property name rather than another spelling of it. *)
let parse_property_name s =
  let named n = not (String.equal n "") in
  if not (String.starts_with ~prefix:"header:" s) then None
  else
    let rest = String.sub s 7 (String.length s - 7) in
    match String.split_on_char ':' rest with
    | [ name ] when valid_header_name name -> Some (name, `Raw, false)
    | [ name; "all" ] when valid_header_name name -> Some (name, `Raw, true)
    | [ name; form ] when valid_header_name name && named form -> (
        match form_of_string form with
        | Some form -> Some (name, form, false)
        | None -> None)
    | [ name; form; "all" ] when valid_header_name name && named form -> (
        match form_of_string form with
        | Some form -> Some (name, form, true)
        | None -> None)
    | _ -> None

let header_property_of_string s : header_property option =
  match parse_property_name s with
  | None -> None
  | Some (name, form, all) -> header_property_of_form form ~name ~all

let property_form s =
  Option.map
    (fun p -> (header_property_form p, header_property_all p))
    (header_property_of_string s)

let raw ?(all = false) name =
  check_name "raw" name;
  Raw { name; all }

let text ?(all = false) header =
  check_header "text" `Text header;
  Text { header; all }

let addresses ?(all = false) header =
  check_header "addresses" `Addresses header;
  Addresses { header; all }

let grouped_addresses ?(all = false) header =
  check_header "grouped_addresses" `Grouped_addresses header;
  Grouped_addresses { header; all }

let message_ids ?(all = false) header =
  check_header "message_ids" `Message_ids header;
  Message_ids { header; all }

let date ?(all = false) header =
  check_header "date" `Date header;
  Date { header; all }

let urls ?(all = false) header =
  check_header "urls" `Urls header;
  Urls { header; all }

type header_value =
  | String_single of string option
  | String_all of string list
  | Addresses_single of Mail_address.t list option
  | Addresses_all of Mail_address.t list list
  | Grouped_single of Mail_address.Group.t list option
  | Grouped_all of Mail_address.Group.t list list
  | Date_single of Ptime.t option
  | Date_all of Ptime.t option list
  | Strings_single of string list option
  | Strings_all of string list option list

let pp_value ppf v =
  let comma ppf () = Format.pp_print_string ppf ", " in
  let bar ppf () = Format.pp_print_string ppf " | " in
  let list ?(sep = comma) pp ppf l =
    Format.pp_print_list ~pp_sep:sep pp ppf l
  in
  let absent ppf () = Format.pp_print_string ppf "(absent)" in
  let unparsable ppf () = Format.pp_print_string ppf "(unparsable)" in
  let space ppf () = Format.pp_print_char ppf ' ' in
  let strings ppf l = list ~sep:space Proto_error.pp_escaped ppf l in
  let date ppf t = Format.pp_print_string ppf (Proto_date.to_string t) in
  let group ppf (g : Mail_address.Group.t) =
    (match g.Mail_address.Group.name with
    | Some n -> Format.fprintf ppf "%a: " Proto_error.pp_escaped n
    | None -> ());
    list Mail_address.pp ppf g.Mail_address.Group.addresses
  in
  let opt ?(none = absent) pp ppf = function
    | None -> none ppf ()
    | Some v -> pp ppf v
  in
  match v with
  (* RFC 8621 Section 4.1.3 makes the empty array of an ":all" form the case
     of a field the message does not carry. *)
  | String_all []
  | Addresses_all []
  | Grouped_all []
  | Date_all []
  | Strings_all [] ->
      absent ppf ()
  | String_single s -> opt Proto_error.pp_escaped ppf s
  | String_all l -> list ~sep:bar Proto_error.pp_escaped ppf l
  | Addresses_single a -> opt (list Mail_address.pp) ppf a
  | Addresses_all l -> list ~sep:bar (list Mail_address.pp) ppf l
  | Grouped_single g -> opt (list group) ppf g
  | Grouped_all l -> list ~sep:bar (list group) ppf l
  | Date_single d -> opt date ppf d
  | Date_all l -> list ~sep:bar (opt ~none:unparsable date) ppf l
  | Strings_single s -> opt strings ppf s
  | Strings_all l -> list ~sep:bar (opt ~none:unparsable strings) ppf l

let value_to_string v = Format.asprintf "%a" pp_value v

(* An encoder is reached only through a codec built for the very form the
   value was decoded in, so a mismatch is a programming error rather than
   something to paper over with a null. *)
let enc_mismatch form ~all =
  let name = form_name form in
  let name = if all then name ^ ":all" else name in
  Jsont.Error.msgf Jsont.Meta.none "EmailHeader: value is not in the %s form"
    name

let make_header_value_jsont (form : form) ~all : header_value Jsont.t =
  let mismatch () = enc_mismatch form ~all in
  match (form, all) with
  | (`Raw | `Text), false ->
      Jsont.map
        ~dec:(fun s -> String_single s)
        ~enc:(function String_single s -> s | _ -> mismatch ())
        (Jsont.option Jsont.string)
  | (`Raw | `Text), true ->
      Jsont.map
        ~dec:(fun l -> String_all l)
        ~enc:(function String_all l -> l | _ -> mismatch ())
        (Jsont.list Jsont.string)
  | `Addresses, false ->
      Jsont.map
        ~dec:(fun l -> Addresses_single l)
        ~enc:(function Addresses_single l -> l | _ -> mismatch ())
        (Jsont.option (Jsont.list Mail_address.jsont))
  | `Addresses, true ->
      Jsont.map
        ~dec:(fun l -> Addresses_all l)
        ~enc:(function Addresses_all l -> l | _ -> mismatch ())
        (Jsont.list (Jsont.list Mail_address.jsont))
  | `Grouped_addresses, false ->
      Jsont.map
        ~dec:(fun l -> Grouped_single l)
        ~enc:(function Grouped_single l -> l | _ -> mismatch ())
        (Jsont.option (Jsont.list Mail_address.Group.jsont))
  | `Grouped_addresses, true ->
      Jsont.map
        ~dec:(fun l -> Grouped_all l)
        ~enc:(function Grouped_all l -> l | _ -> mismatch ())
        (Jsont.list (Jsont.list Mail_address.Group.jsont))
  | (`Message_ids | `Urls), false ->
      Jsont.map
        ~dec:(fun l -> Strings_single l)
        ~enc:(function Strings_single l -> l | _ -> mismatch ())
        (Jsont.option (Jsont.list Jsont.string))
  | (`Message_ids | `Urls), true ->
      Jsont.map
        ~dec:(fun l -> Strings_all l)
        ~enc:(function Strings_all l -> l | _ -> mismatch ())
        (Jsont.list (Jsont.option (Jsont.list Jsont.string)))
  | `Date, false ->
      Jsont.map
        ~dec:(fun t -> Date_single t)
        ~enc:(function Date_single t -> t | _ -> mismatch ())
        (Jsont.option Proto_date.jsont)
  | `Date, true ->
      Jsont.map
        ~dec:(fun l -> Date_all l)
        ~enc:(function Date_all l -> l | _ -> mismatch ())
        (Jsont.list (Jsont.option Proto_date.jsont))

let header_value_jsont =
  let codecs form =
    ( make_header_value_jsont form ~all:false,
      make_header_value_jsont form ~all:true )
  in
  let raw = codecs `Raw
  and text = codecs `Text
  and addresses = codecs `Addresses
  and grouped = codecs `Grouped_addresses
  and message_ids = codecs `Message_ids
  and date = codecs `Date
  and urls = codecs `Urls in
  fun ~form ~all ->
    let single, every =
      match form with
      | `Raw -> raw
      | `Text -> text
      | `Addresses -> addresses
      | `Grouped_addresses -> grouped
      | `Message_ids -> message_ids
      | `Date -> date
      | `Urls -> urls
    in
    if all then every else single
