(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type standard =
  [ `Seen
  | `Flagged
  | `Answered
  | `Draft
  | `Forwarded
  | `Phishing
  | `Junk
  | `NotJunk ]

type extended = Mail_flag.Keyword.extended
type flag_bits = Mail_flag.Keyword.flag_bit
type t = [ standard | extended | flag_bits | `Custom of string ]

let to_mail_flag (k : t) : Mail_flag.Keyword.t = (k :> Mail_flag.Keyword.t)
let to_string (k : t) = Mail_flag.Keyword.to_string (to_mail_flag k)

let of_string s : t =
  if String.length s = 0 || s.[0] <> '$' then `Custom s
  else
    match Mail_flag.Keyword.of_string s with
    | `Deleted -> `Custom s
    | #t as k -> k

let of_mail_flag : Mail_flag.Keyword.t -> t option = function
  | `Deleted -> None
  | #t as k -> Some k

let of_mail_flag_list ks = List.filter_map of_mail_flag ks
let key k = String.lowercase_ascii (to_string k)
let equal a b = String.equal (key a) (key b)
let compare a b = String.compare (key a) (key b)
let pp ppf k = Proto_error.pp_escaped ppf (to_string k)

(* RFC 8621 Section 4.1.1: a keyword "MUST NOT include any of these
   characters:

                           ( ) { ] % * " \
*)
let excluded_char = function
  | '(' | ')' | '{' | ']' | '%' | '*' | '"' | '\\' -> true
  | _ -> false

let validate (k : t) : (t, string) result =
  let s = to_string k in
  let len = String.length s in
  if len = 0 then Error "a keyword must be at least 1 character"
  else if len > 255 then
    Error
      (Printf.sprintf "a keyword must be at most 255 characters, but %S is %d" s
         len)
  else
    let rec check i =
      if i >= len then Ok k
      else
        let c = s.[i] in
        if c < '\x21' || c > '\x7e' then
          Error
            (Printf.sprintf
               "invalid character %C at position %d of keyword %S: a keyword \
                is ASCII %%x21-%%x7e"
               c i s)
        else if excluded_char c then
          Error (Printf.sprintf "keyword %S must not contain %C" s c)
        else check (i + 1)
    in
    check 0

let validate_exn k =
  match validate k with
  | Ok k -> k
  | Error msg -> invalid_arg ("Mail_keyword: " ^ msg)

let check_unique ks =
  let seen = Hashtbl.create (List.length ks) in
  List.iter
    (fun k ->
      let k = validate_exn k in
      let folded = key k in
      if Hashtbl.mem seen folded then
        invalid_arg
          (Printf.sprintf "Mail_keyword: duplicate keyword %S" (to_string k))
      else Hashtbl.add seen folded ())
    ks

let of_list ks =
  check_unique ks;
  List.map (fun k -> (k, true)) ks

(* Jsont_bytesrw.encode_string catches Jsont.Error alone, so an encoder must
   never report a malformed keyword with Invalid_argument. *)
let validate_jsont k =
  match validate k with
  | Ok k -> k
  | Error msg -> Jsont.Error.msgf Jsont.Meta.none "Keyword: %s" msg

let jsont =
  Jsont.map ~kind:"Keyword" ~dec:of_string
    ~enc:(fun k -> to_string (validate_jsont k))
    Jsont.string

let map_jsont =
  let dec entries =
    let keywords = List.map (fun (s, b) -> (of_string s, b)) entries in
    let seen = Hashtbl.create (List.length keywords) in
    List.iter
      (fun (k, value) ->
        if not value then
          Jsont.Error.msgf Jsont.Meta.none
            "Keywords: keyword %S is mapped to false" (to_string k);
        let folded = key k in
        if Hashtbl.mem seen folded then
          Jsont.Error.msgf Jsont.Meta.none
            "Keywords: duplicate case-insensitive keyword %S" (to_string k);
        Hashtbl.add seen folded ())
      keywords;
    keywords
  in
  let enc entries =
    let seen = Hashtbl.create (List.length entries) in
    List.map
      (fun (k, value) ->
        if not value then
          Jsont.Error.msgf Jsont.Meta.none
            "Keywords: keyword %S is mapped to false" (to_string k);
        let s = to_string (validate_jsont k) in
        let folded = String.lowercase_ascii s in
        if Hashtbl.mem seen folded then
          Jsont.Error.msgf Jsont.Meta.none "Keywords: duplicate keyword %S" s;
        Hashtbl.add seen folded ();
        (s, true))
      entries
  in
  Jsont.map ~kind:"Keywords" ~dec ~enc Proto_json_map.string_to_bool

type flag_color = Mail_flag.Flag_color.t

let flag_color_of_keywords ks =
  let bit = function #flag_bits as b -> Some b | _ -> None in
  let bits = List.filter_map bit ks in
  Mail_flag.Flag_color.of_keywords bits

let flag_color_to_keywords c = (Mail_flag.Flag_color.to_keywords c :> t list)
