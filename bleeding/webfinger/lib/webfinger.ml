(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** RFC 7033 WebFinger and RFC 7565 acct URI scheme.

    This module implements the WebFinger protocol as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc7033}RFC 7033}, providing
    type-safe JSON Resource Descriptor (JRD) encoding/decoding and an
    HTTP client for WebFinger queries. It also implements the acct URI
    scheme as specified in {{:https://datatracker.ietf.org/doc/html/rfc7565}RFC 7565}.

    {2 References}
    {ul
    {- {{:https://datatracker.ietf.org/doc/html/rfc7033}RFC 7033} - WebFinger}
    {- {{:https://datatracker.ietf.org/doc/html/rfc7565}RFC 7565} - The 'acct' URI Scheme}
    {- {{:https://datatracker.ietf.org/doc/html/rfc6415}RFC 6415} - Web Host Metadata}} *)

let src = Logs.Src.create "webfinger" ~doc:"WebFinger Protocol"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Error Types} *)

type error =
  | Invalid_resource of string
  | Http_error of { status : int; body : string }
  | Json_error of string
  | Https_required
  | Not_found

let pp_error ppf = function
  | Invalid_resource s -> Format.fprintf ppf "Invalid resource: %s" s
  | Http_error { status; body } -> Format.fprintf ppf "HTTP error %d: %s" status body
  | Json_error s -> Format.fprintf ppf "JSON parse error: %s" s
  | Https_required -> Format.fprintf ppf "WebFinger requires HTTPS"
  | Not_found -> Format.fprintf ppf "Resource not found"

let error_to_string e = Format.asprintf "%a" pp_error e

exception Webfinger_error of error

let raise_error e = raise (Webfinger_error e)

(** {1 Acct Module} *)

module Acct = struct
  type t = {
    userpart : string;
    host : string;
  }

  let make ~userpart ~host =
    if userpart = "" then invalid_arg "Acct.make: userpart cannot be empty";
    if host = "" then invalid_arg "Acct.make: host cannot be empty";
    { userpart; host }

  (** Percent-encode a userpart per RFC 7565.

      Per RFC 7565/RFC 3986, unreserved and sub-delims are allowed unencoded.
      We use Uri.pct_encode with a custom component that encodes @ (which is
      not in sub-delims) but allows the standard unreserved and sub-delims. *)
  let pct_encode_userpart s =
    (* Uri.pct_encode with `Userinfo encodes @ but we need it for our format *)
    Uri.pct_encode ~component:`Userinfo s

  let of_string s =
    if not (String.starts_with ~prefix:"acct:" s) then
      Error (Invalid_resource "URI must start with 'acct:'")
    else
      let rest = String.sub s 5 (String.length s - 5) in
      match String.rindex_opt rest '@' with
      | None ->
          Error (Invalid_resource "acct URI must contain '@' separating userpart and host")
      | Some idx ->
          let userpart_encoded = String.sub rest 0 idx in
          let host = String.sub rest (idx + 1) (String.length rest - idx - 1) in
          if userpart_encoded = "" then
            Error (Invalid_resource "userpart cannot be empty")
          else if host = "" then
            Error (Invalid_resource "host cannot be empty")
          else
            let userpart = Uri.pct_decode userpart_encoded in
            Ok { userpart; host = String.lowercase_ascii host }

  let of_string_exn s =
    match of_string s with
    | Ok acct -> acct
    | Error e -> raise_error e

  let to_string t =
    Printf.sprintf "acct:%s@%s" (pct_encode_userpart t.userpart) t.host

  let userpart t = t.userpart
  let host t = t.host

  let equal a b =
    (* Per RFC 3986 Section 6.2.2: case normalization (host) and
       percent-encoding normalization (userpart decoded for comparison) *)
    a.userpart = b.userpart &&
    String.lowercase_ascii a.host = String.lowercase_ascii b.host

  let pp ppf t =
    Format.fprintf ppf "%s" (to_string t)
end

(** {1 Internal helpers} *)

let pp_properties ppf props =
  List.iter (fun (uri, value) ->
    match value with
    | Some v -> Format.fprintf ppf "  %s: %s@," uri v
    | None -> Format.fprintf ppf "  %s: null@," uri
  ) props

(** {1 Internal JSON helpers} *)

module String_map = Map.Make(String)

let properties_jsont : (string * string option) list Jsont.t =
  let inner =
    Jsont.Object.map ~kind:"Properties" Fun.id
    |> Jsont.Object.keep_unknown (Jsont.Object.Mems.string_map (Jsont.option Jsont.string)) ~enc:Fun.id
    |> Jsont.Object.finish
  in
  Jsont.map
    ~dec:(fun m -> List.of_seq (String_map.to_seq m))
    ~enc:(fun l -> String_map.of_list l)
    inner

let titles_jsont : (string * string) list Jsont.t =
  let inner =
    Jsont.Object.map ~kind:"Titles" Fun.id
    |> Jsont.Object.keep_unknown (Jsont.Object.Mems.string_map Jsont.string) ~enc:Fun.id
    |> Jsont.Object.finish
  in
  Jsont.map
    ~dec:(fun m -> List.of_seq (String_map.to_seq m))
    ~enc:(fun l -> String_map.of_list l)
    inner

(** {1 Link Module} *)

module Link = struct
  type t = {
    rel : string;
    type_ : string option;
    href : string option;
    titles : (string * string) list;
    properties : (string * string option) list;
  }

  let make ~rel ?type_ ?href ?(titles = []) ?(properties = []) () =
    { rel; type_; href; titles; properties }

  let rel t = t.rel
  let type_ t = t.type_
  let href t = t.href
  let titles t = t.titles
  let properties t = t.properties

  let title ?(lang = "und") t =
    match List.assoc_opt lang t.titles with
    | Some title -> Some title
    | None -> List.assoc_opt "und" t.titles

  let property ~uri t = List.assoc_opt uri t.properties |> Option.join

  let jsont =
    let make rel type_ href titles properties =
      { rel; type_; href; titles; properties }
    in
    Jsont.Object.map ~kind:"Link" make
    |> Jsont.Object.mem "rel" Jsont.string ~enc:(fun (l : t) -> l.rel)
    |> Jsont.Object.opt_mem "type" Jsont.string ~enc:(fun (l : t) -> l.type_)
    |> Jsont.Object.opt_mem "href" Jsont.string ~enc:(fun (l : t) -> l.href)
    |> Jsont.Object.mem "titles" titles_jsont ~dec_absent:[] ~enc_omit:(fun x -> x = []) ~enc:(fun (l : t) -> l.titles)
    |> Jsont.Object.mem "properties" properties_jsont ~dec_absent:[] ~enc_omit:(fun x -> x = []) ~enc:(fun (l : t) -> l.properties)
    |> Jsont.Object.skip_unknown
    |> Jsont.Object.finish

  let pp ppf t =
    Format.fprintf ppf "@[<v 2>Link:@,rel: %s@," t.rel;
    Option.iter (Format.fprintf ppf "type: %s@,") t.type_;
    Option.iter (Format.fprintf ppf "href: %s@,") t.href;
    if t.titles <> [] then begin
      Format.fprintf ppf "titles:@,";
      List.iter (fun (lang, title) -> Format.fprintf ppf "  %s: %s@," lang title) t.titles
    end;
    if t.properties <> [] then begin
      Format.fprintf ppf "properties:@,";
      pp_properties ppf t.properties
    end;
    Format.fprintf ppf "@]"
end

(** {1 JRD Module} *)

module Jrd = struct
  type t = {
    subject : string option;
    aliases : string list;
    properties : (string * string option) list;
    links : Link.t list;
  }

  let make ?subject ?(aliases = []) ?(properties = []) ?(links = []) () =
    { subject; aliases; properties; links }

  let subject t = t.subject
  let aliases t = t.aliases
  let properties t = t.properties
  let links t = t.links

  let find_link ~rel t = List.find_opt (fun l -> Link.rel l = rel) t.links
  let find_links ~rel t = List.filter (fun l -> Link.rel l = rel) t.links
  let property ~uri t = List.assoc_opt uri t.properties |> Option.join

  let jsont =
    let make subject aliases properties links =
      { subject; aliases; properties; links }
    in
    Jsont.Object.map ~kind:"JRD" make
    |> Jsont.Object.opt_mem "subject" Jsont.string ~enc:(fun (j : t) -> j.subject)
    |> Jsont.Object.mem "aliases" (Jsont.list Jsont.string) ~dec_absent:[] ~enc_omit:(fun x -> x = []) ~enc:(fun (j : t) -> j.aliases)
    |> Jsont.Object.mem "properties" properties_jsont ~dec_absent:[] ~enc_omit:(fun x -> x = []) ~enc:(fun (j : t) -> j.properties)
    |> Jsont.Object.mem "links" (Jsont.list Link.jsont) ~dec_absent:[] ~enc_omit:(fun x -> x = []) ~enc:(fun (j : t) -> j.links)
    |> Jsont.Object.skip_unknown
    |> Jsont.Object.finish

  let of_string s =
    Jsont_bytesrw.decode_string jsont s
    |> Result.map_error (fun e -> Json_error e)

  let to_string t =
    match Jsont_bytesrw.encode_string jsont t with
    | Ok s -> s
    | Error e -> failwith ("JSON encoding error: " ^ e)

  let pp ppf t =
    Format.fprintf ppf "@[<v>";
    Option.iter (Format.fprintf ppf "subject: %s@,") t.subject;
    if t.aliases <> [] then begin
      Format.fprintf ppf "aliases:@,";
      List.iter (Format.fprintf ppf "  - %s@,") t.aliases
    end;
    if t.properties <> [] then begin
      Format.fprintf ppf "properties:@,";
      pp_properties ppf t.properties
    end;
    if t.links <> [] then begin
      Format.fprintf ppf "links:@,";
      List.iter (fun link -> Format.fprintf ppf "  %a@," Link.pp link) t.links
    end;
    Format.fprintf ppf "@]"
end

(** {1 Common Link Relations} *)

module Rel = struct
  let activitypub = "self"
  let openid = "http://openid.net/specs/connect/1.0/issuer"
  let profile = "http://webfinger.net/rel/profile-page"
  let avatar = "http://webfinger.net/rel/avatar"
  let feed = "http://schemas.google.com/g/2010#updates-from"
  let portable_contacts = "http://portablecontacts.net/spec/1.0"
  let oauth_authorization = "http://tools.ietf.org/html/rfc6749#section-3.1"
  let oauth_token = "http://tools.ietf.org/html/rfc6749#section-3.2"
  let subscribe = "http://ostatus.org/schema/1.0/subscribe"
  let salmon = "salmon"
  let magic_key = "magic-public-key"
end

(** {1 URL Construction} *)

let webfinger_url ~resource ?(rels = []) host =
  let base = Printf.sprintf "https://%s/.well-known/webfinger" host in
  let uri = Uri.of_string base in
  let uri = Uri.add_query_param' uri ("resource", resource) in
  let uri = List.fold_left (fun u rel -> Uri.add_query_param' u ("rel", rel)) uri rels in
  Uri.to_string uri

let webfinger_url_acct acct ?(rels = []) () =
  let resource = Acct.to_string acct in
  let host = Acct.host acct in
  webfinger_url ~resource ~rels host

let host_of_resource resource =
  if String.starts_with ~prefix:"acct:" resource then
    Acct.of_string resource |> Result.map Acct.host
  else
    let uri = Uri.of_string resource in
    match Uri.host uri with
    | Some host -> Ok host
    | None -> Error (Invalid_resource "Cannot determine host from resource URI")

(** {1 HTTP Client} *)

let query session ~resource ?(rels = []) () =
  match host_of_resource resource with
  | Error e -> Error e
  | Ok host ->
      let url = webfinger_url ~resource ~rels host in
      Log.info (fun m -> m "WebFinger query: %s" url);
      let headers = Requests.Headers.empty |> Requests.Headers.set `Accept "application/jrd+json" in
      let response = Requests.get session ~headers url in
      let status = Requests.Response.status_code response in
      let body = Eio.Flow.read_all (Requests.Response.body response) in
      if status = 404 then Error Not_found
      else if status >= 400 then Error (Http_error { status; body })
      else Jrd.of_string body

let query_exn session ~resource ?rels () =
  match query session ~resource ?rels () with
  | Ok jrd -> jrd
  | Error e -> raise_error e

let query_acct session acct ?(rels = []) () =
  let resource = Acct.to_string acct in
  let host = Acct.host acct in
  let url = webfinger_url ~resource ~rels host in
  Log.info (fun m -> m "WebFinger query: %s" url);
  let headers = Requests.Headers.empty |> Requests.Headers.set `Accept "application/jrd+json" in
  let response = Requests.get session ~headers url in
  let status = Requests.Response.status_code response in
  let body = Eio.Flow.read_all (Requests.Response.body response) in
  if status = 404 then Error Not_found
  else if status >= 400 then Error (Http_error { status; body })
  else Jrd.of_string body

let query_acct_exn session acct ?rels () =
  match query_acct session acct ?rels () with
  | Ok jrd -> jrd
  | Error e -> raise_error e
