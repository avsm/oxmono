(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP Link header parsing per RFC 8288

    This module parses Link headers for pagination, API discovery, and
    relationship navigation. Per Recommendation #19.

    Link header example:
    {[
      Link: <https://api.example.com/users?page=2>; rel="next",
            <https://api.example.com/users?page=5>; rel="last"
    ]}
*)

let src = Logs.Src.create "requests.link" ~doc:"HTTP Link header parsing"
module Log = (val Logs.src_log src : Logs.LOG)

(** A parsed Link header entry *)
type t = {
  uri : string;  (** The target URI *)
  rel : string option;  (** The relation type (e.g., "next", "prev", "last") *)
  title : string option;  (** Human-readable title *)
  media_type : string option;  (** Media type hint *)
  hreflang : string option;  (** Language hint *)
  params : (string * string) list;  (** Additional parameters *)
}

let make ~uri ?rel ?title ?media_type ?hreflang ?(params=[]) () =
  { uri; rel; title; media_type; hreflang; params }

let uri t = t.uri
let rel t = t.rel
let title t = t.title
let media_type t = t.media_type
let hreflang t = t.hreflang
let params t = t.params

(** Parse a single link value from a Link header segment.
    Format: <uri>; param1=value1; param2="value2" *)
let parse_link_value str =
  let str = String.trim str in

  (* Find the URI in angle brackets *)
  if String.length str = 0 || str.[0] <> '<' then begin
    Log.debug (fun m -> m "Invalid link value, missing '<': %s" str);
    None
  end else begin
    match String.index_opt str '>' with
    | None ->
        Log.debug (fun m -> m "Invalid link value, missing '>': %s" str);
        None
    | Some close_idx ->
        let uri = String.sub str 1 (close_idx - 1) in
        let params_str =
          if close_idx + 1 < String.length str then
            String.sub str (close_idx + 1) (String.length str - close_idx - 1)
          else ""
        in

        (* Parse parameters *)
        let params = String.split_on_char ';' params_str in
        let parsed_params = List.filter_map (fun param ->
          let param = String.trim param in
          if param = "" then None
          else begin
            match String.index_opt param '=' with
            | None -> None
            | Some eq_idx ->
                let key = String.trim (String.sub param 0 eq_idx) in
                let value_raw = String.trim (String.sub param (eq_idx + 1) (String.length param - eq_idx - 1)) in
                (* Remove quotes if present *)
                let value =
                  if String.length value_raw >= 2 &&
                     value_raw.[0] = '"' &&
                     value_raw.[String.length value_raw - 1] = '"' then
                    String.sub value_raw 1 (String.length value_raw - 2)
                  else
                    value_raw
                in
                Some (String.lowercase_ascii key, value)
          end
        ) params in

        (* Extract known parameters *)
        let rel = List.assoc_opt "rel" parsed_params in
        let title = List.assoc_opt "title" parsed_params in
        let media_type = List.assoc_opt "type" parsed_params in
        let hreflang = List.assoc_opt "hreflang" parsed_params in

        (* Keep other params *)
        let other_params = List.filter (fun (k, _) ->
          not (List.mem k ["rel"; "title"; "type"; "hreflang"])
        ) parsed_params in

        Log.debug (fun m -> m "Parsed link: uri=%s rel=%s"
          uri (Option.value rel ~default:"<none>"));

        Some { uri; rel; title; media_type; hreflang; params = other_params }
  end

(** Parse a complete Link header value (may contain multiple links) *)
let parse header_value =
  Log.debug (fun m -> m "Parsing Link header: %s" header_value);

  (* Split on commas, but be careful of commas inside quotes *)
  let rec split_links str acc current in_quotes =
    if String.length str = 0 then
      let final = String.trim current in
      if final = "" then List.rev acc else List.rev (final :: acc)
    else
      let c = str.[0] in
      let rest = String.sub str 1 (String.length str - 1) in
      if c = '"' then
        split_links rest acc (current ^ String.make 1 c) (not in_quotes)
      else if c = ',' && not in_quotes then
        let trimmed = String.trim current in
        if trimmed = "" then
          split_links rest acc "" false
        else
          split_links rest (trimmed :: acc) "" false
      else
        split_links rest acc (current ^ String.make 1 c) in_quotes
  in

  let link_strs = split_links header_value [] "" false in
  List.filter_map parse_link_value link_strs

(** Parse Link header from response headers *)
let from_headers headers =
  match Headers.get `Link headers with
  | None -> []
  | Some value -> parse value

(** Find a link by relation type *)
let find_rel rel links =
  List.find_opt (fun l -> l.rel = Some rel) links

(** Find all links with a specific relation type *)
let filter_rel rel links =
  List.filter (fun l -> l.rel = Some rel) links

(** Get pagination links from headers.
    Returns (first, prev, next, last) where each is optional. *)
let pagination headers =
  let links = from_headers headers in
  let first = find_rel "first" links |> Option.map uri in
  let prev = find_rel "prev" links |> Option.map uri in
  let next = find_rel "next" links |> Option.map uri in
  let last = find_rel "last" links |> Option.map uri in
  (first, prev, next, last)

(** Check if there are more pages (next link exists) *)
let has_next headers =
  let links = from_headers headers in
  Option.is_some (find_rel "next" links)

(** Get the next page URL if available *)
let next_url headers =
  let links = from_headers headers in
  find_rel "next" links |> Option.map uri

(** Get the previous page URL if available *)
let prev_url headers =
  let links = from_headers headers in
  find_rel "prev" links |> Option.map uri

(** Pretty-print a link *)
let pp ppf link =
  Format.fprintf ppf "<%s>" link.uri;
  Option.iter (fun r -> Format.fprintf ppf "; rel=\"%s\"" r) link.rel;
  Option.iter (fun t -> Format.fprintf ppf "; title=\"%s\"" t) link.title;
  Option.iter (fun t -> Format.fprintf ppf "; type=\"%s\"" t) link.media_type;
  Option.iter (fun h -> Format.fprintf ppf "; hreflang=\"%s\"" h) link.hreflang;
  List.iter (fun (k, v) -> Format.fprintf ppf "; %s=\"%s\"" k v) link.params

let to_string link =
  Format.asprintf "%a" pp link
