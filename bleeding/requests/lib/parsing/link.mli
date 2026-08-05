(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP Link header parsing per RFC 8288

    This module parses Link headers commonly used for:
    - API pagination (rel="next", "prev", "first", "last")
    - Resource discovery
    - Relationship navigation

    Per Recommendation #19: Parse Link headers for pagination support.

    {2 Example: Following Pagination}
    {[
      let rec fetch_all_pages session url acc =
        let response = Requests.get session url in
        let data = Response.body response |> Eio.Flow.read_all in
        let acc = data :: acc in
        match Link.next_url (Response.headers response) with
        | Some next -> fetch_all_pages session next acc
        | None -> List.rev acc
    ]}

    {2 Example: Getting All Pagination URLs}
    {[
      let response = Requests.get session "https://api.example.com/items" in
      let (first, prev, next, last) = Link.pagination (Response.headers response) in
      match next with
      | Some url -> Printf.printf "Next page: %s\n" url
      | None -> print_endline "No more pages"
    ]}
*)

(** A parsed Link header entry *)
type t

(** {1 Constructors} *)

val make :
  uri:string ->
  ?rel:string ->
  ?title:string ->
  ?media_type:string ->
  ?hreflang:string ->
  ?params:(string * string) list ->
  unit -> t
(** Create a link value *)

(** {1 Accessors} *)

val uri : t -> string
(** The target URI *)

val rel : t -> string option
(** The relation type (e.g., "next", "prev", "last", "self") *)

val title : t -> string option
(** Human-readable title *)

val media_type : t -> string option
(** Media type hint (from "type" parameter) *)

val hreflang : t -> string option
(** Language hint *)

val params : t -> (string * string) list
(** Additional parameters not covered by standard accessors *)

(** {1 Parsing} *)

val parse : string -> t list
(** Parse a Link header value into a list of links.
    Handles multiple comma-separated links. *)

val from_headers : Headers.t -> t list
(** Extract and parse Link header from response headers.
    Returns empty list if no Link header present. *)

(** {1 Finding Links} *)

val find_rel : string -> t list -> t option
(** Find the first link with a specific relation type *)

val filter_rel : string -> t list -> t list
(** Find all links with a specific relation type *)

(** {1 Pagination Helpers} *)

val pagination : Headers.t -> string option * string option * string option * string option
(** [pagination headers] extracts pagination links.
    Returns [(first, prev, next, last)] where each is optional.

    Looks for links with rel="first", rel="prev", rel="next", rel="last". *)

val has_next : Headers.t -> bool
(** Check if there are more pages (next link exists) *)

val next_url : Headers.t -> string option
(** Get the next page URL if available *)

val prev_url : Headers.t -> string option
(** Get the previous page URL if available *)

(** {1 Formatting} *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a link in Link header format *)

val to_string : t -> string
(** Convert link to string representation *)

(** {1 Logging} *)

val src : Logs.Src.t
(** Log source for link parsing operations *)
