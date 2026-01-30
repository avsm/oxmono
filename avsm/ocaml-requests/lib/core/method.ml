(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "requests.method" ~doc:"HTTP Methods"
module Log = (val Logs.src_log src : Logs.LOG)

type t = [
  | `GET
  | `POST
  | `PUT
  | `DELETE
  | `HEAD
  | `OPTIONS
  | `PATCH
  | `CONNECT
  | `TRACE
  | `Other of string
]

let to_string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `DELETE -> "DELETE"
  | `HEAD -> "HEAD"
  | `OPTIONS -> "OPTIONS"
  | `PATCH -> "PATCH"
  | `CONNECT -> "CONNECT"
  | `TRACE -> "TRACE"
  | `Other s -> String.uppercase_ascii s

let of_string s =
  match String.uppercase_ascii s with
  | "GET" -> `GET
  | "POST" -> `POST
  | "PUT" -> `PUT
  | "DELETE" -> `DELETE
  | "HEAD" -> `HEAD
  | "OPTIONS" -> `OPTIONS
  | "PATCH" -> `PATCH
  | "CONNECT" -> `CONNECT
  | "TRACE" -> `TRACE
  | other -> `Other other

let pp ppf m = Format.fprintf ppf "%s" (to_string m)

let is_safe = function
  | `GET | `HEAD | `OPTIONS | `TRACE -> true
  | `POST | `PUT | `DELETE | `PATCH | `CONNECT | `Other _ -> false

let is_idempotent = function
  | `GET | `HEAD | `PUT | `DELETE | `OPTIONS | `TRACE -> true
  | `POST | `PATCH | `CONNECT | `Other _ -> false

type body_semantics =
  | Body_required
  | Body_optional
  | Body_forbidden

let request_body_semantics = function
  | `POST | `PUT | `PATCH -> Body_required
  | `DELETE | `OPTIONS -> Body_optional
  | `GET -> Body_optional  (* RFC 9110 Section 9.3.1: GET body has no defined semantics *)
  | `HEAD -> Body_forbidden  (* RFC 9110 Section 9.3.2: identical to GET but no body in response *)
  | `TRACE -> Body_forbidden  (* RFC 9110 Section 9.3.8: MUST NOT send body *)
  | `CONNECT -> Body_forbidden  (* RFC 9110 Section 9.3.6: no body in CONNECT request *)
  | `Other _ -> Body_optional  (* Unknown methods - allow body for flexibility *)

let has_request_body = function
  | `POST | `PUT | `PATCH -> true
  | `GET | `HEAD | `DELETE | `OPTIONS | `CONNECT | `TRACE -> false
  | `Other _ -> false  (* Conservative default for unknown methods *)

let is_cacheable = function
  | `GET | `HEAD -> true
  | `POST -> true  (* POST can be cacheable with explicit headers *)
  | `PUT | `DELETE | `PATCH | `OPTIONS | `CONNECT | `TRACE | `Other _ -> false

let equal m1 m2 =
  match m1, m2 with
  | `Other s1, `Other s2 -> String.equal (String.uppercase_ascii s1) (String.uppercase_ascii s2)
  | m1, m2 -> m1 = m2

let compare m1 m2 =
  match m1, m2 with
  | `Other s1, `Other s2 -> String.compare (String.uppercase_ascii s1) (String.uppercase_ascii s2)
  | m1, m2 -> Stdlib.compare m1 m2