(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Library version and User-Agent header support.
    Per RFC 9110 Section 10.1.5 and Recommendation #30:
    Provides a default User-Agent header for HTTP requests. *)

val version : string
(** Library version string (e.g., "0.1.0") *)

val name : string
(** Library name ("ocaml-requests") *)

val user_agent : string
(** Default User-Agent header value.
    Format: "ocaml-requests/VERSION (OCaml OCAML_VERSION)"
    Example: "ocaml-requests/0.1.0 (OCaml 5.2.0)"

    Per RFC 9110 Section 10.1.5, this helps server-side debugging
    and monitoring. The User-Agent is automatically added to requests
    unless the user provides their own. *)
