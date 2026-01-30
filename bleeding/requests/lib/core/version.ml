(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Library version and User-Agent header support.
    Per RFC 9110 Section 10.1.5 and Recommendation #30:
    Set a default User-Agent to help server-side debugging. *)

(** Library version - update this when releasing new versions *)
let version = "0.1.0"

(** Library name *)
let name = "ocaml-requests"

(** Default User-Agent header value.
    Format follows common conventions: library-name/version (runtime-info)
    Per RFC 9110 Section 10.1.5, this helps with debugging and statistics. *)
let user_agent =
  Printf.sprintf "%s/%s (OCaml %s)" name version Sys.ocaml_version
