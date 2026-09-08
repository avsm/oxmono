(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* RFC 3986 Section 3.1: scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ). *)
let is_valid s =
  let is_scheme_char c =
    Jscontact_ascii.is_alnum c || c = '+' || c = '-' || c = '.'
  in
  match String.index_opt s ':' with
  | None | Some 0 -> false
  | Some i ->
      let scheme = String.sub s 0 i in
      Jscontact_ascii.is_alpha scheme.[0]
      && String.for_all is_scheme_char scheme
      && String.for_all Jscontact_ascii.is_graphic s

let validate ~prop s =
  Jscontact_valid.(
    let* () =
      check (is_valid s)
        "%s: %S is not a URI, which is a scheme and a colon followed by the \
         rest"
        prop s
    in
    ok s)
