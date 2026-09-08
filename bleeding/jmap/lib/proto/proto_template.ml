(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let expand_template ~vars template =
  let bindings = List.map (fun (name, value) -> (name, `String value)) vars in
  Result.map Httpz_uri.to_string
    (Httpz_uri.Template.expand_uri_assoc template bindings)

let expand ~vars source =
  Result.bind (Httpz_uri.Template.of_string source) (expand_template ~vars)
