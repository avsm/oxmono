(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

(* The accessors give back what [v] and [with_style] were given. *)
let test a b c d =
  let g = Guide.v ~branch:a ~last:b ~pipe:c ~space:d () in
  check
    (Guide.branch g = a
    && Guide.last g = b
    && Guide.pipe g = c
    && Guide.space g = d);
  let styled = Guide.with_style Style.bold g in
  check (Style.equal (Guide.style styled) Style.bold && Guide.pipe styled = c);
  Fmt.kstr ignore "%a" Guide.pp g

let suite =
  ("guide", [ test_case "accessors" [ bytes; bytes; bytes; bytes ] test ])
