(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Statistics for connection pool endpoints *)

type t = {
  active : int;
  idle : int;
  total_created : int;
  total_reused : int;
  total_closed : int;
  errors : int;
}

let make ~active ~idle ~total_created ~total_reused ~total_closed ~errors =
  { active; idle; total_created; total_reused; total_closed; errors }

let active t = t.active
let idle t = t.idle
let total_created t = t.total_created
let total_reused t = t.total_reused
let total_closed t = t.total_closed
let errors t = t.errors

let pp ppf t =
  Fmt.pf ppf
    "@[<v>Stats:@,\
     - Active: %d@,\
     - Idle: %d@,\
     - Created: %d@,\
     - Reused: %d@,\
     - Closed: %d@,\
     - Errors: %d@]"
    t.active t.idle t.total_created t.total_reused t.total_closed t.errors
