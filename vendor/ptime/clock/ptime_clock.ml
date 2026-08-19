(*---------------------------------------------------------------------------
   Copyright (c) 2015 The ptime programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Stubs *)

(* The [@@ portable] annotations sit in type position because a signature
   ascription does not lift a structure-level external. The two clock reads
   are [clock_gettime] and [gettimeofday], which are thread safe, so calling
   them from any domain is sound. The time zone read is not annotated: its
   stub calls [localtime] and [gmtime], which return pointers into static
   storage shared by every thread. *)

external ptime_clock_now_d_ps : (unit -> int * int64) @@ portable =
  "ocaml_ptime_clock_now_d_ps"

external ptime_clock_period_d_ps : (unit -> (int * int64) option) @@ portable =
  "ocaml_ptime_clock_period_d_ps"

external ptime_clock_current_tz_offset_s : unit -> int option =
  "ocaml_ptime_clock_current_tz_offset_s"

(* POSIX clock *)

let now () = Ptime.unsafe_of_d_ps (ptime_clock_now_d_ps ())
let period () = Ptime.Span.unsafe_of_d_ps_option (ptime_clock_period_d_ps ())

(* System time zone offset *)

let current_tz_offset_s = ptime_clock_current_tz_offset_s

(* Raw interface *)

let now_d_ps = ptime_clock_now_d_ps
let period_d_ps = ptime_clock_period_d_ps
