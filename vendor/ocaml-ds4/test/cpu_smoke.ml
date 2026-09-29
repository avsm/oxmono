(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Build/link smoke test for the CPU backend.

   This executable depends on [ds4.cpu] instead of the
   [default_implementation] (Metal), so it links the CPU engine archive
   (ds4.c -DDS4_NO_GPU) and the shared FFI stub. Reaching a green link here
   proves the CPU variant is selectable and self-consistent.

   Opening a model needs a multi-GB GGUF, so live inference is not exercised
   here; that is covered by live_agent_mock (DS4_LIVE) against the default
   backend. We only force the C chain to resolve. *)
(* Bind (don't apply) an engine entry point so the CPU archive + stub are
   pulled into the link; [_]-prefixed so it is not flagged as unused. *)
let _create = Ds4.V4.create
let () = print_endline "ds4.cpu: linked and selectable"
