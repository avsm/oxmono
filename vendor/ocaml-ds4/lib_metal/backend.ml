(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Metal backend, with the embedded .metal kernels the C backend JIT-compiles at
   engine-open. *)
let which = `Metal
let sources = Shaders.sources
