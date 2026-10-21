(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* CUDA backend. Unlike Metal, whose kernels the C backend compiles when an
   engine opens, nvcc compiles these into the archive, so there is nothing to
   write to disk. *)
let which = `Cuda
let sources = []
