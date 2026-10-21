(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The compiled-in inference backend.

    This module is virtual. Each backend library supplies its own
    implementation, and an executable selects one by depending on it. The engine
    archive and link flags come from the same library, so the values below
    always describe the code that is actually linked. *)

val which : [ `Metal | `Cuda | `Cpu ]
(** [which] is the backend this implementation provides. *)

val sources : (string * string) list
(** [sources] are the GPU kernels to write to disk when an engine is opened, as
    pairs of name and source. Backends that compile their kernels ahead of time
    return an empty list. *)
