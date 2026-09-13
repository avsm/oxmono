(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
val render : string -> string
(** [render html] extracts terminal text, paragraph boundaries, image alt text
    and explicit HTTP or mailto link targets. It never fetches resources. *)
