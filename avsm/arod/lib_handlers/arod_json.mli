(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** JSON response encoding. *)

val encode : 'a Jsont.t -> 'a -> string
(** [encode codec v] is [v] encoded as minified JSON. Encoding errors produce
    a JSON error object. *)

val stream : 'a Jsont.t -> 'a -> Proffer.Body.Sink.t -> unit
(** [stream codec v sink] writes [v] as JSON directly to [sink]. *)
