(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Emitter - converts YAML data structures to string output

    The emitter can write to either a Buffer (default) or directly to a bytesrw
    Bytes.Writer for streaming output. *)

(** {1 Configuration} *)

type config = {
  encoding : Encoding.t;
  scalar_style : Scalar_style.t;
  layout_style : Layout_style.t;
  indent : int;
  width : int;
  canonical : bool;
}

val default_config : config
(** Default emitter configuration *)

(** {1 Emitter Type} *)

type t

(** {1 Constructors} *)

val create : ?config:config -> unit -> t
(** Create an emitter that writes to an internal buffer *)

val of_writer : ?config:config -> Bytesrw.Bytes.Writer.t -> t
(** Create an emitter that writes directly to a Bytes.Writer *)

(** {1 Output} *)

val contents : t -> string
(** Get accumulated output. Returns empty string for writer-based emitters. *)

val reset : t -> unit
(** Reset emitter state and clear buffer *)

val buffer : t -> Buffer.t option
(** Access underlying buffer (None for writer-based emitters) *)

val flush : t -> unit
(** Flush writer sink (no-op for buffer-based emitters) *)

(** {1 Event Emission} *)

val emit : t -> Event.t -> unit
(** Emit a single event *)

(** {1 Accessors} *)

val config : t -> config
(** Get emitter configuration *)

val is_streaming : t -> bool
(** Check if emitter is writing to a Writer (vs buffer) *)
