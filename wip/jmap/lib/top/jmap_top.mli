(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Toplevel printers for JMAP values.

    A toplevel that has loaded this library prints the abstract types of {!Jmap}
    readably once {!install} has been called. Loading the library installs
    nothing on its own.
    {[
      # #require "jmap.top";;
      # Jmap_top.install ();;
      - : unit = ()
      # Jmap.Proto.Id.of_string_exn "abc123";;
      - : Jmap.Proto.Id.t = abc123
      # Jmap.Proto.Keyword.of_string "$seen";;
      - : Jmap.Proto.Keyword.t = $seen
    ]} *)

(** {1 Printers} *)

val json_printer : Format.formatter -> Jsont.json -> unit
(** [json_printer ppf j] prints [j] on [ppf] as compact JSON text. *)

val jsont_error_printer : Format.formatter -> Jsont.Error.t -> unit
(** [jsont_error_printer ppf e] prints the message and source position of [e] on
    [ppf]. *)

val handles_printer : Format.formatter -> 'rs Jmap.Chain.Handles.t -> unit
(** [handles_printer ppf hs] prints the method name and call id of each handle
    of [hs] on [ppf], as [[Email/query c0; Email/get c1]]. *)

val pp_as_json : 'a Jsont.t -> Format.formatter -> 'a -> unit
(** [pp_as_json codec ppf v] prints [v] on [ppf] as the compact JSON text
    [codec] encodes it to. Use it to install a printer for a type this module
    has none for.

    @raise Invalid_argument if [v] cannot be encoded with [codec]. *)

(** {1 Encoding} *)

val encode : 'a Jsont.t -> 'a -> Jsont.json
(** [encode codec v] is [v] encoded as a JSON value with [codec].

    @raise Invalid_argument if [v] cannot be encoded with [codec]. *)

val encode_string : 'a Jsont.t -> 'a -> string
(** [encode_string codec v] is [v] encoded as JSON text with [codec].

    @raise Invalid_argument if [v] cannot be encoded with [codec]. *)

(** {1 Decoding} *)

val decode : 'a Jsont.t -> Jsont.json -> 'a
(** [decode codec j] is the JSON value [j] decoded with [codec].

    @raise Invalid_argument if [j] does not decode with [codec]. *)

val decode_string : ?max_depth:int -> 'a Jsont.t -> string -> 'a
(** [decode_string codec s] is the JSON text [s] decoded with [codec].

    [max_depth] defaults to {!Httpz_media.Json.default_max_depth}, counting the
    outermost array or object as depth one. Zero accepts scalars only.

    @raise Invalid_argument
      if [s] does not decode with [codec], exceeds the depth bound, or the bound
      is negative. *)

(** {1 Installation} *)

val install : unit -> unit
(** [install ()] installs a printer for identifiers, keywords, addresses, the
    three error types, requests, responses, chain parse errors, JSON values,
    JSON errors and handle lists. Each printer is installed on its own, and one
    that cannot be installed leaves a message on [stderr] and the others in
    place. *)
