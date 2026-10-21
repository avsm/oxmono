(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A typed tool an agent can call.

    {!Dsml.Codec.Invoke} defines its name and parameters. The handler receives
    decoded OCaml values.

    {[
    let weather =
      let open Dsml.Codec in
      Invoke.map "weather" Fun.id
      |> Invoke.param ~enc:Fun.id "city" string ~description:"city name"
      |> Invoke.seal
    in
    Tool.v ~description:"Return the current weather." weather (fun city ->
        city ^ ": 18 C")
    ]} *)

type t
(** A tool the model may call. *)

type result = { text : string; images : string list }
(** A tool observation. [images] contains encoded PNG or JPEG files in display
    order. *)

val text : string -> result
(** [text s] is a text-only observation containing [s]. *)

val pp_result : Format.formatter -> result -> unit
(** [pp_result ppf result] prints [result]. *)

val v : description:string -> 'a Dsml.Codec.t -> ('a -> string) -> t
(** [v ~description args handler] creates a text-returning tool. Invalid
    arguments and ordinary handler exceptions become error observations. Eio
    cancellation, [Out_of_memory], and [Stack_overflow] are re-raised. *)

val raw :
  name:string -> description:string -> schema:Dsml.Json.t ->
  (Dsml.tool_call -> string) -> t
(** [raw] adapts a backend-independent tool with an existing JSON schema and
    invocation handler. *)

val v_result : description:string -> 'a Dsml.Codec.t -> ('a -> result) -> t
(** [v_result ~description args handler] is like {!v}, but [handler] may add
    encoded images to its observation. *)

val name : t -> string
(** [name t] is the name the model uses to call [t]. *)

val description : t -> string
(** [description t] is the description shown to the model. *)

val schema : t -> Dsml.Json.t
(** [schema t] is the JSON Schema for [t]'s arguments. *)

val invoke : t -> Dsml.tool_call -> string
(** [invoke t call] runs [t] with the arguments in [call] and returns its
    result, or an error string if the call fails. *)

val invoke_result : t -> Dsml.tool_call -> result
(** [invoke_result t call] runs [t] and returns its complete observation. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf tool] prints [tool]. *)
