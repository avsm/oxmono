@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Method invocations and result references.

    An Invocation is the triple of a method name, its arguments and a client
    chosen method call id, defined by
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.2} RFC 8620
     Section 3.2}. It is both what a client sends in [methodCalls] and what a
    server sends back in [methodResponses].

    @canonical Jmap.Proto.Invocation *)

(** {1 Invocations} *)

type t = {
  name : string;  (** The method name, such as ["Email/get"]. *)
  arguments : Jsont.json;
      (** The named arguments, a JSON object. {!jsont} requires that on decode
          and encodes the value as it stands, so a value given to {!val-create}
          is not checked. *)
  method_call_id : string;
      (** The client chosen identifier tying a response to its call. *)
}
(** The type for invocations. *)

val create : name:string -> arguments:Jsont.json -> method_call_id:string -> t
(** [create ~name ~arguments ~method_call_id] is the invocation of [name] with
    [arguments], tagged [method_call_id]. *)

val jsont : t Jsont.t
(** [jsont] is the codec for an invocation, which JSON writes as the three
    element array [["methodName", {args}, "methodCallId"]]. Decoding rejects an
    array of any other length, a first or third element that is not a string,
    and a second element that is not an object. *)

(** {1 Result references}

    A result reference lets one method call use a value from the result of an
    earlier call in the same request, per
    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-3.7} RFC 8620
     Section 3.7}. *)

type result_reference = {
  result_of : string;  (** The method call id to take the result from. *)
  name : string;  (** The method name that call must have. *)
  path : Json_pointer.t;
      (** The pointer to the value within that result. It uses ordinary JSON
          Pointer syntax; during resolution, [*] maps through an array as
          specified by JMAP. *)
}
(** The type for result references. *)

val result_reference :
  result_of:string -> name:string -> path:Json_pointer.t -> result_reference
(** [result_reference ~result_of ~name ~path] is the result reference to [path]
    in the result of [result_of]. *)

val result_reference_of_strings :
  result_of:string ->
  name:string ->
  path:string ->
  (result_reference, Jsont.Error.t) result
(** [result_reference_of_strings ~result_of ~name ~path] is
    {!val-result_reference} with [path] parsed as a JSON Pointer. The JMAP
    wildcard extension is applied when the reference is resolved. The error says
    why [path] is not a JSON Pointer. *)

val result_reference_jsont : result_reference Jsont.t
(** [result_reference_jsont] is the codec for a result reference. *)
