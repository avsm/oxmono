(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP request body construction

    This module provides various ways to construct HTTP request bodies,
    including strings, files, streams, forms, and multipart data.

    {2 Examples}

    {[
      (* Simple text body *)
      let body = Body.text "Hello, World!"

      (* JSON body *)
      let body = Body.json {|{"name": "Alice", "age": 30}|}

      (* Form data *)
      let body = Body.form [
        ("username", "alice");
        ("password", "secret")
      ]

      (* File upload *)
      let body = Body.of_file ~mime:Mime.pdf (Eio.Path.(fs / "document.pdf"))

      (* Multipart form with file *)
      let body = Body.multipart [
        { name = "field"; filename = None;
          content_type = Mime.text_plain;
          content = `String "value" };
        { name = "file"; filename = Some "photo.jpg";
          content_type = Mime.jpeg;
          content = `File (Eio.Path.(fs / "photo.jpg")) }
      ]
    ]}
*)

(** Log source for body operations *)
val src : Logs.Src.t

type t
(** Abstract body type representing HTTP request body content. *)

(** {1 Basic Constructors} *)

val empty : t
(** [empty] creates an empty body (no content). *)

val of_string : Mime.t -> string -> t
(** [of_string mime content] creates a body from a string with the specified MIME type.
    Example: [of_string Mime.json {|{"key": "value"}|}] *)

val of_stream : ?length:int64 -> Mime.t -> Eio.Flow.source_ty Eio.Resource.t -> t
(** [of_stream ?length mime stream] creates a streaming body. If [length] is provided,
    it will be used for the Content-Length header, otherwise chunked encoding is used. *)

val of_file : ?mime:Mime.t -> _ Eio.Path.t -> t
(** [of_file ?mime path] creates a body from a file. If [mime] is not provided,
    the MIME type is automatically detected from the file extension using the
    {{:https://github.com/mirage/ocaml-magic-mime}magic-mime} library,
    which provides accurate MIME type mappings for hundreds of file extensions. *)

(** {1 Convenience Constructors} *)

val json : Jsont.json -> t
(** [json value] creates a JSON body from a Jsont.json value.
    The value is encoded to a JSON string with Content-Type: application/json.

    Example:
    {[
      let body = Body.json (Jsont.Object ([
        ("status", Jsont.String "success");
        ("count", Jsont.Number 42.);
        ("items", Jsont.Array ([Jsont.String "first"; Jsont.String "second"], Jsont.Meta.none))
      ], Jsont.Meta.none))
    ]}
*)

val jsonv : 'a Jsont.t -> 'a -> t
(** [jsonv codec value] creates a JSON body by encoding [value] using the
    typed [codec]. The value is encoded to a minified JSON string with
    Content-Type: application/json.

    This is the preferred way to create JSON bodies from typed OCaml values,
    as it provides type safety and works with custom record types.

    Example:
    {[
      (* Define a codec for your type *)
      type user = { name : string; age : int }

      let user_codec =
        Jsont.Obj.map ~kind:"user" (fun name age -> { name; age })
        |> Jsont.Obj.mem "name" Jsont.string ~enc:(fun u -> u.name)
        |> Jsont.Obj.mem "age" Jsont.int ~enc:(fun u -> u.age)
        |> Jsont.Obj.finish

      (* Create a JSON body from a typed value *)
      let body = Body.jsonv user_codec { name = "Alice"; age = 30 }
    ]}

    @raise Eio.Io with {!Error.Json_encode_error} if encoding fails. *)

val json_stream : Jsont.json -> t
(** [json_stream json_value] creates a streaming JSON body from a Jsont.json value.
    The JSON value will be encoded to a minified JSON string and streamed.

    Example:
    {[
      let large_data = Jsont.Object ([
        ("users", Jsont.Array ([...], Jsont.Meta.none))
      ], Jsont.Meta.none) in
      let body = Body.json_stream large_data
    ]}
*)

val text : string -> t
(** [text str] creates a plain text body with Content-Type: text/plain. *)

val form : (string * string) list -> t
(** [form fields] creates a URL-encoded form body with Content-Type: application/x-www-form-urlencoded.
    Example: [form [("username", "alice"); ("password", "secret")]] *)

(** {1 Multipart Support} *)

type 'a part = {
  name : string;                (** Form field name *)
  filename : string option;     (** Optional filename for file uploads *)
  content_type : Mime.t;        (** MIME type of this part *)
  content : [
    | `String of string          (** String content *)
    | `Stream of Eio.Flow.source_ty Eio.Resource.t  (** Streaming content *)
    | `File of 'a Eio.Path.t    (** File content *)
  ];
}
(** A single part in a multipart body. *)

val multipart : _ part list -> t
(** [multipart parts] creates a multipart/form-data body from a list of parts.
    This is commonly used for file uploads and complex form submissions.

    Example:
    {[
      let body = Body.multipart [
        { name = "username"; filename = None;
          content_type = Mime.text_plain;
          content = `String "alice" };
        { name = "avatar"; filename = Some "photo.jpg";
          content_type = Mime.jpeg;
          content = `File (Eio.Path.(fs / "photo.jpg")) }
      ]
    ]}
*)

(** {1 Properties} *)

val content_type : t -> Mime.t option
(** [content_type body] returns the MIME type of the body, if set. *)

val content_length : t -> int64 option
(** [content_length body] returns the content length in bytes, if known.
    Returns [None] for streaming bodies without a predetermined length. *)

(** {1 Private API} *)

(** Internal functions exposed for use by other modules in the library.
    These are not part of the public API and may change between versions. *)
module Private : sig
  val to_flow_source : sw:Eio.Switch.t -> t -> Eio.Flow.source_ty Eio.Resource.t option
  (** [to_flow_source ~sw body] converts the body to an Eio flow source.
      Uses the switch to manage resources like file handles.
      This function is used internally by the Client module. *)

  val to_string : t -> string
  (** [to_string body] converts the body to a string for HTTP/1.1 requests.
      Only works for materialized bodies (String type).
      Raises Failure for streaming/file/multipart bodies. *)

  val is_empty : t -> bool
  (** [is_empty body] returns true if the body is empty. *)

  val is_chunked : t -> bool
  (** [is_chunked body] returns true if the body should use chunked transfer encoding
      (i.e., it's a stream without known length or a multipart body). *)

  val write : sw:Eio.Switch.t -> Eio.Buf_write.t -> t -> unit
  (** [write ~sw w body] writes the body content to the buffer writer.
      Uses the switch to manage resources like file handles. *)

  val write_chunked : sw:Eio.Switch.t -> Eio.Buf_write.t -> t -> unit
  (** [write_chunked ~sw w body] writes the body content using HTTP chunked
      transfer encoding. Each chunk is prefixed with its hex size. *)
end
