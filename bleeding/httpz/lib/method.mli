(** This module represents HTTP request methods recognized by the parser.

    The core method semantics are defined by
    {{:https://www.rfc-editor.org/rfc/rfc9110.html#name-methods} RFC 9110, Section 9};
    WebDAV methods are defined by
    {{:https://www.rfc-editor.org/rfc/rfc4918.html} RFC 4918}. A method not represented
    here produces {!Buf_read.Unsupported_method} when it is a token followed by a space,
    and {!Buf_read.Invalid_method} otherwise, so the unrecognized method in
    ["GETX/foo HTTP/1.1"] is a malformed request line rather than an unsupported method.
    Clients can still write extension methods with {!Req.write_request_line}. *)

(** A [t] is a recognized request method. *)
type t =
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Connect
  | Options
  | Trace
  | Patch
  | Propfind
  | Proppatch
  | Mkcol
  | Copy
  | Move
  | Lock
  | Unlock
  | Report

(** [to_string method_] is the uppercase wire spelling of [method_]. *)
val to_string : t -> string @@ portable

(** [pp formatter method_] is the formatter operation that prints the uppercase wire
    spelling of [method_]. *)
val pp : Stdlib.Format.formatter -> t -> unit @@ portable
