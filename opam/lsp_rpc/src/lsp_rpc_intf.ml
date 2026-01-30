open! Core
open Async
open Jsonrpc

(** LSP uses a bespoke protocol where each packet has a header with content type and
    length followed by a body that conforms to the JSON RPC 2.0 specification. This module
    implements a client and server for this specification without making any assumptions
    about supported capabilities. *)

module Request_error = struct
  (** [`Method_not_implemented] means the callee has no implementation for this method.

      [`Request_failed] means the request failed for some other reason. *)
  type t =
    [ `Method_not_implemented
    | `Request_failed of string
    ]
end

module Client_error = Request_error

module Server_error = struct
  (** [`Content_modified] means the request was invalidated because the server detected
      the content of a document was modified unexpectedly (e.g., by a process outside the
      editor). When this error is received, the caller should consider calling the method
      again.

      [`Server_cancelled] means the request was cancelled by the server. This should only
      be used for requests that are explicitly marked server-cancellable in the LSP spec.

      NOTE: [`Content_modified] and [`Server_cancelled] are NOT sent when there are queued
      messages that communicate modifications to the document. If a state change after a
      request invalidates the request, it is the responsibility of the client to send a
      notification to cancel the request. *)
  type t =
    [ Request_error.t
    | `Content_modified
    | `Server_cancelled
    ]
end

module Notification_parse_result = struct
  (** If a received notification has invalid parameters, the parse failure message and
      unparsed notification will be provided with [Invalid_params]. Modeling this scenario
      and letting the caller choose the desired behavior is necessary because there is no
      way to provide a default error handling mechanism to surface the error to the
      counterparty (unlike for requests, where an error response can be returned) and it
      is unlikely that the caller will want to raise when this happens. Callers may simply
      want to ignore notifications with invalid params. *)
  type 'a t =
    | T of 'a
    | Invalid_params of
        { message : string
        ; notification : Notification.t
        }
end

module On_client_termination = struct
  type t =
    | Do_nothing
    | Detect_and_exit of
        (* Some language servers support invocation with a [-clientProcessId] flag, so the
           pid may be known at startup. If the flag isn't supported or isn't passed, the
           pid can also be read from the client's [Initialize] request, but the field is
           optional. *)
        { client_pid : [ `Pid of Pid.t | `Use_pid_in_initialize_request_if_provided ]
        ; poll_every : Time_ns.Span.t
        }
end

module type Rpc = sig
  type 'a sendable_request
  type 'a receivable_request
  type sendable_notification
  type receivable_notification
  type request_error
  type t

  (** [cancelled] is filled when the counterparty cancels the request. *)
  type request_handler =
    { f :
        'a.
        'a receivable_request
        -> cancelled:unit Deferred.t
        -> ('a, request_error) Deferred.Result.t
    }

  (** The [reader] and [writer] will be closed when any of the following occur:
      - [reader] sees [`Eof]
      - A fatal parse error occurs, leaving [reader] in a bad state
      - The [write] system call fails when using [writer] *)
  val create
    :  Reader.t
    -> Writer.t
       (* Note that the [CancelRequest] and [Exit] notifications will never be passed to
          this handler - they are handled at the RPC layer. *)
    -> on_notification:
         (receivable_notification Notification_parse_result.t -> unit Deferred.t)
    -> on_request:request_handler
    -> on_error:(Rpc_error.t -> unit)
    -> t

  (** Send an LSP request. This function should NOT be called from inside [on_request]:
      doing so will cause deadlock if the counterparty has a synchronous LSP
      implementation or processes requests sequentially (as this implementation does). *)
  val call : t -> 'a sendable_request -> 'a Deferred.Or_error.t

  (** [call'] gives finer control over [call] by differentiating failure to send from a
      response error and by exposing the response error in its typed form. See the warning
      there about not calling it in [on_request]. *)
  val call'
    :  t
    -> 'a sendable_request
    -> ('a, Response.Error.t) Result.t Deferred.Or_error.t

  (** Send an LSP notification. *)
  val notify : t -> sendable_notification -> unit Deferred.Or_error.t
end

module type Lsp_rpc = sig
  module Rpc_error = Rpc_error

  module Client :
    Rpc
    with type 'a sendable_request := 'a Lsp.Client_request.t
     and type 'a receivable_request := 'a Lsp.Server_request.t
     and type sendable_notification := Lsp.Client_notification.t
     and type receivable_notification := Lsp.Server_notification.t
     and type request_error := Client_error.t

  (** This [Server] implementation takes care of failing requests that are sent before
      [Initialize] and after [Shutdown] and takes care of exiting on [Exit]. It also
      negotiates the character encoding, favoring UTF-8 if supported by the client.

      Unicode notes:
      - Text is always sent between the client and server in UTF-8. This is specified in
        the base protocol.
      - Positions are expressed in terms of "code units" of the negotiated encoding. For
        UTF-8, that means byte offsets. For UTF-16, that means 1 for 2-byte unicode
        scalars and 2 for 4-byte unicode scalars (i.e., multiply by 2 to get the byte
        offset). The [Text_document] module in the LSP library handles conversions w.r.t.
        the encoding. For any direct usage of positions, including constructing positions
        to send back to the client, you need to perform conversions yourself (since the
        text is UTF-8). The [Utf_offset_conv] library can help with these conversions.
      - Because all ASCII characters are encoded in 1 byte in UTF-8 and 2 bytes in UTF-16,
        the position's offset in ASCII text can be used directly as a byte index in the
        text without any conversion. Therefore, if you don't handle conversions, lines
        with only ASCII characters will still be interpreted properly. *)
  module Server : sig
    include
      Rpc
      with type 'a sendable_request := 'a Lsp.Server_request.t
       and type 'a receivable_request := 'a Lsp.Client_request.t
       and type sendable_notification := Lsp.Server_notification.t
       and type receivable_notification := Lsp.Client_notification.t
       and type request_error := Server_error.t

    val encoding : here:[%call_pos] -> t -> [ `UTF_8 | `UTF_16 ]

    val create
      :  ?on_client_termination:On_client_termination.t (* default: [Do_nothing]. *)
      -> Reader.t
      -> Writer.t
      -> on_notification:
           (Lsp.Client_notification.t Notification_parse_result.t -> unit Deferred.t)
      -> on_request:
           request_handler (* Calling [Rpc_error.raise] here is a reasonable choice. *)
      -> on_error:(Rpc_error.t -> unit)
      -> t
  end
end
