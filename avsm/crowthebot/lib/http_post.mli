(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

(** Explicit public HTTP POST requests, separate from website reads. *)
type t
val create : fetch:_ Fetch.t -> clock:_ Eio.Time.Mono.t -> t
(** [create ~fetch ~clock] creates a POST-only client. [fetch] must have no
    cookies, credentials or retry middleware and use {!Feed_http.public_connect}.
    Requests use [User-Agent: crowthebot], no redirects and a 30-second deadline.
    Request URLs and response status are logged through {!Diagnostics.Tools}.
    At most 64 KiB of response text is read before returning a bounded excerpt. *)
val names : string list
val is_tool : string -> bool
val tools : Agentkit.Agent.Tool.t list
val system_prompt : string
val max_body_bytes : int
(** [max_body_bytes] is the maximum request body size. *)
val invoke : t -> string -> string -> (string, string) result
(** [invoke t name arguments] performs one [http_post] request. Arguments contain
    [url], [body] and optionally [content_type]. JSON is the default. Plain text
    and URL-encoded form bodies are also supported. Results are complete JSON
    values under 4000 bytes, containing HTTP status and response text.
    HTTP error statuses are returned with their response, including redirects.
    Cancellation propagates. Network errors and timeouts report an uncertain
    outcome. A caller must not automatically repeat such a request.

    The caller must check authority and use {!Audit.run} before invoking this
    tool. Response text and arguments are never written to console logs. *)
