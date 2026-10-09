(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

(** Public website reads for model analysis. *)
type t

val create : fetch:_ Fetch.t -> clock:_ Eio.Time.Mono.t -> t
(** [create ~fetch ~clock] initializes a website reader with a bounded cache
    of eight snapshots. [fetch] must have no credentials or cookies and use
    {!Feed_http.public_connect}. Requests use GET, [User-Agent: crowthebot],
    at most three redirects and a 30-second deadline. Every HTTP request and
    response status is logged through {!Diagnostics.Tools}. *)

val names : string list
val is_tool : string -> bool
val tools : Agentkit.Agent.Tool.t list
val system_prompt : string
val max_bytes : int
(** [max_bytes] is the maximum decoded download and extracted text size. *)

val normalize : string -> string
(** [normalize url] validates and canonicalizes a public HTTP(S) URL, dropping
    its client-side fragment. Credentials and private IP literals are refused.
    Resolved addresses must additionally be checked by the connector. *)

val invoke : t -> string -> string -> (string, string) result
(** [invoke t name arguments] performs [website_fetch] or [website_read].
    HTML is parsed to readable text with links and image descriptions. Scripts,
    styles and hidden elements are excluded. Text and JSON are also accepted.
    No JavaScript executes. Results are complete JSON values under 4000 bytes.
    [next_offset] pages through an immutable cached snapshot without another
    HTTP request. UTF-8 byte offsets must follow character boundaries.

    HTTP errors, invalid input and expired snapshots return [Error].
    Cancellation propagates. The caller must enforce access and wrap each
    invocation with {!Audit.run} for durable request and outcome logging. *)
