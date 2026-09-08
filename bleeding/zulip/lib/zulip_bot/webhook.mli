(** Zulip outgoing-webhook validation and dispatch.

    The HTTP application owns request parsing and maps validation results to
    responses. *)

type result = [ `Accepted | `Stopped | `Invalid_token | `Malformed of string ]
(** The type for webhook acceptance results. [`Accepted] means that the message
    was accepted for bot dispatch. Bot filters can discard it, and queued
    handlers may not have completed. [`Stopped] means that the bot stopped
    before dispatch completed. *)

val handle : token:string -> Bot.t -> payload:string -> result
(** [handle ~token bot ~payload] validates a Zulip outgoing-webhook [payload]
    against [token] and dispatches its message to [bot]. It first checks that
    the bot is running and that [token] is nonempty. It then enforces a 1 MiB
    payload limit, a depth limit of 64, and at most 4096 members in each object
    or array. The payload must contain a matching string token, a supported
    string trigger, and a decodable message. Supported triggers are [mention],
    [direct_message], and [private_message].

    Dispatch can block under bot backpressure. [`Accepted] follows successful
    dispatch or filtering. A mismatching string token, or an empty configured
    [token], returns [`Invalid_token]. A non-string payload token and syntax,
    structure, trigger, or message errors return [`Malformed message]. *)
