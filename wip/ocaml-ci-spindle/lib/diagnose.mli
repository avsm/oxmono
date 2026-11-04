(** Diagnosing failed builds with a language model.

    The model is asked once, through an OpenAI-compatible chat completions API
    such as OpenRouter's or vLLM's, with the failed builds' logs and the project
    files they name. It is given no tools, so it can only answer. *)

type t

type failure = {
  workflow : string;  (** The failed build's workflow name. *)
  error : string option;
  log : string;  (** Its log, as {!Spindle.Executor.outcome} gives it. *)
}

val default_url : string
(** [default_url] is [http://sequoia:8000/v1], a test endpoint. *)

val default_model : string
(** [default_model] is [Qwen/Qwen3.8-27B-FP8], the model it serves. *)

val v :
  ?key:string ->
  ?model:string ->
  ?timeout:float ->
  clock:_ Eio.Time.Mono.t ->
  Fetch_httpz.t ->
  string ->
  t
(** [v ~key ~model ~timeout ~clock fetch url] asks [model], [default_model] by
    default, at the API whose base URL is [url], such as
    [https://openrouter.ai/api/v1]. [key] is sent as a bearer token, and is sent
    nowhere else, since redirects are not followed. A request may take [timeout]
    seconds, 600 by default. *)

val model : t -> string
val url : t -> string

val prompt :
  files:Spindle.Executor.files -> passed:string list -> failure list -> string
(** [prompt ~files ~passed failures] is the question about [failures]: the tail
    of each one's failed step, the names of the builds that [passed], and the
    root opam and dune files with up to six files the logs name, read from
    [files]. Each log is cut to 24 KiB and each file over 32 KiB is left out. *)

val ask : t -> prompt:string -> (string, string) result
(** [ask t ~prompt] is the model's Markdown answer to [prompt], without any
    reasoning it showed first, or why there is none. *)

(**/**)

val strip_thinking : string -> string
val mentioned : string -> string list
val answer : string -> (string, string) result
