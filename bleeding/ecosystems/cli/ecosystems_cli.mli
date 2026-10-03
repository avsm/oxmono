val main :
  out:Format.formatter ->
  err:Format.formatter ->
  < clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  int Cmdliner.Cmd.t
(** [main ~out ~err env] is the [oecosystems] command. Results go to [out]
    and one-line error messages to [err]. A command that fails returns exit
    code 1. *)
