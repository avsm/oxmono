open! Core
open! Async

let log =
  Log.create
    ()
    ~level:`Info
    ~output:[ Log.Output.file `Text ~filename:"/dev/null" ]
    ~on_error:`Raise
;;

let%bench "unused" = [%log.t.debug log "blah"]
let%bench "used printf" = [%log.t.format log "blah"]
let%bench "used printf w/subst" = [%log.t.format log "%s" "blah"]
let%bench "used string" = [%log.t.string log "blah"]
let%bench "used sexp" = [%log.t log ("blah" : string)]
let%bench "used message" = [%log.t log "blah"]
let%bench "flush" = Log.flushed log
