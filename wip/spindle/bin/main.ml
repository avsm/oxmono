(* SPDX-License-Identifier: ISC *)
open Cmdliner

let executor =
  Arg.(
    value
    & opt (enum [ ("print", `Print); ("solve", `Solve) ]) `Print
    & info [ "executor" ]
        ~doc:
          "How workflows are handled. $(b,print) describes each one. \
           $(b,solve) runs a $(b,solve) workflow that pretends to solve \
           dependencies, then adds a workflow for each compiler that logs \
           made-up build and test output. Neither runs anything.")

let () =
  Spindle_cli.main executor (fun env ~sw:_ -> function
    | `Print -> ([ Spindle.Job.inspect ], Spindle.Executor.print env#clock)
    | `Solve ->
        ( [ Spindle.Job.v "solve" [ Spindle.Job.step "solve" "opam solve" ] ],
          Spindle.Executor.solve env#clock ))
