val cmd : int Cmdliner.Cmd.t
(** Commands embedded in the Sortal binary. All network clients are created
    under Eio switches, with no subprocess or scripting runtime. *)
