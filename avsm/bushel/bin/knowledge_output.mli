val print : ?elapsed:float -> command:string -> string -> unit
(** [print ~command response] prints a human-readable knowledge response. Errors
    go to stderr. Other results go to stdout. [elapsed] is the duration in
    seconds, used in the indexing summary. Terminal controls in response text
    are sanitized. *)
