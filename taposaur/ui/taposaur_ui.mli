(** Small, domain-independent Htmlit widgets used by Taposaur views.
    Tables sort their rendered rows in the browser, not the whole dataset. *)
val css : string
val javascript : string
(** Load [javascript] with defer after pages containing sortable tables. *)
val node : string -> (string * string) list ->
  Htmlit.El.html list -> Htmlit.El.html
val badge : ?tone:[ `Neutral | `Good | `Warning ] -> string -> Htmlit.El.html
val metric : label:string -> Htmlit.El.html -> Htmlit.El.html
val input : name:string -> label:string -> kind:string -> value:string ->
  Htmlit.El.html
val select : name:string -> label:string -> value:string ->
  (string * string) list -> Htmlit.El.html
val checkbox : id:string -> label:string -> checked:bool -> Htmlit.El.html
(** [checkbox ~id ~label ~checked] renders a labelled native checkbox. *)
val cell : ?ratio:float -> string -> Htmlit.El.html list -> Htmlit.El.html
val table : ?class_name:string -> ?gap_sort:bool -> (string * bool) list ->
  Htmlit.El.html list list -> Htmlit.El.html
(** [table columns rows] renders sortable columns. [true] selects numeric
    sorting. Rows contain cells, optionally with explicit [cell] sort values.
    Without a sort value the browser uses the cell's visible text. *)
val coverage : present:int -> total:int -> Htmlit.El.html
