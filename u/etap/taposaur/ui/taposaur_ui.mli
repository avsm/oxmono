(** Taposaur's widgets. The generic ones are {!Sitekit}'s, styled by
    Taposaur's stylesheet. Tables sort their rendered rows in the browser,
    not the whole dataset, with {!Sitekit.Asset.script}. *)
val css : string
(** [css] is Taposaur's own stylesheet, linked after Sitekit's. *)
val node : string -> (string * string) list ->
  Htmlit.El.html list -> Htmlit.El.html
(** [node name attrs children] is an element. Its attribute names are
    Taposaur's literals, never request data. *)
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
    Without a sort value the browser uses the cell's visible text. [gap_sort]
    adds the choice of sorting coverage by count or by percentage. *)
val coverage : present:int -> total:int -> Htmlit.El.html
(** [coverage ~present ~total] is {!Sitekit.coverage} in a [coverage-spark]
    element that Taposaur's layout sizes. *)
val state_sprite : Htmlit.El.html
(** [state_sprite] is {!Sitekit.state_sprite}. Include it before any
    {!state_icon} or a client-built [<use>] of [#sk-state-STATE]. *)
val state_icon : ?label:string -> string -> Htmlit.El.html
(** [state_icon state] is {!Sitekit.state_icon}. *)
