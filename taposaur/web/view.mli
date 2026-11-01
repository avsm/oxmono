(** HTML presentation only. Views accept domain records, never handles. *)
val el : string -> Htmlit.El.html list -> Htmlit.El.html
val txt : string -> Htmlit.El.html
val p : string -> Htmlit.El.html
val count : int -> string
val link : string -> string -> Htmlit.El.html
val url : string -> (string * string) list -> string
val page : ?kind:string -> string -> Htmlit.El.html list -> string
val fragment : string -> Htmlit.El.html list -> string
val home : details_cached:int ->
  Taposaur_catalog.stats -> (string * string) list -> string
val search : (string * string) list -> Taposaur_catalog.page -> string
val paper : ?catalogue_status:string -> Taposaur_paper.t -> string
val paper_preview : Taposaur.Summary.t -> string
