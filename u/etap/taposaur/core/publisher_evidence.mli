(** Exact-DOI publisher evidence retained outside the rebuildable catalogue. *)
type t = {doi:string; publisher:string; provider:string; endpoint:string;
  sha256:string; retrieved:float}
val jsont : t Jsont.t
val label : t -> string
