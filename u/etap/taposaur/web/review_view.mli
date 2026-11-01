(** Read-only review presentation. All bibliography is frozen snapshot data. *)
module S = Taposaur_review_store
val index : S.review list -> string
val review : S.review -> S.revision list -> S.run list -> S.snapshot list -> string
val snapshot : S.snapshot -> Taposaur_review.Statistics.t option -> string
val compare : S.snapshot -> S.snapshot -> S.difference -> string
val papers : S.snapshot -> publisher:string option ->
  filter:(Taposaur_review.Statistics.field * bool) option -> total:int ->
  Taposaur_review.observation list -> string
