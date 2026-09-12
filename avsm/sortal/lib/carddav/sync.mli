(** Combined preview against a freshly exported source and server snapshot. *)

val preview :
  ?collection:string ->
  ?previous:string ->
  ?seed:string ->
  dav:Remote.t ->
  source:string ->
  bundle:string ->
  username:string ->
  output:string ->
  unit ->
  Common.value
(** Requires a read-only transport and new output directory. Retains existing
    UIDs and journals, saves complete snapshots and reviewable YAML diffs. *)
