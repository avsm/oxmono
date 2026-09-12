(** Immutable local recovery bundles, compatible with the earlier manifests. *)

val contact : string -> Common.value
(** Parse YAML, reject duplicate keys, and validate with the native V2 schema.
    The original generic tree is retained for losslessness checks. *)

val verify : ?source:string -> string -> Common.value
(** Verify inventory, hashes, identity, all mapped fields and original photos.
    With [source], compare every source file with its archived bytes too. *)

val export :
  ?previous:string ->
  ?renames:string list ->
  ?as_of:string ->
  ?version:string ->
  source:string ->
  output:string ->
  unit ->
  Common.value
(** Create a new private bundle. [previous] retains the store and contact UIDs;
    [renames] are [OLD=NEW] bindings. A failed export removes only the output
    created by this invocation. Source files are never changed. *)
