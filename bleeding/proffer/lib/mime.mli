(** This module maps filename extensions to content types. *)

(** [of_path name] is the Content-Type for [name], chosen from its extension with case
    folded, or ["application/octet-stream"] when the extension is absent or unknown. A
    name whose only dot starts it, such as [".gitignore"], has no extension. *)
val of_path : string -> string @@ portable
