(** This module maps filename extensions to content types. *)

(** [of_path name] is the Content-Type for [name], chosen from its extension with case
    folded, or ["application/octet-stream"] when the extension is absent or unknown. A
    name whose only dot starts it, such as [".gitignore"], has no extension.
    The lookup allocates no heap memory and returns a shared constant string. *)
val[@zero_alloc] of_path : string @ local -> string @@ portable
