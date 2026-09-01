(* The last dot after the last slash starts the extension. A dot in position zero of the
   final segment is a dotfile, which has no extension. *)
let extension name =
  let slash =
    match String.rindex_opt name '/' with
    | Some i -> i + 1
    | None -> 0
  in
  match String.rindex_opt name '.' with
  | Some i when i > slash -> Some (String.sub name (i + 1) (String.length name - i - 1))
  | _ -> None
;;

(* magic-mime's generated lookup is a pure read over immutable tables. The
   package predates portable-mode annotations, so audit that boundary once
   rather than making every portable Proffer route nonportable. *)
let magic_lookup : (?default:string -> string -> string) @ portable =
  Obj.magic_portable Magic_mime.lookup
;;

let of_path name =
  (* Let magic-mime own the general registry. Its 1.3.x database predates a
     few types used by modern web applications, and spells JavaScript and
     Markdown differently, so keep only those compatibility overrides here. *)
  match Option.map String.lowercase_ascii (extension name) with
  | Some ("js" | "mjs") -> "text/javascript"
  | Some ("md" | "markdown" | "mkd") -> "text/markdown"
  | Some "avif" -> "image/avif"
  | Some "webmanifest" -> "application/manifest+json"
  | Some _ -> magic_lookup ~default:"application/octet-stream" name
  | None -> "application/octet-stream"
;;
