let table =
  [ ("pdf", "application/pdf"); ("html", "text/html"); ("css", "text/css");
    ("js", "text/javascript"); ("svg", "image/svg+xml"); ("png", "image/png");
    ("jpg", "image/jpeg"); ("jpeg", "image/jpeg"); ("webp", "image/webp");
    ("xml", "application/xml"); ("wasm", "application/wasm");
    ("ico", "image/x-icon"); ("woff", "font/woff"); ("woff2", "font/woff2");
    ("bib", "application/x-bibtex");
    ("webmanifest", "application/manifest+json");
    ("txt", "text/plain"); ("json", "application/json");
    ("atom", "application/atom+xml"); ("opml", "text/x-opml") ]

(* The last dot after the last slash starts the extension. A dot in position
   zero of the final segment is a dotfile, which has no extension. *)
let extension name =
  let slash =
    match String.rindex_opt name '/' with Some i -> i + 1 | None -> 0
  in
  match String.rindex_opt name '.' with
  | Some i when i > slash ->
      Some (String.sub name (i + 1) (String.length name - i - 1))
  | _ -> None

let of_path name =
  match extension name with
  | None -> "application/octet-stream"
  | Some ext -> (
      match List.assoc_opt (String.lowercase_ascii ext) table with
      | Some ct -> ct
      | None -> "application/octet-stream")
