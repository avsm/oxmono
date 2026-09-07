open Proffer

let checks = ref 0

let check name b =
  incr checks;
  if not b
  then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)
;;

let () =
  check "css" (Mime.of_path "a/b/site.css" = "text/css");
  check "svg" (Mime.of_path "icon.svg" = "image/svg+xml");
  check "png" (Mime.of_path "x.png" = "image/png");
  check "woff2" (Mime.of_path "f.woff2" = "font/woff2");
  check "markdown" (Mime.of_path "README.md" = "text/markdown");
  check "markdown alias" (Mime.of_path "README.mkd" = "text/markdown");
  check "long markdown alias" (Mime.of_path "README.markdown" = "text/markdown");
  check "javascript" (Mime.of_path "app.js" = "text/javascript");
  check "module javascript" (Mime.of_path "app.mjs" = "text/javascript");
  check "csv" (Mime.of_path "data.csv" = "text/csv");
  check "avif" (Mime.of_path "image.avif" = "image/avif");
  check "webmanifest"
    (Mime.of_path "app.webmanifest" = "application/manifest+json");
  check "wasm" (Mime.of_path "app.wasm" = "application/wasm");
  check "epub" (Mime.of_path "book.epub" = "application/epub+zip");
  check "office document"
    (Mime.of_path "notes.docx"
     = "application/vnd.openxmlformats-officedocument.wordprocessingml.document");
  check "OCaml interface" (Mime.of_path "api.mli" = "text/x-ocaml");
  check "punctuation in extension" (Mime.of_path "header.h++" = "text/x-chdr");
  check "numeric extension" (Mime.of_path "clip.3gp" = "video/3gpp");
  check "conflicting source mapping keeps precedence"
    (Mime.of_path "schema.ac" = "application/pkix-attr-cert");
  check "case is folded" (Mime.of_path "PHOTO.JPG" = "image/jpeg");
  check "no extension is octet-stream" (Mime.of_path "README" = "application/octet-stream");
  check
    "unknown extension is octet-stream"
    (Mime.of_path "a.proffer-unknown" = "application/octet-stream");
  check
    "a dotfile has no extension"
    (Mime.of_path ".gitignore" = "application/octet-stream");
  List.iter
    (fun path ->
      check ("no recognised extension: " ^ String.escaped path)
        (Mime.of_path path = "application/octet-stream"))
    [ ""; "."; ".."; "/"; "/."; "/.."; ".css"; "dir/.css";
      "dir.css/README"; "dir.css/"; "x.css/README"; "x.css/";
      "x."; "dir/x."; "x.css."; "README"; "Makefile"; "opam";
      "x.css?query"; "x.css#fragment"; "x.%63ss"; "x.css\000";
      "x.\255css"; "x.Rhistory"; "x.Rout.save" ];
  List.iter
    (fun path -> check ("CSS path: " ^ path) (Mime.of_path path = "text/css"))
    [ "x.css"; "x.CsS"; "./x.css"; "../x.css"; ".hidden.css";
      "dir/.hidden.css"; "dir.with.dots/x.css"; "x..css"; "..css";
      "//x.css" ];
  check "only the final extension matters"
    (Mime.of_path "archive.tar.bz2" = "application/x-bzip2");
  check "long unknown extension"
    (Mime.of_path ("x." ^ String.make 100_000 'a') = "application/octet-stream");
  check "long extensionless name"
    (Mime.of_path (String.make 100_000 'a') = "application/octet-stream");
  check "long directory need not be scanned"
    (Mime.of_path (String.make 100_000 'a' ^ "/x.CSS") = "text/css");
  (* Check the hand-maintained registry's invariants and cover the complete
     binary-search range, including ASCII case folding and dotfile handling. *)
  Stdlib_stable.Iarray.iteri
    (fun i (extension, content_type) ->
      check ("lowercase key: " ^ extension)
        (extension = String.lowercase_ascii extension);
      check ("extension key: " ^ extension)
        (extension <> "" && not (String.contains extension '.')
         && not (String.contains extension '/'));
      if i > 0 then (
        let previous, _ = Stdlib_stable.Iarray.get Mime_data.extensions (i - 1) in
        check ("strictly sorted key: " ^ extension)
          (String.compare previous extension < 0));
      List.iter
        (fun spelling ->
          List.iter
            (fun prefix ->
              let path = prefix ^ spelling in
              check path (Mime.of_path path = content_type))
            [ "file."; "dir.with.dots/file."; "dir/.hidden." ];
          check ("dotfile: " ^ spelling)
            (Mime.of_path ("dir/." ^ spelling) = "application/octet-stream"))
        [ extension; String.uppercase_ascii extension; String.capitalize_ascii extension ])
    Mime_data.extensions;
  Printf.printf "test_mime: %d checks ok\n" !checks
;;
