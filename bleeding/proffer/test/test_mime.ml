open Proffer

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let () =
  check "css" (Mime.of_path "a/b/site.css" = "text/css");
  check "svg" (Mime.of_path "icon.svg" = "image/svg+xml");
  check "png" (Mime.of_path "x.png" = "image/png");
  check "woff2" (Mime.of_path "f.woff2" = "font/woff2");
  check "case is folded" (Mime.of_path "PHOTO.JPG" = "image/jpeg");
  check "no extension is octet-stream"
    (Mime.of_path "README" = "application/octet-stream");
  check "unknown extension is octet-stream"
    (Mime.of_path "a.xyz" = "application/octet-stream");
  check "a dotfile has no extension"
    (Mime.of_path ".gitignore" = "application/octet-stream");
  Printf.printf "test_mime: %d checks ok\n" !checks
