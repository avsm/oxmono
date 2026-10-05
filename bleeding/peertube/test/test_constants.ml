(* A video with no category, licence or language is served with
   [{"id": null, "label": "Unknown"}] for each of them. These must decode,
   because they are what PeerTube sends for most videos on a small server. *)

let decode jsont s =
  match Jsont_bytesrw.decode_string jsont s with
  | Ok v -> Ok v
  | Error e -> Error e

let unknown = {|{"id": null, "label": "Unknown"}|}
let known_int = {|{"id": 2, "label": "Attribution"}|}
let known_str = {|{"id": "en", "label": "English"}|}

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let () =
  let module P = Peer_tube in
  check "a category with a null id decodes"
    (Result.is_ok (decode P.VideoConstantNumberCategory.T.jsont unknown));
  check "a licence with a null id decodes"
    (Result.is_ok (decode P.VideoConstantNumberLicence.T.jsont unknown));
  check "a language with a null id decodes"
    (Result.is_ok (decode P.VideoConstantStringLanguage.T.jsont unknown));
  check "a category with an id still decodes"
    (match decode P.VideoConstantNumberCategory.T.jsont known_int with
     | Ok c -> P.VideoConstantNumberCategory.T.id c <> None
     | Error _ -> false);
  check "a language with an id still decodes"
    (match decode P.VideoConstantStringLanguage.T.jsont known_str with
     | Ok l -> P.VideoConstantStringLanguage.T.id l <> None
     | Error _ -> false);
  Printf.printf "test_constants: %d checks passed\n" !checks
