(** Tests for chunk key encoding *)

open Alcotest
open Zarr

(* Module aliases for nested types *)
module S = Zarr.Ztypes.Separator

let test_encode_default () =
  let enc = Zarr.Default { separator = S.Slash } in
  check string "1D" "c/5" (Chunk_key.encode enc [|5|]);
  check string "2D" "c/2/3" (Chunk_key.encode enc [|2; 3|]);
  check string "3D" "c/1/2/3" (Chunk_key.encode enc [|1; 2; 3|])

let test_encode_default_dot () =
  let enc = Zarr.Default { separator = S.Dot } in
  check string "1D" "c.5" (Chunk_key.encode enc [|5|]);
  check string "2D" "c.2.3" (Chunk_key.encode enc [|2; 3|])

let test_encode_v2 () =
  let enc = Zarr.V2 { separator = S.Dot } in
  check string "1D" "5" (Chunk_key.encode enc [|5|]);
  check string "2D" "2.3" (Chunk_key.encode enc [|2; 3|])

let test_decode_default () =
  let enc = Zarr.Default { separator = S.Slash } in
  check (option (array int)) "1D" (Some [|5|]) (Chunk_key.decode enc "c/5");
  check (option (array int)) "2D" (Some [|2; 3|]) (Chunk_key.decode enc "c/2/3");
  check (option (array int)) "invalid prefix" None (Chunk_key.decode enc "d/5");
  check (option (array int)) "no prefix" None (Chunk_key.decode enc "5");
  check (option (array int)) "invalid number" None (Chunk_key.decode enc "c/abc")

let test_decode_v2 () =
  let enc = Zarr.V2 { separator = S.Dot } in
  check (option (array int)) "1D" (Some [|5|]) (Chunk_key.decode enc "5");
  check (option (array int)) "2D" (Some [|2; 3|]) (Chunk_key.decode enc "2.3")

let test_full_path () =
  let enc = Zarr.Default { separator = S.Slash } in
  check string "root" "c/0/0" (Chunk_key.full_path "" enc [|0; 0|]);
  check string "root slash" "c/0/0" (Chunk_key.full_path "/" enc [|0; 0|]);
  check string "with path" "foo/bar/c/0/0" (Chunk_key.full_path "foo/bar" enc [|0; 0|]);
  check string "path trailing slash" "foo/bar/c/0/0" (Chunk_key.full_path "foo/bar/" enc [|0; 0|])

let test_metadata_path () =
  check string "root" "zarr.json" (Chunk_key.metadata_path "");
  check string "root slash" "zarr.json" (Chunk_key.metadata_path "/");
  check string "path" "foo/zarr.json" (Chunk_key.metadata_path "foo");
  check string "path trailing slash" "foo/zarr.json" (Chunk_key.metadata_path "foo/");
  check string "nested path" "foo/bar/zarr.json" (Chunk_key.metadata_path "foo/bar")

let test_json_roundtrip () =
  let enc = Zarr.Default { separator = S.Slash } in
  let json = Chunk_key.to_json enc in
  (match Chunk_key.of_json json with
  | Ok (Zarr.Default { separator = S.Slash }) -> ()
  | Ok _ -> fail "wrong encoding type"
  | Error _ -> fail "failed to parse");

  let enc = Zarr.V2 { separator = S.Dot } in
  let json = Chunk_key.to_json enc in
  match Chunk_key.of_json json with
  | Ok (Zarr.V2 { separator = S.Dot }) -> ()
  | Ok _ -> fail "wrong encoding type"
  | Error _ -> fail "failed to parse"

let tests = [
  "encode default", `Quick, test_encode_default;
  "encode default dot", `Quick, test_encode_default_dot;
  "encode v2", `Quick, test_encode_v2;
  "decode default", `Quick, test_decode_default;
  "decode v2", `Quick, test_decode_v2;
  "full_path", `Quick, test_full_path;
  "metadata_path", `Quick, test_metadata_path;
  "json roundtrip", `Quick, test_json_roundtrip;
]
