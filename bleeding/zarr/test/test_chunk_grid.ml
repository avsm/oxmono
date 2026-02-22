(** Tests for chunk grid operations *)

open Alcotest

(* Save Array before Zarr shadows it *)
module StdArray = Array

open Zarr

let test_chunk_shape () =
  let grid = Zarr.Regular { chunk_shape = [|10; 20; 30|] } in
  check (Alcotest.array Alcotest.int) "chunk_shape" [|10; 20; 30|] (Chunk_grid.chunk_shape grid)

let test_num_chunks () =
  let grid = Zarr.Regular { chunk_shape = [|10; 10|] } in
  check (Alcotest.array Alcotest.int) "exact fit" [|10; 10|]
    (Chunk_grid.num_chunks grid [|100; 100|]);
  check (Alcotest.array Alcotest.int) "with remainder" [|11; 11|]
    (Chunk_grid.num_chunks grid [|101; 101|]);
  check (Alcotest.array Alcotest.int) "smaller than chunk" [|1; 1|]
    (Chunk_grid.num_chunks grid [|5; 5|])

let test_chunk_for_index () =
  let grid = Zarr.Regular { chunk_shape = [|10; 10|] } in
  check (Alcotest.array Alcotest.int) "origin" [|0; 0|]
    (Chunk_grid.chunk_for_index grid [|0; 0|]);
  check (Alcotest.array Alcotest.int) "in first chunk" [|0; 0|]
    (Chunk_grid.chunk_for_index grid [|5; 5|]);
  check (Alcotest.array Alcotest.int) "on boundary" [|1; 1|]
    (Chunk_grid.chunk_for_index grid [|10; 10|]);
  check (Alcotest.array Alcotest.int) "far corner" [|4; 4|]
    (Chunk_grid.chunk_for_index grid [|45; 49|])

let test_chunk_bounds () =
  let grid = Zarr.Regular { chunk_shape = [|10; 10|] } in
  let array_shape = [|100; 100|] in

  let bounds = Chunk_grid.chunk_bounds grid array_shape [|0; 0|] in
  check (pair int int) "first chunk start" (0, 10) (StdArray.get bounds 0);
  check (pair int int) "first chunk second dim" (0, 10) (StdArray.get bounds 1);

  let bounds = Chunk_grid.chunk_bounds grid array_shape [|5; 5|] in
  check (pair int int) "middle chunk start" (50, 60) (StdArray.get bounds 0);
  check (pair int int) "middle chunk second dim" (50, 60) (StdArray.get bounds 1);

  (* Edge chunk with remainder *)
  let bounds = Chunk_grid.chunk_bounds grid [|95; 95|] [|9; 9|] in
  check (pair int int) "edge chunk truncated" (90, 95) (StdArray.get bounds 0)

let test_chunk_size () =
  let grid = Zarr.Regular { chunk_shape = [|10; 10|] } in

  let size = Chunk_grid.chunk_size grid [|100; 100|] [|0; 0|] in
  check (Alcotest.array Alcotest.int) "full chunk" [|10; 10|] size;

  let size = Chunk_grid.chunk_size grid [|95; 95|] [|9; 9|] in
  check (Alcotest.array Alcotest.int) "edge chunk" [|5; 5|] size

let test_is_valid_chunk () =
  let grid = Zarr.Regular { chunk_shape = [|10; 10|] } in
  let array_shape = [|100; 100|] in

  check bool "valid 0,0" true (Chunk_grid.is_valid_chunk grid array_shape [|0; 0|]);
  check bool "valid 9,9" true (Chunk_grid.is_valid_chunk grid array_shape [|9; 9|]);
  check bool "invalid 10,0" false (Chunk_grid.is_valid_chunk grid array_shape [|10; 0|]);
  check bool "invalid -1,0" false (Chunk_grid.is_valid_chunk grid array_shape [|-1; 0|]);
  check bool "wrong dims" false (Chunk_grid.is_valid_chunk grid array_shape [|0|])

let test_iter_chunks () =
  let grid = Zarr.Regular { chunk_shape = [|10; 10|] } in
  let chunks = ref [] in
  Chunk_grid.iter_chunks grid [|25; 25|] (fun coords ->
    chunks := coords :: !chunks
  );
  check int "num chunks" 9 (List.length !chunks);
  check bool "contains 0,0" true (List.exists (fun c -> c = [|0; 0|]) !chunks);
  check bool "contains 2,2" true (List.exists (fun c -> c = [|2; 2|]) !chunks)

let test_total_chunks () =
  let grid = Zarr.Regular { chunk_shape = [|10; 10|] } in
  check int "exact fit" 100 (Chunk_grid.total_chunks grid [|100; 100|]);
  check int "with remainder" 9 (Chunk_grid.total_chunks grid [|25; 25|])

let test_json_roundtrip () =
  let grid = Zarr.Regular { chunk_shape = [|10; 20; 30|] } in
  let json = Chunk_grid.to_json grid in
  match Chunk_grid.of_json json with
  | Ok (Zarr.Regular { chunk_shape }) ->
    check (Alcotest.array Alcotest.int) "roundtrip chunk_shape" [|10; 20; 30|] chunk_shape
  | Error _ -> fail "failed to parse"

let tests = [
  "chunk_shape", `Quick, test_chunk_shape;
  "num_chunks", `Quick, test_num_chunks;
  "chunk_for_index", `Quick, test_chunk_for_index;
  "chunk_bounds", `Quick, test_chunk_bounds;
  "chunk_size", `Quick, test_chunk_size;
  "is_valid_chunk", `Quick, test_is_valid_chunk;
  "iter_chunks", `Quick, test_iter_chunks;
  "total_chunks", `Quick, test_total_chunks;
  "json roundtrip", `Quick, test_json_roundtrip;
]
