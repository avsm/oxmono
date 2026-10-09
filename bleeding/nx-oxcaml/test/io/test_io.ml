(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

let check name value = if not value then failwith name

let with_file f =
  let path = Filename.temp_file "nx-io-test-" ".npy" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)

let load path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let stream ?alignment ga =
  let pieces = ref [] in
  Nx_io.write_npy_genarray ?alignment
    ~write:(fun s -> pieces := s :: !pieces) ga;
  List.rev !pieces

let compare_pristine ga =
  let header = Pristine_npy.encode_header
      ~layout:(Bigarray.Genarray.layout ga)
      ~packed_kind:(Pristine_npy.K (Nx_buffer.genarray_kind ga))
      ~dims:(Bigarray.Genarray.dims ga) in
  check "default upstream header preserved"
    (Nx_io.npy_header ~alignment:16 ga = header);
  (* The pristine file writer maps rank-zero arrays with Unix.map_file,
     which rejects them. Compare scalar headers, then exercise streaming
     with the reader separately. *)
  if Array.length (Bigarray.Genarray.dims ga) <> 0 then
  with_file (fun path ->
      Pristine_npy.write ga path;
      check "pristine and streamed NumPy files agree"
        (String.concat "" (stream ~alignment:16 ga) = load path))

let test_round_trip () =
  let ga = Bigarray.Genarray.create Bigarray.float32 Bigarray.c_layout
      [|17000|] in
  for i = 0 to 16999 do Bigarray.Genarray.set ga [|i|] (float_of_int i) done;
  compare_pristine ga;
  let pieces = stream ga in
  check "bounded streaming" (List.length pieces = 3
      && List.for_all (fun s -> String.length s <= 65536) pieces);
  with_file (fun path ->
      let oc = open_out_bin path in
      Fun.protect ~finally:(fun () -> close_out oc)
        (fun () -> List.iter (fun s -> output_string oc s) pieces);
      let t = Nx_io.to_typed Nx.float32 (Nx_io.load_npy path) in
      check "Nx reads streamed shape" (Nx.shape t = [|17000|]);
      let result = Nx.to_bigarray t in
      for i = 0 to 16999 do
        check "Nx reads every streamed value"
          (Bigarray.Genarray.get result [|i|] = float_of_int i)
      done);
  with_file (fun path ->
      Nx_io.save_npy path (Nx.of_bigarray ga);
      let saved = load path in
      check "Nx save uses pristine encoding"
        (saved = String.concat "" (stream ~alignment:16 ga)))

let () =
  let open Bigarray in
  List.iter (fun dims ->
      let ga = Genarray.create float32 c_layout dims in
      Genarray.fill ga 2.5;
      compare_pristine ga;
      with_file (fun path ->
          let oc = open_out_bin path in
          Fun.protect ~finally:(fun () -> close_out oc)
            (fun () -> Nx_io.write_npy_genarray
                ~write:(fun bytes -> output_string oc bytes) ga);
          let t = Nx_io.to_typed Nx.float32 (Nx_io.load_npy path) in
          check "Nx reads scalar and empty shapes" (Nx.shape t = dims);
          if dims = [||] then
            check "Nx reads scalar value"
              (Genarray.get (Nx.to_bigarray t) [||] = 2.5))) [[||]; [|1|]; [|3;4|]; [|2;3;4|]; [|0|]];
  let fortran = Genarray.create float64 fortran_layout [|2;3|] in
  for i = 1 to 2 do
    for j = 1 to 3 do
      Genarray.set fortran [|i;j|] (float_of_int (i * 10 + j))
    done
  done;
  compare_pristine fortran;
  let integers = Genarray.create int64 c_layout [|4|] in
  Genarray.fill integers 1234567890123L;
  compare_pristine integers;
  test_round_trip ();
  check "invalid alignment rejected"
    (try ignore (Nx_io.npy_header ~alignment:17 integers); false
     with Invalid_argument _ -> true);
  print_endline "Nx NPY streaming, round trips and pristine differentials passed."
