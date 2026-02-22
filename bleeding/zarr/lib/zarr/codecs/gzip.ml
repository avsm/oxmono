(** Gzip codec - gzip compression using decompress library *)

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Compress bytes using gzip *)
let compress ~level input =
  let input_len = Bytes.length input in
  if input_len = 0 then Bytes.empty
  else begin
    let output = Buffer.create (input_len / 2 + 128) in

    let w = De.Lz77.make_window ~bits:15 in
    let q = De.Queue.create 0x1000 in

    let input_pos = ref 0 in
    let refill (buf : bigstring) =
      let available = input_len - !input_pos in
      let buf_len = Bigarray.Array1.dim buf in
      let len = min buf_len available in
      for i = 0 to len - 1 do
        Bigarray.Array1.set buf i (Bytes.get input (!input_pos + i))
      done;
      input_pos := !input_pos + len;
      len
    in

    let flush (buf : bigstring) len =
      for i = 0 to len - 1 do
        Buffer.add_char output (Bigarray.Array1.get buf i)
      done
    in

    (* Create input and output buffers for the library *)
    let i_buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0x1000 in
    let o_buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0x1000 in

    (* Use Gz for actual gzip format *)
    let cfg = Gz.Higher.configuration Gz.Unix (fun () -> Int32.zero) in
    Gz.Higher.compress ~level ~w ~q ~refill ~flush () cfg i_buf o_buf;

    Buffer.to_bytes output
  end

(** Decompress bytes using gzip *)
let decompress input =
  let input_len = Bytes.length input in
  if input_len = 0 then Ok Bytes.empty
  else begin
    try
      let output = Buffer.create (input_len * 4) in

      let input_pos = ref 0 in
      let refill (buf : bigstring) =
        let available = input_len - !input_pos in
        let buf_len = Bigarray.Array1.dim buf in
        let len = min buf_len available in
        for i = 0 to len - 1 do
          Bigarray.Array1.set buf i (Bytes.get input (!input_pos + i))
        done;
        input_pos := !input_pos + len;
        len
      in

      let flush (buf : bigstring) len =
        for i = 0 to len - 1 do
          Buffer.add_char output (Bigarray.Array1.get buf i)
        done
      in

      (* Create input and output buffers for the library *)
      let i_buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0x1000 in
      let o_buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0x1000 in

      (* Use Gz for actual gzip format *)
      match Gz.Higher.uncompress ~refill ~flush i_buf o_buf with
      | Ok _metadata -> Ok (Buffer.to_bytes output)
      | Error (`Msg msg) -> Error (`Codec_error ("gzip decompress failed: " ^ msg))
    with
    | Invalid_argument msg -> Error (`Codec_error ("gzip decompress invalid: " ^ msg))
    | exn -> Error (`Codec_error ("gzip decompress error: " ^ Printexc.to_string exn))
  end

(** Create a gzip codec with specified compression level *)
let create level : Codec_intf.bytes_to_bytes = {
  encode = (fun bytes -> compress ~level bytes);
  decode = (fun bytes -> decompress bytes);
  compute_encoded_size = (fun _ -> None);  (* Compressed size is variable *)
}
