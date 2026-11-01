exception Limit

let input ~max_bytes flow =
  if max_bytes <= 0 then invalid_arg "max_bytes must be positive";
  let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
  let count = ref 0 in
  Xmlm.make_input ~strip:false (`Fun (fun () ->
    if Eio.Buf_read.peek_char reader = None then raise End_of_file;
    if !count >= max_bytes then raise Limit;
    let c = Eio.Buf_read.any_char reader in
    incr count; Char.code c))

let normalize s =
  let b = Buffer.create (String.length s) in
  let pending = ref false in
  String.iter
    (fun ch ->
       match ch with
       | ' ' | '\t' | '\n' | '\r' -> pending := true
       | ch ->
         if !pending && Buffer.length b > 0 then Buffer.add_char b ' ';
         pending := false;
         Buffer.add_char b ch)
    s;
  Buffer.contents b

let non_empty b =
  match normalize (Buffer.contents b) with "" -> None | s -> Some s
