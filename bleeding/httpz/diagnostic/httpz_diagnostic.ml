module Buffer = Base.Buffer

(* Render C0 controls, DEL, and UTF-8-encoded C1 controls visibly so that an OSC, DCS or
   CSI sequence and line injection cannot survive into a log or a terminal. *)
let sanitize s =
  let b = Buffer.create (String.length s) in
  (* A bare C1 byte is escaped only when the whole string is not UTF-8: in valid UTF-8 the
     same byte values occur inside multi-byte sequences. *)
  let valid_utf8 = String.is_valid_utf_8 s in
  let escaped_byte byte = Buffer.add_string b (Printf.sprintf "\\x%02X" byte) in
  let rec go i =
    if i >= String.length s then ()
    else
      let byte = Char.code (String.unsafe_get s i) in
      if byte <= 0x1f || byte = 0x7f then begin
        escaped_byte byte;
        go (i + 1)
      end
      else if
        byte = 0xc2
        && i + 1 < String.length s
        &&
        let next = Char.code (String.unsafe_get s (i + 1)) in
        next >= 0x80 && next <= 0x9f
      then begin
        let control = Char.code (String.unsafe_get s (i + 1)) in
        Buffer.add_string b (Printf.sprintf "\\u{%04X}" control);
        go (i + 2)
      end
      else if (not valid_utf8) && byte >= 0x80 && byte <= 0x9f then begin
        escaped_byte byte;
        go (i + 1)
      end
      else begin
        Buffer.add_char b (String.unsafe_get s i);
        go (i + 1)
      end
  in
  go 0;
  Buffer.contents b
;;
