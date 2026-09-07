module Buffer = Base.Buffer

(* Render C0 controls, DEL, UTF-8-encoded C1 controls, and the Unicode line
   separators and bidi controls visibly so that an OSC, DCS or CSI sequence,
   line injection, or display-order spoofing cannot survive into a log or a
   terminal. *)
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
      else if
        byte = 0xd8 && i + 1 < String.length s
        && Char.code (String.unsafe_get s (i + 1)) = 0x9c
      then begin
        Buffer.add_string b "\\u{061C}";
        go (i + 2)
      end
      else if
        (* U+200E/U+200F are directional marks. U+2028 .. U+202E are
           E2 80 A8..AE, and U+2066..U+2069 bidi isolates are E2 81 A6..A9. *)
        byte = 0xe2
        && i + 2 < String.length s
        &&
        let b1 = Char.code (String.unsafe_get s (i + 1))
        and b2 = Char.code (String.unsafe_get s (i + 2)) in
        (b1 = 0x80 && (b2 = 0x8e || b2 = 0x8f || (b2 >= 0xa8 && b2 <= 0xae)))
        || (b1 = 0x81 && b2 >= 0xa6 && b2 <= 0xa9)
      then begin
        let b1 = Char.code (String.unsafe_get s (i + 1))
        and b2 = Char.code (String.unsafe_get s (i + 2)) in
        let u = 0x2000 lor ((b1 land 0x3f) lsl 6) lor (b2 land 0x3f) in
        Buffer.add_string b (Printf.sprintf "\\u{%04X}" u);
        go (i + 3)
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
