type mode = Rev1 | Utf8
type t = { raw : string; mode : mode; utf8 : (string, string) result }

let fail message = Error message
let add_utf8 b cp =
  if cp < 0x80 then Buffer.add_char b (Char.chr cp)
  else if cp < 0x800 then (
    Buffer.add_char b (Char.chr (0xc0 lor (cp lsr 6)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3f))))
  else if cp < 0x10000 then (
    Buffer.add_char b (Char.chr (0xe0 lor (cp lsr 12)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3f)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3f))))
  else (
    Buffer.add_char b (Char.chr (0xf0 lor (cp lsr 18)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 12) land 0x3f)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3f)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3f))))

let decode_utf8 s =
  let n=String.length s in
  let continuation i =
    i<n && let c=Char.code s.[i] in c land 0xc0 = 0x80 in
  let rec loop i acc =
    if i=n then Ok (List.rev acc)
    else
      let c=Char.code s.[i] in
      if c<0x80 then loop (i+1) (c::acc)
      else if c>=0xc2 && c<=0xdf && continuation (i+1) then
        let cp=((c land 0x1f) lsl 6) lor (Char.code s.[i+1] land 0x3f) in
        loop (i+2) (cp::acc)
      else if c>=0xe0 && c<=0xef && continuation (i+1) &&
              continuation (i+2) then
        let b1=Char.code s.[i+1] in
        let cp=((c land 0x0f) lsl 12) lor
          ((b1 land 0x3f) lsl 6) lor (Char.code s.[i+2] land 0x3f) in
        if cp<0x800 || (cp>=0xd800 && cp<=0xdfff)
        then fail "invalid UTF-8 scalar" else loop (i+3) (cp::acc)
      else if c>=0xf0 && c<=0xf4 && continuation (i+1) &&
              continuation (i+2) && continuation (i+3) then
        let cp=((c land 0x07) lsl 18) lor
          ((Char.code s.[i+1] land 0x3f) lsl 12) lor
          ((Char.code s.[i+2] land 0x3f) lsl 6) lor
          (Char.code s.[i+3] land 0x3f) in
        if cp<0x10000 || cp>0x10ffff
        then fail "invalid UTF-8 scalar" else loop (i+4) (cp::acc)
      else fail "invalid UTF-8 encoding" in
  loop 0 []

let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,"
let b64_value = function
  | 'A'..'Z' as c -> Some (Char.code c - 65)
  | 'a'..'z' as c -> Some (Char.code c - 97 + 26)
  | '0'..'9' as c -> Some (Char.code c - 48 + 52)
  | '+' -> Some 62 | ',' -> Some 63 | _ -> None

let encode_b64 bytes =
  let b=Buffer.create (String.length bytes*2) in
  let rec loop i =
    if i>=String.length bytes then () else (
      let a=Char.code bytes.[i] in
      let n=String.length bytes in
      let second=if i+1<n then Char.code bytes.[i+1] else 0 in
      let third=if i+2<n then Char.code bytes.[i+2] else 0 in
      Buffer.add_char b alphabet.[a lsr 2];
      Buffer.add_char b alphabet.[((a land 3) lsl 4) lor (second lsr 4)];
      if i+1<n then
        Buffer.add_char b alphabet.[((second land 15) lsl 2) lor (third lsr 6)];
      if i+2<n then Buffer.add_char b alphabet.[third land 63];
      loop (i+3)) in
  loop 0; Buffer.contents b

let decode_b64 s =
  let b=Buffer.create (String.length s) in
  let rec loop i bits acc =
    if i=String.length s then
      if bits>=6 || (bits>0 && acc land ((1 lsl bits)-1) <> 0)
      then fail "invalid modified base64 padding"
      else Ok (Buffer.contents b)
    else match b64_value s.[i] with
      | None -> fail "invalid modified base64 digit"
      | Some digit ->
          let acc=(acc lsl 6) lor digit in
          let bits=bits+6 in
          if bits>=8 then (
            let bits=bits-8 in
            Buffer.add_char b (Char.chr ((acc lsr bits) land 0xff));
            loop (i+1) bits (acc land ((1 lsl bits)-1)))
          else loop (i+1) bits acc in
  if s="" then fail "empty modified base64 shift" else loop 0 0 0

let encode_rev1 s =
  match decode_utf8 s with
  | Error _ as error -> error
  | Ok cps ->
      let out=Buffer.create (String.length s) in
      let shifted=Buffer.create 32 in
      let flush () =
        if Buffer.length shifted>0 then (
          Buffer.add_char out '&';
          Buffer.add_string out (encode_b64 (Buffer.contents shifted));
          Buffer.add_char out '-';
          Buffer.clear shifted) in
      let add_u16 n =
        Buffer.add_char shifted (Char.chr (n lsr 8));
        Buffer.add_char shifted (Char.chr (n land 0xff)) in
      let rec loop = function
        | [] -> flush (); Ok (Buffer.contents out)
        | cp::rest when cp=0x26 ->
            flush (); Buffer.add_string out "&-"; loop rest
        | cp::rest when cp>=0x20 && cp<=0x7e ->
            flush (); Buffer.add_char out (Char.chr cp); loop rest
        | cp::_ when cp<0x20 || cp=0x7f ->
            fail "control character in mailbox name"
        | cp::rest when cp<=0xffff ->
            add_u16 cp; loop rest
        | cp::rest ->
            let x=cp-0x10000 in
            add_u16 (0xd800 lor (x lsr 10));
            add_u16 (0xdc00 lor (x land 0x3ff));
            loop rest in
      loop cps

let decode_rev1 s =
  let n=String.length s in
  let out=Buffer.create n in
  let decode_shift segment =
    match decode_b64 segment with
    | Error _ as e -> e
    | Ok bytes ->
        let len=String.length bytes in
        if len mod 2 <> 0 then fail "odd-length UTF-16BE mailbox shift"
        else
          let u16 i =
            (Char.code bytes.[i] lsl 8) lor Char.code bytes.[i+1] in
          let rec loop i =
            if i=len then Ok ()
            else
              let a=u16 i in
              if a>=0xd800 && a<=0xdbff then
                if i+2>=len then fail "unpaired UTF-16 surrogate"
                else
                  let b=u16 (i+2) in
                  if b<0xdc00 || b>0xdfff
                  then fail "unpaired UTF-16 surrogate"
                  else (
                    add_utf8 out (0x10000 +
                      ((a-0xd800) lsl 10) + (b-0xdc00));
                    loop (i+4))
              else if a>=0xdc00 && a<=0xdfff
              then fail "unpaired UTF-16 surrogate"
              else if a>=0x20 && a<=0x7e
              then fail "printable ASCII encoded in mailbox shift"
              else if a<0x20 || a=0x7f
              then fail "control character in mailbox name"
              else (add_utf8 out a; loop (i+2)) in
          loop 0 in
  let rec loop i =
    if i=n then Ok (Buffer.contents out)
    else
      let c=s.[i] in
      if c='&' then
        match String.index_from_opt s (i+1) '-' with
        | None -> fail "unterminated modified UTF-7 shift"
        | Some j when j=i+1 ->
            Buffer.add_char out '&'; loop (j+1)
        | Some j ->
            (match decode_shift (String.sub s (i+1) (j-i-1)) with
             | Error _ as e -> e | Ok () -> loop (j+1))
      else
        let k=Char.code c in
        if k<0x20 || k>0x7e then fail "non-printable ASCII outside mailbox shift"
        else (Buffer.add_char out c; loop (i+1)) in
  loop 0

let decode ~mode s = match mode with
  | Rev1 -> decode_rev1 s
  | Utf8 ->
      (match decode_utf8 s with Error _ as e -> e | Ok _ -> Ok s)

let encode ~mode s = match mode with
  | Rev1 -> encode_rev1 s
  | Utf8 ->
      (match decode_utf8 s with Error _ as e -> e | Ok _ -> Ok s)

let of_wire ~mode raw = {raw;mode;utf8=decode ~mode raw}
