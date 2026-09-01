open Base

(* The WHATWG urlencoded byte serializer keeps a smaller set literal than RFC
   3986 unreserved: [~] is escaped and [*] is not. *)
let[@inline] is_literal (c : char) =
  match c with
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
  | '*' | '-' | '.' | '_' -> true
  | _ -> false
;;

let hex_upper = "0123456789ABCDEF"

let encode_into buf s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let c = String.get s !i in
    if Char.equal c ' '
    then Buffer.add_char buf '+'
    else if is_literal c
    then Buffer.add_char buf c
    else (
      let v = Char.to_int c in
      Buffer.add_char buf '%';
      Buffer.add_char buf (String.get hex_upper (v lsr 4));
      Buffer.add_char buf (String.get hex_upper (v land 0xF)));
    Stdlib.incr i
  done
;;

let encode pairs =
  match pairs with
  | [] -> ""
  | _ ->
    let buf = Buffer.create 64 in
    let first = ref true in
    List.iter pairs ~f:(fun (name, value) ->
      if !first then first := false else Buffer.add_char buf '&';
      encode_into buf name;
      Buffer.add_char buf '=';
      encode_into buf value);
    Buffer.contents buf
;;

(* A malformed escape is data, not an error: the byte is kept and scanning
   resumes after it. Only this path allocates a buffer; the strict decoder
   handles every well-formed window. *)
(* [sub] reads its source at [local], which [String.sub] does not. *)
let sub (s : string @ local) ~pos ~len =
  let b = Stdlib.Bytes.create len in
  Stdlib.Bytes.unsafe_blit_string s pos b 0 len;
  Stdlib.Bytes.unsafe_to_string b
;;

let decode_lenient (s : string @ local) ~off ~len =
  let stop = off + len in
  let buf = Buffer.create len in
  let i = ref off in
  while !i < stop do
    let c = String.get s !i in
    if Char.equal c '+'
    then (
      Buffer.add_char buf ' ';
      Stdlib.incr i)
    else if Char.equal c '%' && !i + 2 < stop
    then (
      let hi = Httpz_uri.Scanner.hex_val (String.get s (!i + 1)) in
      let lo = Httpz_uri.Scanner.hex_val (String.get s (!i + 2)) in
      if hi >= 0 && lo >= 0
      then (
        Buffer.add_char buf (Stdlib.Char.unsafe_chr ((hi * 16) + lo));
        i := !i + 3)
      else (
        Buffer.add_char buf c;
        Stdlib.incr i))
    else (
      Buffer.add_char buf c;
      Stdlib.incr i)
  done;
  Buffer.contents buf
;;

let decode_window (s : string @ local) ~off ~len =
  if len = 0
  then ""
  else if not (Httpz_uri.Scanner.needs_percent_decode s ~pos:off ~len ~plus_as_space:true)
  then sub s ~pos:off ~len
  else (
    let dst = Bytes.create len in
    let written =
      Httpz_uri.Scanner.percent_decode_into s ~pos:off ~len ~dst ~dst_pos:0 ~plus_as_space:true
    in
    if written >= 0
    then Bytes.To_string.sub dst ~pos:0 ~len:written
    else decode_lenient s ~off ~len)
;;

let decode (s : string @ local) =
  let n = String.length s in
  let acc = ref [] in
  let start = ref 0 in
  while !start <= n do
    let stop = ref !start in
    while !stop < n && not (Char.equal (String.get s !stop) '&') do
      Stdlib.incr stop
    done;
    if !stop > !start
    then (
      let eq = ref !start in
      while !eq < !stop && not (Char.equal (String.get s !eq) '=') do
        Stdlib.incr eq
      done;
      let pair =
        if !eq >= !stop
        then decode_window s ~off:!start ~len:(!stop - !start), ""
        else
          ( decode_window s ~off:!start ~len:(!eq - !start)
          , decode_window s ~off:(!eq + 1) ~len:(!stop - !eq - 1) )
      in
      acc := pair :: !acc);
    start := !stop + 1
  done;
  List.rev !acc
;;
