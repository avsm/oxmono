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

let encoded_length s =
  String.fold s ~init:0 ~f:(fun n c -> n + if is_literal c || Char.equal c ' ' then 1 else 3)

let encode pairs =
  let size = List.fold pairs ~init:0 ~f:(fun n (key, value) ->
    n + encoded_length key + encoded_length value + 2) in
  if size = 0 then "" else
  let out = Stdlib.Bytes.create (size - 1) in
  let pos = ref 0 in
  let add c = Stdlib.Bytes.unsafe_set out !pos c; Stdlib.incr pos in
  let encode s = String.iter s ~f:(fun c ->
    if Char.equal c ' ' then add '+'
    else if is_literal c then add c
    else begin
      let v = Char.to_int c in
      add '%'; add (String.get hex_upper (v lsr 4)); add (String.get hex_upper (v land 15))
    end) in
  List.iteri pairs ~f:(fun i (key, value) ->
    if i > 0 then add '&';
    encode key; add '='; encode value);
  Stdlib.Bytes.unsafe_to_string out
;;

(* Count valid percent triplets first, preserving malformed escapes literally.
   The owned result has its final size and never needs a second string copy. *)
let decode_window (s : string @ local) ~off ~len =
  let stop = off + len in
  let triplet i =
    i + 2 < stop && Char.equal (String.get s i) '%'
    && Httpz_uri.Scanner.hex_val (String.get s (i + 1)) >= 0
    && Httpz_uri.Scanner.hex_val (String.get s (i + 2)) >= 0
  in
  let mutable i = off in
  let mutable size = 0 in
  while i < stop do
    size <- size + 1;
    i <- i + (if triplet i then 3 else 1)
  done;
  let dst = Stdlib.Bytes.create size in
  let mutable i = off in
  let mutable k = 0 in
  while i < stop do
    if triplet i then begin
      let v = Httpz_uri.Scanner.hex_val (String.get s (i + 1)) * 16
        + Httpz_uri.Scanner.hex_val (String.get s (i + 2)) in
      Stdlib.Bytes.unsafe_set dst k (Stdlib.Char.unsafe_chr v);
      i <- i + 3
    end else begin
      let c = String.get s i in
      Stdlib.Bytes.unsafe_set dst k (if Char.equal c '+' then ' ' else c);
      i <- i + 1
    end;
    k <- k + 1
  done;
  Stdlib.Bytes.unsafe_to_string dst
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
