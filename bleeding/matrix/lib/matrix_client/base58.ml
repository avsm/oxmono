let alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

let digit_of_char =
  let t = Array.make 256 (-1) in
  String.iteri (fun i c -> t.(Char.code c) <- i) alphabet;
  t

let divmod58 (digits : int array) (from : int) =
  let rem = ref 0 in
  for i = from to Array.length digits - 1 do
    let acc = (!rem * 256) + digits.(i) in
    digits.(i) <- acc / 58;
    rem := acc mod 58
  done;
  !rem

let divmod256 (digits : int array) (from : int) =
  let rem = ref 0 in
  for i = from to Array.length digits - 1 do
    let acc = (!rem * 58) + digits.(i) in
    digits.(i) <- acc / 256;
    rem := acc mod 256
  done;
  !rem

let leading_count s c =
  let n = String.length s in
  let rec go i = if i < n && String.get s i = c then go (i + 1) else i in
  go 0

let encode s =
  let len = String.length s in
  let zeros = leading_count s '\x00' in
  if zeros = len then String.make zeros '1'
  else begin
    let digits = Array.init len (fun i -> Char.code (String.get s i)) in
    let out = Buffer.create (len * 2) in
    (* [from] walks past bytes the division has zeroed, so repeated
       long division is O(n^2/2) rather than O(n^2). *)
    let from = ref zeros in
    while !from < len do
      Buffer.add_char out (String.get alphabet (divmod58 digits !from));
      while !from < len && digits.(!from) = 0 do
        incr from
      done
    done;
    let body = Buffer.contents out in
    let body_len = String.length body in
    String.make zeros '1'
    ^ String.init body_len (fun i -> String.get body (body_len - 1 - i))
  end

let decode s =
  let len = String.length s in
  let ones = leading_count s '1' in
  let bad = ref None in
  let digits =
    Array.init len (fun i ->
        let d = digit_of_char.(Char.code (String.get s i)) in
        if d < 0 && !bad = None then
          bad :=
            Some
              (`Msg
                 (Printf.sprintf "invalid base58 character %C" (String.get s i)));
        if d < 0 then 0 else d)
  in
  match !bad with
  | Some e -> Error e
  | None ->
      if ones = len then Ok (String.make ones '\x00')
      else begin
        let out = Buffer.create (len + 1) in
        let from = ref ones in
        while !from < len do
          Buffer.add_char out (Char.chr (divmod256 digits !from));
          while !from < len && digits.(!from) = 0 do
            incr from
          done
        done;
        let body = Buffer.contents out in
        let body_len = String.length body in
        Ok
          (String.make ones '\x00'
          ^ String.init body_len (fun i -> String.get body (body_len - 1 - i)))
      end

type error = [ `Msg of string ]

let header = "\x8b\x01"
let parity s = String.fold_left (fun acc c -> acc lxor Char.code c) 0 s

let group_by_four s =
  let n = String.length s in
  let b = Buffer.create (n + (n / 4)) in
  String.iteri
    (fun i c ->
      if i > 0 && i mod 4 = 0 then Buffer.add_char b ' ';
      Buffer.add_char b c)
    s;
  Buffer.contents b

let encode_key key =
  let framed = header ^ key in
  let framed = framed ^ String.make 1 (Char.chr (parity framed)) in
  group_by_four (encode framed)

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let strip_whitespace s =
  String.to_seq s |> Seq.filter (fun c -> not (is_space c)) |> String.of_seq

let decode_key s =
  match decode (strip_whitespace s) with
  | Error _ as e -> e
  | Ok bytes ->
      let n = String.length bytes in
      (* 2-byte header + parity, the frame [encode_key] produces for a
         zero-length key. *)
      if n < 3 then Error (`Msg "recovery key too short")
      else if String.sub bytes 0 2 <> header then
        Error (`Msg "unexpected recovery key header")
      else if parity bytes <> 0 then Error (`Msg "parity check failed")
      else Ok (String.sub bytes 2 (n - 3))
