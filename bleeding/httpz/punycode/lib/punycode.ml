let base = 36
let tmin = 1
let tmax = 26
let skew = 38
let damp = 700
let initial_bias = 72
let initial_n = 0x80
let delimiter = '-'
let ace_prefix = "xn--"
let max_label_length = 63

type position =
  { byte_offset : int
  ; char_index : int
  }

let position_byte_offset pos = pos.byte_offset
let position_char_index pos = pos.char_index

let pp_position fmt pos =
  Format.fprintf fmt "byte %d, char %d" pos.byte_offset pos.char_index
;;

type error_reason =
  | Overflow of position
  | Invalid_character of position * Uchar.t
  | Invalid_digit of position * char
  | Unexpected_end of position
  | Invalid_utf8 of position
  | Label_too_long of int
  | Empty_label

let pp_error_reason fmt = function
  | Overflow pos -> Format.fprintf fmt "arithmetic overflow at %a" pp_position pos
  | Invalid_character (pos, u) ->
    Format.fprintf fmt "invalid character U+%04X at %a" (Uchar.to_int u) pp_position pos
  | Invalid_digit (pos, c) ->
    Format.fprintf
      fmt
      "invalid Punycode digit '%c' (0x%02X) at %a"
      c
      (Char.code c)
      pp_position
      pos
  | Unexpected_end pos ->
    Format.fprintf fmt "unexpected end of input at %a" pp_position pos
  | Invalid_utf8 pos -> Format.fprintf fmt "invalid UTF-8 sequence at %a" pp_position pos
  | Label_too_long len ->
    Format.fprintf fmt "label too long: %d bytes (max %d)" len max_label_length
  | Empty_label -> Format.fprintf fmt "empty label"
;;

exception Error of error_reason

let () =
  Printexc.Safe.register_printer (function
    | Error reason -> Some (Format.asprintf "Punycode.Error: %a" pp_error_reason reason)
    | _ -> None)
;;

let error_reason_to_string reason = Format.asprintf "%a" pp_error_reason reason
let overflow pos = raise (Error (Overflow pos))
let invalid_character pos u = raise (Error (Invalid_character (pos, u)))
let invalid_digit pos c = raise (Error (Invalid_digit (pos, c)))
let unexpected_end pos = raise (Error (Unexpected_end pos))
let invalid_utf8 pos = raise (Error (Invalid_utf8 pos))
let label_too_long len = raise (Error (Label_too_long len))
let empty_label () = raise (Error Empty_label)

type case_flag =
  | Uppercase
  | Lowercase

let is_basic u = Uchar.to_int u < 0x80
let is_ascii_string s = String.for_all (fun c -> Char.code c < 0x80) s

let has_ace_prefix s =
  let len = String.length s in
  len >= 4
  && (s.[0] = 'x' || s.[0] = 'X')
  && (s.[1] = 'n' || s.[1] = 'N')
  && s.[2] = '-'
  && s.[3] = '-'
;;

(* Punycode digits map 0--25 to ASCII letters and 26--35 to decimal digits. *)

let encode_digit d case_flag =
  if d < 26
  then Char.chr (d + if case_flag = Uppercase then 0x41 else 0x61)
  else Char.chr (d - 26 + 0x30)
;;

let decode_digit c =
  let code = Char.code c in
  if code >= 0x30 && code <= 0x39
  then Some (code - 0x30 + 26)
  else if code >= 0x41 && code <= 0x5A
  then Some (code - 0x41)
  else if code >= 0x61 && code <= 0x7A
  then Some (code - 0x61)
  else None
;;

let is_flagged c =
  let code = Char.code c in
  code >= 0x41 && code <= 0x5A
;;

(* RFC 3492, Section 6.1 bias adaptation. *)
let adapt ~delta ~numpoints ~firsttime =
  let delta = if firsttime then delta / damp else delta / 2 in
  let delta = delta + (delta / numpoints) in
  let threshold = (base - tmin) * tmax / 2 in
  let rec loop delta k =
    if delta > threshold
    then loop (delta / (base - tmin)) (k + base)
    else k + ((base - tmin + 1) * delta / (delta + skew))
  in
  loop delta 0
;;

(* Accumulate a generalized variable-length integer without wrapping. *)
let safe_mul_add a b c pos =
  if c = 0 then a else if b > (max_int - a) / c then overflow pos else a + (b * c)
;;

let utf8_to_codepoints s =
  let len = String.length s in
  let acc = ref [] in
  let byte_offset = ref 0 in
  let char_index = ref 0 in
  while !byte_offset < len do
    let pos = { byte_offset = !byte_offset; char_index = !char_index } in
    let dec = String.get_utf_8_uchar s !byte_offset in
    if Uchar.utf_decode_is_valid dec
    then (
      acc := Uchar.utf_decode_uchar dec :: !acc;
      byte_offset := !byte_offset + Uchar.utf_decode_length dec;
      incr char_index)
    else invalid_utf8 pos
  done;
  Array.of_list (List.rev !acc)
;;

let codepoints_to_utf8 codepoints =
  let buf = Buffer.create (Array.length codepoints * 2) in
  Array.iter (Buffer.add_utf_8_uchar buf) codepoints;
  Buffer.contents buf
;;

(* Emit basic code points first, then encode the others in ascending order as the deltas
   defined by RFC 3492, Section 6.3. *)
let encode_impl codepoints case_flags =
  let input_length = Array.length codepoints in
  if input_length = 0
  then ""
  else (
    let output = Buffer.create (input_length * 2) in
    let basic_count = ref 0 in
    for j = 0 to input_length - 1 do
      let cp = codepoints.(j) in
      if is_basic cp
      then (
        let c = Uchar.to_int cp in
        let case =
          match case_flags with
          | Some flags -> flags.(j)
          | None -> Lowercase
        in
        let c' =
          if c >= 0x41 && c <= 0x5A
          then if case = Lowercase then c + 0x20 else c
          else if c >= 0x61 && c <= 0x7A
          then if case = Uppercase then c - 0x20 else c
          else c
        in
        Buffer.add_char output (Char.chr c');
        incr basic_count)
    done;
    let b = !basic_count in
    let h = ref b in
    if b > 0 then Buffer.add_char output delimiter;
    let n = ref initial_n in
    let delta = ref 0 in
    let bias = ref initial_bias in
    while !h < input_length do
      let m =
        Array.fold_left
          (fun acc cp ->
            let cp_val = Uchar.to_int cp in
            if cp_val >= !n && cp_val < acc then cp_val else acc)
          max_int
          codepoints
      in
      let pos = { byte_offset = 0; char_index = !h } in
      delta := safe_mul_add !delta (m - !n) (!h + 1) pos;
      n := m;
      for j = 0 to input_length - 1 do
        let cp = Uchar.to_int codepoints.(j) in
        let pos = { byte_offset = 0; char_index = j } in
        if cp < !n
        then (
          if !delta = max_int then overflow pos else incr delta)
        else if cp = !n
        then (
          let q = ref !delta in
          let k = ref base in
          let done_encoding = ref false in
          while not !done_encoding do
            let t =
              if !k <= !bias
              then tmin
              else if !k >= !bias + tmax
              then tmax
              else !k - !bias
            in
            if !q < t
            then (
              let case =
                match case_flags with
                | Some flags -> flags.(j)
                | None -> Lowercase
              in
              Buffer.add_char output (encode_digit !q case);
              done_encoding := true)
            else (
              let digit = t + ((!q - t) mod (base - t)) in
              Buffer.add_char output (encode_digit digit Lowercase);
              q := (!q - t) / (base - t);
              k := !k + base)
          done;
          bias := adapt ~delta:!delta ~numpoints:(!h + 1) ~firsttime:(!h = b);
          delta := 0;
          incr h)
      done;
      let pos = { byte_offset = 0; char_index = !h } in
      if !delta = max_int || !n = max_int then overflow pos;
      incr delta;
      incr n
    done;
    Buffer.contents output)
;;

let encode codepoints = encode_impl codepoints None

let encode_with_case codepoints case_flags =
  if Array.length codepoints <> Array.length case_flags
  then invalid_arg "encode_with_case: array lengths must match";
  encode_impl codepoints (Some case_flags)
;;

(* Decode each generalized integer and insert its code point at the position carried by
   the delta. *)
let decode_impl input =
  let input_length = String.length input in
  if input_length = 0
  then [||], [||]
  else (
    let b = Option.value ~default:0 (String.rindex_opt input delimiter) in
    let output = ref [] in
    let case_output = ref [] in
    for j = 0 to b - 1 do
      let c = input.[j] in
      let pos = { byte_offset = j; char_index = j } in
      let code = Char.code c in
      if code >= 0x80
      then invalid_character pos (Uchar.of_int code)
      else (
        output := Uchar.of_int code :: !output;
        case_output := (if is_flagged c then Uppercase else Lowercase) :: !case_output)
    done;
    let output = ref (Array.of_list (List.rev !output)) in
    let case_output = ref (Array.of_list (List.rev !case_output)) in
    let n = ref initial_n in
    let i = ref 0 in
    let bias = ref initial_bias in
    let in_pos = ref (if b > 0 then b + 1 else 0) in
    while !in_pos < input_length do
      let oldi = !i in
      let w = ref 1 in
      let k = ref base in
      let done_decoding = ref false in
      while not !done_decoding do
        let pos = { byte_offset = !in_pos; char_index = Array.length !output } in
        if !in_pos >= input_length
        then unexpected_end pos
        else (
          let c = input.[!in_pos] in
          incr in_pos;
          match decode_digit c with
          | None -> invalid_digit pos c
          | Some digit ->
            i := safe_mul_add !i digit !w pos;
            let t =
              if !k <= !bias
              then tmin
              else if !k >= !bias + tmax
              then tmax
              else !k - !bias
            in
            if digit < t
            then done_decoding := true
            else (
              let base_minus_t = base - t in
              if !w > max_int / base_minus_t
              then overflow pos
              else (
                w := !w * base_minus_t;
                k := !k + base)))
      done;
      let out_len = Array.length !output in
      bias := adapt ~delta:(!i - oldi) ~numpoints:(out_len + 1) ~firsttime:(oldi = 0);
      let pos = { byte_offset = !in_pos - 1; char_index = out_len } in
      let increment = !i / (out_len + 1) in
      if increment > max_int - !n
      then overflow pos
      else (
        n := !n + increment;
        i := !i mod (out_len + 1);
        if not (Uchar.is_valid !n)
        then invalid_character pos Uchar.rep
        else (
          let new_output = Array.make (out_len + 1) (Uchar.of_int 0) in
          let new_case = Array.make (out_len + 1) Lowercase in
          for j = 0 to !i - 1 do
            new_output.(j) <- !output.(j);
            new_case.(j) <- !case_output.(j)
          done;
          new_output.(!i) <- Uchar.of_int !n;
          new_case.(!i)
          <- (if !in_pos > 0 && is_flagged input.[!in_pos - 1]
              then Uppercase
              else Lowercase);
          for j = !i to out_len - 1 do
            new_output.(j + 1) <- !output.(j);
            new_case.(j + 1) <- !case_output.(j)
          done;
          output := new_output;
          case_output := new_case;
          incr i))
    done;
    !output, !case_output)
;;

let decode input = fst (decode_impl input)
let decode_with_case input = decode_impl input

let encode_utf8 s =
  let codepoints = utf8_to_codepoints s in
  encode codepoints
;;

let decode_utf8 punycode =
  let codepoints = decode punycode in
  codepoints_to_utf8 codepoints
;;

let encode_label label =
  if String.length label = 0
  then empty_label ()
  else if is_ascii_string label
  then (
    let len = String.length label in
    if len > max_label_length then label_too_long len else label)
  else (
    let encoded = encode_utf8 label in
    let result = ace_prefix ^ encoded in
    let len = String.length result in
    if len > max_label_length then label_too_long len else result)
;;

let decode_label label =
  let len = String.length label in
  if len = 0
  then empty_label ()
  else if len > max_label_length
  then label_too_long len
  else if has_ace_prefix label
  then (
    let punycode = String.sub label 4 (len - 4) in
    decode_utf8 punycode)
  else label
;;
