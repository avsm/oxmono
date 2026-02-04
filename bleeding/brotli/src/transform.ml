(* Brotli dictionary word transformations (RFC 7932 Appendix B) *)

(* Transform types *)
type transform_type =
  | Identity
  | OmitLast of int   (* 1-9 *)
  | UppercaseFirst
  | UppercaseAll
  | OmitFirst of int  (* 1-9 *)

(* A complete transform with prefix, type, and suffix *)
type transform = {
  prefix : string;
  transform : transform_type;
  suffix : string;
}

(* UTF-8 uppercase conversion as specified in RFC 7932 *)
let to_upper_case dst pos =
  let c = Char.code (Bytes.get dst pos) in
  if c < 0xc0 then begin
    (* ASCII or continuation byte *)
    if c >= 97 && c <= 122 then
      Bytes.set dst pos (Char.chr (c lxor 32));
    1
  end
  else if c < 0xe0 then begin
    (* 2-byte UTF-8 sequence *)
    let c1 = Char.code (Bytes.get dst (pos + 1)) in
    Bytes.set dst (pos + 1) (Char.chr (c1 lxor 32));
    2
  end
  else begin
    (* 3-byte UTF-8 sequence *)
    let c2 = Char.code (Bytes.get dst (pos + 2)) in
    Bytes.set dst (pos + 2) (Char.chr (c2 lxor 5));
    3
  end

(* All 121 transforms from RFC 7932 *)
let transforms = [|
  { prefix = ""; transform = Identity; suffix = "" };
  { prefix = ""; transform = Identity; suffix = " " };
  { prefix = " "; transform = Identity; suffix = " " };
  { prefix = ""; transform = OmitFirst 1; suffix = "" };
  { prefix = ""; transform = UppercaseFirst; suffix = " " };
  { prefix = ""; transform = Identity; suffix = " the " };
  { prefix = " "; transform = Identity; suffix = "" };
  { prefix = "s "; transform = Identity; suffix = " " };
  { prefix = ""; transform = Identity; suffix = " of " };
  { prefix = ""; transform = UppercaseFirst; suffix = "" };
  { prefix = ""; transform = Identity; suffix = " and " };
  { prefix = ""; transform = OmitFirst 2; suffix = "" };
  { prefix = ""; transform = OmitLast 1; suffix = "" };
  { prefix = ", "; transform = Identity; suffix = " " };
  { prefix = ""; transform = Identity; suffix = ", " };
  { prefix = " "; transform = UppercaseFirst; suffix = " " };
  { prefix = ""; transform = Identity; suffix = " in " };
  { prefix = ""; transform = Identity; suffix = " to " };
  { prefix = "e "; transform = Identity; suffix = " " };
  { prefix = ""; transform = Identity; suffix = "\"" };
  { prefix = ""; transform = Identity; suffix = "." };
  { prefix = ""; transform = Identity; suffix = "\">" };
  { prefix = ""; transform = Identity; suffix = "\n" };
  { prefix = ""; transform = OmitLast 3; suffix = "" };
  { prefix = ""; transform = Identity; suffix = "]" };
  { prefix = ""; transform = Identity; suffix = " for " };
  { prefix = ""; transform = OmitFirst 3; suffix = "" };
  { prefix = ""; transform = OmitLast 2; suffix = "" };
  { prefix = ""; transform = Identity; suffix = " a " };
  { prefix = ""; transform = Identity; suffix = " that " };
  { prefix = " "; transform = UppercaseFirst; suffix = "" };
  { prefix = ""; transform = Identity; suffix = ". " };
  { prefix = "."; transform = Identity; suffix = "" };
  { prefix = " "; transform = Identity; suffix = ", " };
  { prefix = ""; transform = OmitFirst 4; suffix = "" };
  { prefix = ""; transform = Identity; suffix = " with " };
  { prefix = ""; transform = Identity; suffix = "'" };
  { prefix = ""; transform = Identity; suffix = " from " };
  { prefix = ""; transform = Identity; suffix = " by " };
  { prefix = ""; transform = OmitFirst 5; suffix = "" };
  { prefix = ""; transform = OmitFirst 6; suffix = "" };
  { prefix = " the "; transform = Identity; suffix = "" };
  { prefix = ""; transform = OmitLast 4; suffix = "" };
  { prefix = ""; transform = Identity; suffix = ". The " };
  { prefix = ""; transform = UppercaseAll; suffix = "" };
  { prefix = ""; transform = Identity; suffix = " on " };
  { prefix = ""; transform = Identity; suffix = " as " };
  { prefix = ""; transform = Identity; suffix = " is " };
  { prefix = ""; transform = OmitLast 7; suffix = "" };
  { prefix = ""; transform = OmitLast 1; suffix = "ing " };
  { prefix = ""; transform = Identity; suffix = "\n\t" };
  { prefix = ""; transform = Identity; suffix = ":" };
  { prefix = " "; transform = Identity; suffix = ". " };
  { prefix = ""; transform = Identity; suffix = "ed " };
  { prefix = ""; transform = OmitFirst 9; suffix = "" };
  { prefix = ""; transform = OmitFirst 7; suffix = "" };
  { prefix = ""; transform = OmitLast 6; suffix = "" };
  { prefix = ""; transform = Identity; suffix = "(" };
  { prefix = ""; transform = UppercaseFirst; suffix = ", " };
  { prefix = ""; transform = OmitLast 8; suffix = "" };
  { prefix = ""; transform = Identity; suffix = " at " };
  { prefix = ""; transform = Identity; suffix = "ly " };
  { prefix = " the "; transform = Identity; suffix = " of " };
  { prefix = ""; transform = OmitLast 5; suffix = "" };
  { prefix = ""; transform = OmitLast 9; suffix = "" };
  { prefix = " "; transform = UppercaseFirst; suffix = ", " };
  { prefix = ""; transform = UppercaseFirst; suffix = "\"" };
  { prefix = "."; transform = Identity; suffix = "(" };
  { prefix = ""; transform = UppercaseAll; suffix = " " };
  { prefix = ""; transform = UppercaseFirst; suffix = "\">" };
  { prefix = ""; transform = Identity; suffix = "=\"" };
  { prefix = " "; transform = Identity; suffix = "." };
  { prefix = ".com/"; transform = Identity; suffix = "" };
  { prefix = " the "; transform = Identity; suffix = " of the " };
  { prefix = ""; transform = UppercaseFirst; suffix = "'" };
  { prefix = ""; transform = Identity; suffix = ". This " };
  { prefix = ""; transform = Identity; suffix = "," };
  { prefix = "."; transform = Identity; suffix = " " };
  { prefix = ""; transform = UppercaseFirst; suffix = "(" };
  { prefix = ""; transform = UppercaseFirst; suffix = "." };
  { prefix = ""; transform = Identity; suffix = " not " };
  { prefix = " "; transform = Identity; suffix = "=\"" };
  { prefix = ""; transform = Identity; suffix = "er " };
  { prefix = " "; transform = UppercaseAll; suffix = " " };
  { prefix = ""; transform = Identity; suffix = "al " };
  { prefix = " "; transform = UppercaseAll; suffix = "" };
  { prefix = ""; transform = Identity; suffix = "='" };
  { prefix = ""; transform = UppercaseAll; suffix = "\"" };
  { prefix = ""; transform = UppercaseFirst; suffix = ". " };
  { prefix = " "; transform = Identity; suffix = "(" };
  { prefix = ""; transform = Identity; suffix = "ful " };
  { prefix = " "; transform = UppercaseFirst; suffix = ". " };
  { prefix = ""; transform = Identity; suffix = "ive " };
  { prefix = ""; transform = Identity; suffix = "less " };
  { prefix = ""; transform = UppercaseAll; suffix = "'" };
  { prefix = ""; transform = Identity; suffix = "est " };
  { prefix = " "; transform = UppercaseFirst; suffix = "." };
  { prefix = ""; transform = UppercaseAll; suffix = "\">" };
  { prefix = " "; transform = Identity; suffix = "='" };
  { prefix = ""; transform = UppercaseFirst; suffix = "," };
  { prefix = ""; transform = Identity; suffix = "ize " };
  { prefix = ""; transform = UppercaseAll; suffix = "." };
  { prefix = "\194\160"; transform = Identity; suffix = "" };  (* non-breaking space UTF-8 *)
  { prefix = " "; transform = Identity; suffix = "," };
  { prefix = ""; transform = UppercaseFirst; suffix = "=\"" };
  { prefix = ""; transform = UppercaseAll; suffix = "=\"" };
  { prefix = ""; transform = Identity; suffix = "ous " };
  { prefix = ""; transform = UppercaseAll; suffix = ", " };
  { prefix = ""; transform = UppercaseFirst; suffix = "='" };
  { prefix = " "; transform = UppercaseFirst; suffix = "," };
  { prefix = " "; transform = UppercaseAll; suffix = "=\"" };
  { prefix = " "; transform = UppercaseAll; suffix = ", " };
  { prefix = ""; transform = UppercaseAll; suffix = "," };
  { prefix = ""; transform = UppercaseAll; suffix = "(" };
  { prefix = ""; transform = UppercaseAll; suffix = ". " };
  { prefix = " "; transform = UppercaseAll; suffix = "." };
  { prefix = ""; transform = UppercaseAll; suffix = "='" };
  { prefix = " "; transform = UppercaseAll; suffix = ". " };
  { prefix = " "; transform = UppercaseFirst; suffix = "=\"" };
  { prefix = " "; transform = UppercaseAll; suffix = "='" };
  { prefix = " "; transform = UppercaseFirst; suffix = "='" };
|]

let num_transforms = Array.length transforms

(* Apply a transform to a dictionary word *)
let apply_transform dst dst_pos word word_offset word_length transform_id =
  let t = transforms.(transform_id) in
  let prefix = t.prefix in
  let suffix = t.suffix in

  (* Calculate skip amount for OmitFirst transforms *)
  let skip = match t.transform with
    | OmitFirst n -> min n word_length
    | _ -> 0
  in

  (* Calculate length reduction for OmitLast transforms *)
  let omit_last = match t.transform with
    | OmitLast n -> n
    | _ -> 0
  in

  let actual_word_len = word_length - skip - omit_last in
  let actual_word_len = max 0 actual_word_len in

  let mutable idx = dst_pos in

  (* Write prefix *)
  for i = 0 to String.length prefix - 1 do
    Bytes.set dst idx prefix.[i];
    idx <- idx + 1
  done;

  (* Write word (possibly skipped and/or truncated) *)
  let word_start = word_offset + skip in
  for i = 0 to actual_word_len - 1 do
    Bytes.set dst idx (String.get word (word_start + i));
    idx <- idx + 1
  done;

  (* Apply uppercase transformations *)
  let uppercase_start = dst_pos + String.length prefix in
  begin match t.transform with
    | UppercaseFirst ->
      if actual_word_len > 0 then
        ignore (to_upper_case dst uppercase_start)
    | UppercaseAll ->
      let mutable remaining = actual_word_len in
      let mutable pos = uppercase_start in
      while remaining > 0 do
        let step = to_upper_case dst pos in
        pos <- pos + step;
        remaining <- remaining - step
      done
    | _ -> ()
  end;

  (* Write suffix *)
  for i = 0 to String.length suffix - 1 do
    Bytes.set dst idx suffix.[i];
    idx <- idx + 1
  done;

  idx - dst_pos

(* Transform a dictionary word in place *)
let transform_dictionary_word ~dst ~dst_pos ~word_index ~word_length ~transform_id =
  let word = Dictionary.data in
  let word_offset = Dictionary.get_offset_by_length word_length + word_index * word_length in
  apply_transform dst dst_pos word word_offset word_length transform_id
