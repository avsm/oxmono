let max_domain_length = 253

type error_reason =
  | Punycode_error of Punycode.error_reason
  | Invalid_label of string
  | Domain_too_long of int
  | Verification_failed

let pp_error_reason fmt = function
  | Punycode_error e ->
      Format.fprintf fmt "Punycode error: %a" Punycode.pp_error_reason e
  | Invalid_label msg -> Format.fprintf fmt "invalid label: %s" msg
  | Domain_too_long len ->
      Format.fprintf fmt "domain too long: %d bytes (max %d)" len
        max_domain_length
  | Verification_failed ->
      Format.fprintf fmt "IDNA verification failed (round-trip mismatch)"

exception Error of error_reason

let () = Printexc.Safe.register_printer (function
  | Error reason -> Some (Format.asprintf "Punycode_idna.Error: %a" pp_error_reason reason)
  | _ -> None)

let error_reason_to_string reason = Format.asprintf "%a" pp_error_reason reason

let punycode_error e = raise (Error (Punycode_error e))
let invalid_label msg = raise (Error (Invalid_label msg))
let domain_too_long len = raise (Error (Domain_too_long len))
let verification_failed () = raise (Error Verification_failed)

let normalize_nfc s = Uunf_string.normalize_utf_8 `NFC s

let validate_utf8 s =
  let rec loop byte =
    if byte = String.length s then ()
    else
      let decoded = String.get_utf_8_uchar s byte in
      if not (Uchar.utf_decode_is_valid decoded) then
        invalid_label (Printf.sprintf "malformed UTF-8 at byte %d" byte)
      else loop (byte + Uchar.utf_decode_length decoded)
  in
  loop 0

let max_ulabel_input_bytes = 1024

(* STD3 labels contain only letters, digits, and interior hyphens. *)
let is_std3_valid label =
  let len = String.length label in
  let is_ldh c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c = '-'
  in
  len > 0
  && label.[0] <> '-'
  && label.[len - 1] <> '-'
  && String.for_all is_ldh label

let check_hyphen_rules label =
  let len = String.length label in
  let positions_3_4 =
    let byte = ref 0 in
    let char = ref 0 in
    let third = ref false in
    let fourth = ref false in
    let valid = ref true in
    while !valid && !byte < len && !char < 4 do
      let decoded = String.get_utf_8_uchar label !byte in
      if not (Uchar.utf_decode_is_valid decoded)
      then valid := false
      else (
        let hyphen = Uchar.to_int (Uchar.utf_decode_uchar decoded) = Char.code '-' in
        if !char = 2 then third := hyphen;
        if !char = 3 then fourth := hyphen;
        byte := !byte + Uchar.utf_decode_length decoded;
        incr char)
    done;
    !third && !fourth
  in
  len > 0
  && label.[0] <> '-'
  && label.[len - 1] <> '-'
  && (not positions_3_4 || Punycode.has_ace_prefix label)

(* An apparent A-label must be a canonical encoding of a U-label, not merely
   ASCII beginning [xn--]. This rejects empty and fake ACE payloads before a
   DNS or public-suffix policy can give them special treatment. *)
let decode_alabel label =
  let len = String.length label in
  let payload = String.sub label 4 (len - 4) in
  if payload = "" then invalid_label "empty A-label payload";
  let decoded =
    try Punycode.decode_utf8 payload
    with Punycode.Error e -> punycode_error e
  in
  if Punycode.is_ascii_string decoded then
    invalid_label "A-label decodes to ASCII only";
  let normalized = normalize_nfc decoded in
  let encoded =
    try Punycode.encode_utf8 normalized
    with Punycode.Error e -> punycode_error e
  in
  if
    not
      (String.equal
         (String.lowercase_ascii payload)
         (String.lowercase_ascii encoded))
  then verification_failed ();
  normalized

let label_to_ascii_impl ~check_hyphens ~use_std3_rules label =
  let len = String.length label in
  if len = 0 then invalid_label "empty label"
  else if Punycode.is_ascii_string label then begin
    if len > Punycode.max_label_length then
      punycode_error (Punycode.Label_too_long len);
    if check_hyphens && not (check_hyphen_rules label) then
      invalid_label "invalid hyphen placement"
    else if use_std3_rules && not (is_std3_valid label) then
      invalid_label "STD3 rules violation"
    else (
      if Punycode.has_ace_prefix label then ignore (decode_alabel label);
      label)
  end
  else begin
    validate_utf8 label;
    if len > max_ulabel_input_bytes then invalid_label "U-label input is too large";
    let normalized = normalize_nfc label in
    if check_hyphens && not (check_hyphen_rules normalized) then
      invalid_label "invalid hyphen placement";
    let encoded =
      try Punycode.encode_utf8 normalized
      with Punycode.Error e -> punycode_error e
    in
    let result = Punycode.ace_prefix ^ encoded in
    let result_len = String.length result in
    if result_len > Punycode.max_label_length then
      punycode_error (Punycode.Label_too_long result_len)
    else
      let decoded =
        try Punycode.decode_utf8 encoded
        with Punycode.Error _ -> verification_failed ()
      in
      if decoded <> normalized then verification_failed () else result
  end

let label_to_ascii ?(check_hyphens = true) ?(use_std3_rules = false) label =
  label_to_ascii_impl ~check_hyphens ~use_std3_rules label

(* RFC 1034 caps a DNS label at 63 octets and RFC 5890 2.3.2.1 carries that to
   the A-label, so an over-long label names nothing that can be looked up. The
   cap also bounds Punycode decoding, which is quadratic in the payload. *)
let label_to_unicode label =
  let len = String.length label in
  if Punycode.has_ace_prefix label then begin
    if len > Punycode.max_label_length then
      punycode_error (Punycode.Label_too_long len);
    decode_alabel label
  end else if Punycode.is_ascii_string label then begin
    if len > Punycode.max_label_length then
      punycode_error (Punycode.Label_too_long len);
    label
  end else begin
    validate_utf8 label;
    if len > max_ulabel_input_bytes then invalid_label "U-label input is too large";
    label
  end

let join_labels labels = String.concat "." labels

let domain_parts domain =
  validate_utf8 domain;
  let len = String.length domain in
  let rooted = len > 0 && domain.[len - 1] = '.' in
  let body = if rooted then String.sub domain 0 (len - 1) else domain in
  (if rooted && body = "" then [] else String.split_on_char '.' body), rooted

let to_ascii ?(check_hyphens = true) ?(use_std3_rules = false) domain =
  let labels, rooted = domain_parts domain in
  let encoded_labels =
    List.map (label_to_ascii_impl ~check_hyphens ~use_std3_rules) labels
  in
  let body = join_labels encoded_labels in
  let len = String.length body in
  if len > max_domain_length then domain_too_long len
  else if rooted then body ^ "." else body

let to_unicode domain =
  let labels, rooted = domain_parts domain in
  let decoded_labels = List.map label_to_unicode labels in
  let body = join_labels decoded_labels in
  if rooted then body ^ "." else body
