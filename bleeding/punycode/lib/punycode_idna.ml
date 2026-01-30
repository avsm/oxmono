(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* IDNA (Internationalized Domain Names in Applications) Implementation *)

let max_domain_length = 253

(* {1 Error Types} *)

type error_reason =
  | Punycode_error of Punycode.error_reason
  | Invalid_label of string
  | Domain_too_long of int
  | Normalization_failed
  | Verification_failed

let pp_error_reason fmt = function
  | Punycode_error e ->
      Format.fprintf fmt "Punycode error: %a" Punycode.pp_error_reason e
  | Invalid_label msg -> Format.fprintf fmt "invalid label: %s" msg
  | Domain_too_long len ->
      Format.fprintf fmt "domain too long: %d bytes (max %d)" len
        max_domain_length
  | Normalization_failed -> Format.fprintf fmt "Unicode normalization failed"
  | Verification_failed ->
      Format.fprintf fmt "IDNA verification failed (round-trip mismatch)"

exception Error of error_reason

let () = Printexc.register_printer (function
  | Error reason -> Some (Format.asprintf "Punycode_idna.Error: %a" pp_error_reason reason)
  | _ -> None)

let error_reason_to_string reason = Format.asprintf "%a" pp_error_reason reason

(* {1 Error Constructors} *)

let punycode_error e = raise (Error (Punycode_error e))
let invalid_label msg = raise (Error (Invalid_label msg))
let domain_too_long len = raise (Error (Domain_too_long len))
let verification_failed () = raise (Error Verification_failed)

(* {1 Unicode Normalization} *)

let normalize_nfc s = Uunf_string.normalize_utf_8 `NFC s

(* {1 Validation Helpers} *)

let is_ace_label label = Punycode.has_ace_prefix label

(* Check if a label follows STD3 rules (hostname restrictions):
   - Only LDH (letters, digits, hyphens)
   - Cannot start or end with hyphen *)
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

(* Check hyphen placement: hyphens not in positions 3 and 4 (except for ACE) *)
let check_hyphen_rules label =
  let len = String.length label in
  if len >= 4 && label.[2] = '-' && label.[3] = '-' then
    (* Hyphens in positions 3 and 4 - only valid for ACE prefix *)
    is_ace_label label
  else true

(* {1 Label Operations} *)

let label_to_ascii_impl ~check_hyphens ~use_std3_rules label =
  let len = String.length label in
  if len = 0 then invalid_label "empty label"
  else if len > Punycode.max_label_length then
    punycode_error (Punycode.Label_too_long len)
  else if Punycode.is_ascii_string label then begin
    (* All ASCII - validate and pass through *)
    if use_std3_rules && not (is_std3_valid label) then
      invalid_label "STD3 rules violation"
    else if check_hyphens && not (check_hyphen_rules label) then
      invalid_label "invalid hyphen placement"
    else label
  end
  else begin
    (* Has non-ASCII - normalize and encode *)
    let normalized = normalize_nfc label in

    (* Encode to Punycode *)
    let encoded =
      try Punycode.encode_utf8 normalized
      with Punycode.Error e -> punycode_error e
    in
    let result = Punycode.ace_prefix ^ encoded in
    let result_len = String.length result in
    if result_len > Punycode.max_label_length then
      punycode_error (Punycode.Label_too_long result_len)
    else if check_hyphens && not (check_hyphen_rules result) then
      invalid_label "invalid hyphen placement in encoded label"
    else
      (* Verification: decode and compare to original normalized form *)
      let decoded =
        try Punycode.decode_utf8 encoded
        with Punycode.Error _ -> verification_failed ()
      in
      if decoded <> normalized then verification_failed () else result
  end

let label_to_ascii ?(check_hyphens = true) ?(use_std3_rules = false) label =
  label_to_ascii_impl ~check_hyphens ~use_std3_rules label

let label_to_unicode label =
  if is_ace_label label then begin
    let encoded = String.sub label 4 (String.length label - 4) in
    try Punycode.decode_utf8 encoded
    with Punycode.Error e -> punycode_error e
  end
  else label

(* {1 Domain Operations} *)

(* Split domain into labels *)
let split_domain domain = String.split_on_char '.' domain

(* Join labels into domain *)
let join_labels labels = String.concat "." labels

let to_ascii ?(check_hyphens = true) ?(check_bidi = false)
    ?(check_joiners = false) ?(use_std3_rules = false) ?(transitional = false)
    domain =
  (* Note: check_bidi, check_joiners, and transitional are accepted but
     not fully implemented - they would require additional Unicode data *)
  let _ = check_bidi in
  let _ = check_joiners in
  let _ = transitional in

  let labels = split_domain domain in
  let encoded_labels =
    List.map (label_to_ascii_impl ~check_hyphens ~use_std3_rules) labels
  in
  let result = join_labels encoded_labels in
  let len = String.length result in
  if len > max_domain_length then domain_too_long len else result

let to_unicode domain =
  let labels = split_domain domain in
  let decoded_labels = List.map label_to_unicode labels in
  join_labels decoded_labels

(* {1 Domain Name Library Integration} *)

let domain_to_ascii ?(check_hyphens = true) ?(use_std3_rules = false) domain =
  let s = Domain_name.to_string domain in
  let ascii = to_ascii ~check_hyphens ~use_std3_rules s in
  match Domain_name.of_string ascii with
  | Error (`Msg msg) -> invalid_label msg
  | Ok d -> d

let domain_to_unicode domain =
  let s = Domain_name.to_string domain in
  let unicode = to_unicode s in
  match Domain_name.of_string unicode with
  | Error (`Msg msg) -> invalid_label msg
  | Ok d -> d

(* {1 Validation} *)

let is_idna_valid domain =
  try ignore (to_ascii domain); true
  with Error _ -> false
