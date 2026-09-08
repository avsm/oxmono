(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Apple Mail flag colors.

    See
    {{:https://datatracker.ietf.org/doc/draft-ietf-mailmaint-messageflag-mailboxattribute#section-3}
     draft-ietf-mailmaint-messageflag-mailboxattribute Section 3}.

    The Apple Mail flag color encoding uses three keywords to represent colors
    as a 3-bit pattern:
    - [$MailFlagBit0]: bit 0
    - [$MailFlagBit1]: bit 1
    - [$MailFlagBit2]: bit 2

    Bit patterns (bit0, bit1, bit2):
    - Red: (false, false, false) = 000
    - Orange: (true, false, false) = 100
    - Yellow: (false, true, false) = 010
    - Green: (true, true, false) = 110
    - Blue: (false, false, true) = 001
    - Purple: (true, false, true) = 101
    - Gray: (false, true, true) = 011
    - 111: undefined *)

type t =
  [ `Red  (** Bit pattern: 000 *)
  | `Orange  (** Bit pattern: 100 *)
  | `Yellow  (** Bit pattern: 010 *)
  | `Green  (** Bit pattern: 110 *)
  | `Blue  (** Bit pattern: 001 *)
  | `Purple  (** Bit pattern: 101 *)
  | `Gray  (** Bit pattern: 011 *) ]

let to_keywords = function
  | `Red -> []
  | `Orange -> [ `MailFlagBit0 ]
  | `Yellow -> [ `MailFlagBit1 ]
  | `Green -> [ `MailFlagBit0; `MailFlagBit1 ]
  | `Blue -> [ `MailFlagBit2 ]
  | `Purple -> [ `MailFlagBit0; `MailFlagBit2 ]
  | `Gray -> [ `MailFlagBit1; `MailFlagBit2 ]

let of_keywords_default_red
    (keywords : [ `MailFlagBit0 | `MailFlagBit1 | `MailFlagBit2 ] list) =
  let has k = List.exists (fun x -> x = k) keywords in
  match (has `MailFlagBit0, has `MailFlagBit1, has `MailFlagBit2) with
  | false, false, false -> Some `Red
  | true, false, false -> Some `Orange
  | false, true, false -> Some `Yellow
  | true, true, false -> Some `Green
  | false, false, true -> Some `Blue
  | true, false, true -> Some `Purple
  | false, true, true -> Some `Gray
  | true, true, true -> None

let of_keywords keywords =
  (* An empty bit set is ambiguous between "no flag color" and red. *)
  match keywords with
  | [] -> None
  | _ -> of_keywords_default_red keywords

let to_string = function
  | `Red -> "red"
  | `Orange -> "orange"
  | `Yellow -> "yellow"
  | `Green -> "green"
  | `Blue -> "blue"
  | `Purple -> "purple"
  | `Gray -> "gray"

let of_string s =
  match String.lowercase_ascii s with
  | "red" -> Some `Red
  | "orange" -> Some `Orange
  | "yellow" -> Some `Yellow
  | "green" -> Some `Green
  | "blue" -> Some `Blue
  | "purple" -> Some `Purple
  | "gray" | "grey" -> Some `Gray
  | _ -> None

let pp ppf color = Format.pp_print_string ppf (to_string color)
let equal a b = a = b
let compare a b = Stdlib.compare a b
