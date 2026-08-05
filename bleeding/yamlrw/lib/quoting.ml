(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML scalar quoting detection *)

(** Check if a string value needs quoting in YAML output. Returns true if the
    string:
    - Is empty
    - Starts with an indicator character
    - Is a reserved word (null, true, false, yes, no, etc.)
    - Contains characters that would be ambiguous
    - Looks like a number *)
let needs_quoting s =
  if String.length s = 0 then true
  else
    let first = s.[0] in
    (* Check first character for indicators *)
    if
      first = '-' || first = '?' || first = ':' || first = ',' || first = '['
      || first = ']' || first = '{' || first = '}' || first = '#' || first = '&'
      || first = '*' || first = '!' || first = '|' || first = '>'
      || first = '\'' || first = '"' || first = '%' || first = '@'
      || first = '`' || first = ' '
    then true
    else
      (* Check for reserved/special values *)
      let lower = String.lowercase_ascii s in
      if
        lower = "null" || lower = "true" || lower = "false" || lower = "yes"
        || lower = "no" || lower = "on" || lower = "off" || lower = "~"
        || lower = ".inf" || lower = "-.inf" || lower = ".nan"
      then true
      else
        (* Check for problematic characters *)
        try
          String.iter
            (fun c ->
              if c = ':' || c = '#' || c = '\n' || c = '\r' then raise Exit)
            s;
          (* Check if it looks like a number *)
          try
            ignore (Float.of_string s);
            true
          with _ -> false
        with Exit -> true

(** Check if a string requires double quotes (vs single quotes). Returns true if
    the string contains characters that need escape sequences. *)
let needs_double_quotes s =
  try
    String.iter
      (fun c ->
        if c = '\n' || c = '\r' || c = '\t' || c = '\\' || c < ' ' || c = '"'
        then raise Exit)
      s;
    false
  with Exit -> true

(** Choose the appropriate quoting style for a string value *)
let choose_style s =
  match (needs_double_quotes s, needs_quoting s) with
  | true, _ -> `Double_quoted
  | _, true -> `Single_quoted
  | _ -> `Plain
