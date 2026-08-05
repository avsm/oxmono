(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = {
  verbose : bool;
  user : string;
  host : string option;
}

let strip_crlf s =
  let len = String.length s in
  if len >= 2 && s.[len - 2] = '\r' && s.[len - 1] = '\n' then
    String.sub s 0 (len - 2)
  else if len >= 1 && (s.[len - 1] = '\r' || s.[len - 1] = '\n') then
    String.sub s 0 (len - 1)
  else s

let parse line =
  let line = String.trim (strip_crlf line) in
  if line = "" then { verbose = false; user = ""; host = None }
  else
    let verbose, rest =
      if String.length line >= 2
         && line.[0] = '/'
         && (line.[1] = 'W' || line.[1] = 'w')
      then
        let rest = String.trim (String.sub line 2 (String.length line - 2)) in
        (true, rest)
      else (false, line)
    in
    if rest = "" then { verbose; user = ""; host = None }
    else
      match String.rindex_opt rest '@' with
      | Some i ->
        let user = String.trim (String.sub rest 0 i) in
        let host = String.trim (String.sub rest (i + 1)
                                  (String.length rest - i - 1)) in
        { verbose; user; host = if host = "" then None else Some host }
      | None ->
        { verbose; user = rest; host = None }

let pp ppf t =
  if t.verbose then Format.pp_print_string ppf "/W ";
  if t.user <> "" then Format.pp_print_string ppf t.user;
  match t.host with
  | Some h -> Format.fprintf ppf "@%s" h
  | None -> ()
