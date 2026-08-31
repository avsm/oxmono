(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The constraints the specification puts on a node name, in the order
   it lists them. A name composed only of periods is rejected whatever
   its length, so "..." goes with "." and "..". "zarr.json" is refused
   because a node of that name would own the metadata key of its
   parent. *)

let periods_only n =
  let ok = ref true in
  String.iter (fun c -> if c <> '.' then ok := false) n;
  !ok

let name_error n =
  if String.equal n "" then Some "is empty"
  else if String.contains n '/' then Some "holds a '/'"
  else if periods_only n then Some "is periods alone"
  else if String.starts_with ~prefix:"__" n then
    Some "starts with the reserved prefix \"__\""
  else if String.equal n "zarr.json" then Some "is \"zarr.json\""
  else None

let is_valid_name n = name_error n = None

(* A leading '/' is the spelling the specification uses and is dropped
   before splitting. A second one is not: it leaves an empty first
   name, which is what makes "//a" a failure rather than "/a". *)
let check path =
  let body =
    if String.length path > 0 && path.[0] = '/' then
      String.sub path 1 (String.length path - 1)
    else path
  in
  if not (String.equal body "") then
    List.iter
      (fun n ->
        match name_error n with
        | None -> ()
        | Some m ->
            Error.raise_
              (Error.Metadata
                 (Printf.sprintf "node path %S: the name %S %s" path n m)))
      (String.split_on_char '/' body)
