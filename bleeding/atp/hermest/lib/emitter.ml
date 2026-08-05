(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

type t = {
  mutable imports : string list;
  mutable generated_unions : string list;
  mutable union_names : (string list * string) list (* refs -> context name *);
  cyclic_nsids :
    string list (* nsids that form cycles - refs to these use Jsont.json *);
  buf : Buffer.t;
}

let make ?(cyclic_nsids = []) () =
  {
    imports = [];
    generated_unions = [];
    union_names = [];
    cyclic_nsids;
    buf = Buffer.create 4096;
  }

(** check if an nsid is part of a cycle *)
let is_cyclic_nsid t nsid = List.mem nsid t.cyclic_nsids

(** add an import if not already present *)
let add_import t module_name =
  if not (List.mem module_name t.imports) then
    t.imports <- module_name :: t.imports

let get_imports t = t.imports

(** mark a union type as generated to avoid duplicates *)
let mark_union_generated t union_name =
  if not (List.mem union_name t.generated_unions) then
    t.generated_unions <- union_name :: t.generated_unions

let is_union_generated t union_name = List.mem union_name t.generated_unions

(** register a context-based name for a union based on its refs, allowing inline
    unions to be reused when the same refs appear elsewhere *)
let register_union_name t refs context_name =
  let sorted_refs = List.sort String.compare refs in
  if not (List.exists (fun (r, _) -> r = sorted_refs) t.union_names) then
    t.union_names <- (sorted_refs, context_name) :: t.union_names

(** look up a union's registered context-based name *)
let lookup_union_name t refs =
  let sorted_refs = List.sort String.compare refs in
  List.assoc_opt sorted_refs t.union_names

let emit t s = Buffer.add_string t.buf s

let emitln t s =
  Buffer.add_string t.buf s;
  Buffer.add_char t.buf '\n'

let emit_newline t = Buffer.add_char t.buf '\n'
let contents t = Buffer.contents t.buf
