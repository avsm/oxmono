(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* Unicode character data

   XXX. For now we kept that simple and use the Stdlib's Set and
   Maps. Bring in Uucp's tmapbool and tmap if that turns out to be too
   costly in space or time. *)

(* The four tables below are read on the parsing path and so must stay at
   module level, and a module-level value of a stdlib Set or Map cannot be
   read from a portable function: Set.S and Map.S declare no kind on t, so
   the value is seen as contended. An iarray of immutable data can be read.

   Each table is therefore still built through the stdlib container upstream
   used, then frozen into an iarray sorted by key that binary search reads.
   Building through the container is what keeps the duplicate handling
   upstream had, which is that a later entry of the source array wins, and it
   keeps the sort order out of this file's hands. The containers themselves
   are local to the initialisation, which runs once. Lookup stays
   logarithmic. *)

module Iarray = Basement.Stdlib_iarray_labels

module Uset = struct
  include Set.Make (Uchar)
  let of_array =
    let add acc u = add (Uchar.unsafe_of_int u) acc in
    Array.fold_left add empty
end

module Umap = struct
  include Map.Make (Uchar)
  let of_array =
    let add acc (u, f) = add (Uchar.unsafe_of_int u) f acc in
    Array.fold_left add empty
end

let uchars_of_uset s =
  Iarray.of_array
    (Array.of_list (List.map Uchar.to_int (Uset.elements s)))

let uchar_bindings_of_umap m =
  let binding (u, v) = Uchar.to_int u, v in
  Iarray.of_array (Array.of_list (List.map binding (Umap.bindings m)))

(* The three searches below take the last index rather than compute it, so
   that a table's length is read once at initialisation and not on every
   lookup, and they halve with [lsr] rather than [/]. Both matter. Written
   the obvious way these lookups run about a third slower than the Set and
   Map they replace, since [/] on a possibly negative operand compiles to
   sign correction. Written this way they run faster. [lsr] is right here
   because [mid] is only ever computed when [lo <= hi] and [lo >= 0], so the
   sum cannot be negative. *)

(* [mem_uchar t last u] is [true] iff [u] is in the sorted table [t] whose
   last index is [last]. *)
let mem_uchar (t : int iarray) last u =
  let u = Uchar.to_int u in
  let rec loop lo hi =
    if lo > hi then false else
    let mid = (lo + hi) lsr 1 in
    let v = Iarray.get t mid in
    if u < v then loop lo (mid - 1) else
    if u > v then loop (mid + 1) hi else true
  in
  loop 0 last

(* [find_uchar t last u] is the value bound to [u] in the sorted table [t]
   whose last index is [last]. *)
let find_uchar (t : (int * string) iarray) last u =
  let u = Uchar.to_int u in
  let rec loop lo hi =
    if lo > hi then None else
    let mid = (lo + hi) lsr 1 in
    let (k, v) = Iarray.get t mid in
    if u < k then loop lo (mid - 1) else
    if u > k then loop (mid + 1) hi else Some v
  in
  loop 0 last

let whitespace_uchars =
  uchars_of_uset (Uset.of_array Cmarkit_data_uchar.whitespace)

let whitespace_last = Iarray.length whitespace_uchars - 1

let punctuation_uchars =
  uchars_of_uset (Uset.of_array Cmarkit_data_uchar.punctuation)

let punctuation_last = Iarray.length punctuation_uchars - 1

let case_fold_uchars =
  uchar_bindings_of_umap (Umap.of_array Cmarkit_data_uchar.case_fold)

let case_fold_last = Iarray.length case_fold_uchars - 1

let unicode_version = Cmarkit_data_uchar.unicode_version
let is_unicode_whitespace u = mem_uchar whitespace_uchars whitespace_last u
let is_unicode_punctuation u = mem_uchar punctuation_uchars punctuation_last u
let unicode_case_fold u = find_uchar case_fold_uchars case_fold_last u

(* HTML entity data. *)

module String_map = Map.Make (String)

let html_entities =
  let add acc (entity, rep) = String_map.add entity rep acc in
  let m = Array.fold_left add String_map.empty Cmarkit_data_html.entities in
  Iarray.of_array (Array.of_list (String_map.bindings m))

let html_entity_last = Iarray.length html_entities - 1

let html_entity e =
  let t = html_entities in
  let rec loop lo hi =
    if lo > hi then None else
    let mid = (lo + hi) lsr 1 in
    let (k, v) = Iarray.get t mid in
    let c = String.compare e k in
    if c < 0 then loop lo (mid - 1) else
    if c > 0 then loop (mid + 1) hi else Some v
  in
  loop 0 html_entity_last

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
