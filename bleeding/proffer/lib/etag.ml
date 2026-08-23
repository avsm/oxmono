(* An entity-tag holds both the opaque value a conditional request compares
   and the quoted form that goes on the wire, rendered once when the tag is
   built. The opaque value must not contain a double quote.

   Rendering on every use instead, which is what [to_string] over a variant
   did, costs a string per response. A site that reuses a tag, which is every
   memoised page, then pays for the same three bytes of quoting on every
   request that hits the cache. *)

type t = { opaque : string; rendered : string; weak : bool }

let strong s = { opaque = s; rendered = "\"" ^ s ^ "\""; weak = false }
let weak s = { opaque = s; rendered = "W/\"" ^ s ^ "\""; weak = true }
let is_weak t = t.weak
let opaque t = t.opaque
let to_string t = t.rendered

(* Weak comparison per RFC 9110 section 8.8.3.2: the opaque values match and
   the strength is ignored. It is the only comparison a conditional GET needs.
   Strong comparison exists for Range requests, which are out of scope. *)
let weak_equal a b = String.equal a.opaque b.opaque

let trim s =
  let n = String.length s in
  let i = ref 0 and j = ref n in
  while !i < !j && (s.[!i] = ' ' || s.[!i] = '\t') do
    incr i
  done;
  while !j > !i && (s.[!j - 1] = ' ' || s.[!j - 1] = '\t') do
    decr j
  done;
  String.sub s !i (!j - !i)

(* [of_field_value s] is one entity-tag as it appears in an If-None-Match list.
   A tag that is not correctly quoted is rejected, which makes the whole
   condition fail to match and so sends the full response. *)
let of_field_value s =
  let s = trim s in
  let is_strong, body =
    if String.length s >= 2 && String.sub s 0 2 = "W/" then
      (false, String.sub s 2 (String.length s - 2))
    else (true, s)
  in
  let n = String.length body in
  if n >= 2 && body.[0] = '"' && body.[n - 1] = '"' then
    let v = String.sub body 1 (n - 2) in
    Some (if is_strong then strong v else weak v)
  else None
