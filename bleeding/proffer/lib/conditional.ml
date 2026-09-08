(* Request-precondition policy. Date/ETag parsing uses local field values and
   unboxed time; no intermediate parsed-condition objects reach the heap. *)
module M = Httpz.Method
module H = Httpz.Header_name
module F64 = Stdlib_upstream_compatible.Float_u

(* Repeated field lines combine into one comma-joined value (RFC 9110 section
   5.3), which is not a valid date, so a repeated date field is [Null]. *)
let[@zero_alloc] rec only_field (t : Headers.t @ local) name
    (found : string or_null @ local) = exclave_
  match t with
  | [] -> found
  | f :: tl ->
      if Headers.same_name f.Headers.name name then
        match found with
        | This _ -> Null
        | Null -> only_field tl name (This f.Headers.value)
      else only_field tl name found

let[@zero_alloc] imf_date ~has_now (now : float#) (t : Headers.t @ local) name
    : #(bool * float#) =
  match only_field t name Null with
  | Null -> #(false, #0.)
  | This v ->
      let #(valid, parsed) = Date.parse_imf ~has_now now v in
      #(valid, parsed)

(* An entity-tag condition is a list, so repeated fields combine. *)
let[@zero_alloc] rec any_field (t : Headers.t @ local) name ~strong
    (etag : Etag.t option @ local) =
  match t with
  | [] -> false
  | f :: tl ->
      Headers.same_name f.Headers.name name
      && Etag.matches_field f.Headers.value ~strong etag
      || any_field tl name ~strong etag

let[@zero_alloc] condition_matches (headers : Headers.t @ local) name ~strong
    (etag : Etag.t option @ local) =
  any_field headers name ~strong etag

(* IMF-fixdate has whole-second resolution. *)
let[@zero_alloc] not_after (a : float @ local) (b : float#) =
  F64.compare (F64.floor (F64.of_float a)) (F64.floor b) <= 0

type precondition = Proceed | Revalidated | Failed

let[@zero_alloc] is_conditional_read (req : Req.t @ local) =
  let meth = Req.meth req in
  Method.equal meth M.Get || Method.equal meth M.Head

(* A generic handler exposes validators only after it has run, which is too
   late to protect a mutation. Refuse conditional writes before dispatch rather
   than claim a post-state comparison prevented an effect. Invalid or repeated
   date fields are ignored, as RFC 9110 requires. *)
let[@zero_alloc] reject_conditional_write ~has_now (now : float#)
    (req : Req.t @ local) =
  (not (Req.preconditions_handled req))
  && (not (is_conditional_read req))
  &&
  let headers = Req.headers req in
  Headers.mem headers H.If_match
  || Headers.mem headers H.If_none_match
  ||
  let #(valid, _) = imf_date ~has_now now headers H.If_unmodified_since in
  valid

(* RFC 9110 section 13.2.2 fixes this order. Unsafe conditional requests have
   already been refused above, before dispatch; this evaluates GET and HEAD.
   If-Range is not evaluated because this library does not serve ranges. *)
let[@inline always][@zero_alloc] evaluate ~has_now (now : float#) (req : Req.t @ local)
    (d : Resp.description @ local) =
  let code = Status.code d.Resp.status in
  if Req.preconditions_handled req || code < 200 || code >= 300 then Proceed
  else
    let headers = Req.headers req in
    let safe = is_conditional_read req in
    let has_if_match = Headers.mem headers H.If_match in
    if
      has_if_match
      && not (condition_matches headers H.If_match ~strong:true d.Resp.etag)
    then Failed
    else
      let unmodified_since_failed =
        (not has_if_match)
        &&
        match d.Resp.last_modified with
        | None -> false
        | Some lm ->
            let #(valid, until) =
              imf_date ~has_now now headers H.If_unmodified_since
            in
            valid && not (not_after lm until)
      in
      if unmodified_since_failed then Failed
      else if Headers.mem headers H.If_none_match then
        if condition_matches headers H.If_none_match ~strong:false d.Resp.etag
        then Revalidated
        else Proceed
      else
        match d.Resp.last_modified with
        | Some lm when safe ->
            let #(valid, since) =
              imf_date ~has_now now headers H.If_modified_since
            in
            if not valid then Proceed
            (* A date the server has not reached yet is meaningless and would
               let a client pin 304s, so it is ignored (section 13.1.3). *)
            else if has_now && F64.compare (F64.floor since) (F64.floor now) > 0
            then Proceed
            else if not_after lm since then Revalidated
            else Proceed
        | _ -> Proceed
