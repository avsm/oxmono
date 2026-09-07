(* Cache the rendered field value because entity-tags are commonly reused. The
   strings are global so a tag read out of a local description yields them. *)

type t =
  { global_ opaque : string
  ; global_ rendered : string
  ; weak : bool
  }

let strong s = { opaque = s; rendered = "\"" ^ s ^ "\""; weak = false }
let weak s = { opaque = s; rendered = "W/\"" ^ s ^ "\""; weak = true }
let is_weak (t : t @ local) = t.weak
let opaque (t : t @ local) = t.opaque
let to_string (t : t @ local) = t.rendered

(* Weak comparison ignores tag strength, as specified by RFC 9110 section 8.8.3.2. *)
let weak_equal (a : t @ local) (b : t @ local) = String.equal a.opaque b.opaque

(* Owned-string fields retain unbounded lists and Proffer's mixed-wildcard
   policy, independently of Httpz.Etag's bounded transport parser. *)
let[@zero_alloc] skip_ows (v : string @ local) i j = Headers.skip_ows v i j
let[@zero_alloc] trim_ows (v : string @ local) i j = Headers.trim_ows v i j

let[@zero_alloc] rec same_bytes (a : string @ local) i (b : string @ local) j n =
  n = 0
  || Char.equal (String.unsafe_get a i) (String.unsafe_get b j)
     && same_bytes a (i + 1) b (j + 1) (n - 1)

(* Strong comparison, as RFC 9110 section 8.8.3.2 defines it for If-Match. A
   weak tag on either side never matches. Weak comparison ignores strength. *)
let[@zero_alloc] item_matches (v : string @ local) i j ~strong
    (etag : t @ local) =
  let weak_item =
    j - i >= 2
    && Char.equal (String.unsafe_get v i) 'W'
    && Char.equal (String.unsafe_get v (i + 1)) '/'
  in
  let i = if weak_item then i + 2 else i in
  let n = j - i in
  n >= 2
  && Char.equal (String.unsafe_get v i) '"'
  && Char.equal (String.unsafe_get v (j - 1)) '"'
  && ((not strong) || not (weak_item || is_weak etag))
  &&
  let opaque = opaque etag in
  String.length opaque = n - 2 && same_bytes opaque 0 v (i + 1) (n - 2)

(* Wildcards and entity-tags share the quote-aware field scan. *)
let[@zero_alloc] condition_item (v : string @ local) first limit ~strong
    (etag : t option @ local) =
  (limit - first = 1 && String.unsafe_get v first = '*')
  || match etag with
     | Some etag -> item_matches v first limit ~strong etag
     | None -> false

let[@zero_alloc] rec any_item (v : string @ local) ~strong (etag : t option @ local)
    ~start ~i ~quoted =
  let n = String.length v in
  if i = n then
    let a = skip_ows v start n in
    condition_item v a (trim_ows v a n) ~strong etag
  else
    let c = String.unsafe_get v i in
    if Char.equal c '"' then
      any_item v ~strong etag ~start ~i:(i + 1) ~quoted:(not quoted)
    else if Char.equal c ',' && not quoted then
      (let a = skip_ows v start i in
       condition_item v a (trim_ows v a i) ~strong etag)
      || any_item v ~strong etag ~start:(i + 1) ~i:(i + 1) ~quoted:false
    else any_item v ~strong etag ~start ~i:(i + 1) ~quoted

let[@inline always][@zero_alloc] matches_field (value : string @ local) ~strong
    (etag : t option @ local) =
  any_item value ~strong etag ~start:0 ~i:0 ~quoted:false
