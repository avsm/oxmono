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
