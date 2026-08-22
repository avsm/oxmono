(* Statuses are httpz's. This module exists only to give them the short names
   a site spells, and to keep [Status.code] where callers already look for it.
     *)
module St = Httpz.Res

type t = St.status

let code = St.status_code
let reason = St.status_reason
let of_code = St.status_of_int
