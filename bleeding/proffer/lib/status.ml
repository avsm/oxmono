module St = Httpz.Res

type t = St.status

let code = St.status_code
let reason = St.status_reason
let of_code = St.status_of_int
