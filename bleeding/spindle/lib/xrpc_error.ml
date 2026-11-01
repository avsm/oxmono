(* SPDX-License-Identifier: ISC *)
exception E of Httpz.Res.status * string * string

let fail status code message = raise (E (status, code, message))
