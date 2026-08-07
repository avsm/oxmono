type t =
  [ `GET
  | `HEAD
  | `POST
  | `PUT
  | `DELETE
  | `PATCH
  | `OPTIONS
  | `Other of string ]

let to_string = function
  | `GET -> "GET"
  | `HEAD -> "HEAD"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `DELETE -> "DELETE"
  | `PATCH -> "PATCH"
  | `OPTIONS -> "OPTIONS"
  | `Other s -> s

let of_string s =
  match s with
  | "GET" -> `GET
  | "HEAD" -> `HEAD
  | "POST" -> `POST
  | "PUT" -> `PUT
  | "DELETE" -> `DELETE
  | "PATCH" -> `PATCH
  | "OPTIONS" -> `OPTIONS
  | s -> `Other s

(* Compared through the wire spelling, so [`Other "GET"] and [`GET] are the
   same method. [of_string] never builds that pair, but a backend mapping its
   own method type might. *)
let equal (a : t) (b : t) = String.equal (to_string a) (to_string b)
