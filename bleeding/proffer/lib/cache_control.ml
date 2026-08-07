type span = [ `Secs of int | `Hours of int | `Days of int ]

(* The header value is built once, when the policy is described, and copied
   into every response that carries it. *)
type t = string

let seconds = function
  | `Secs n -> n
  | `Hours n -> n * 3600
  | `Days n -> n * 86400

let no_store = "no-store"

let private' ?max_age () =
  match max_age with
  | None -> "private"
  | Some s -> Printf.sprintf "private, max-age=%d" (seconds s)

let public ~max_age ?s_maxage ?stale_while_revalidate ?(must_revalidate = false)
    ?(immutable = false) () =
  let b = Buffer.create 64 in
  Buffer.add_string b (Printf.sprintf "public, max-age=%d" (seconds max_age));
  (match s_maxage with
  | None -> ()
  | Some n -> Buffer.add_string b (Printf.sprintf ", s-maxage=%d" n));
  (match stale_while_revalidate with
  | None -> ()
  | Some n ->
      Buffer.add_string b (Printf.sprintf ", stale-while-revalidate=%d" n));
  if must_revalidate then Buffer.add_string b ", must-revalidate";
  if immutable then Buffer.add_string b ", immutable";
  Buffer.contents b

let to_string t = t
