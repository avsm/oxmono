type span =
  [ `Secs of int
  | `Hours of int
  | `Days of int
  ]

type t = string

let checked_seconds factor n =
  if n < 0 || n > max_int / factor
  then invalid_arg "Proffer.Cache_control: duration is negative or too large";
  n * factor
;;

let seconds = function
  | `Secs n -> checked_seconds 1 n
  | `Hours n -> checked_seconds 3600 n
  | `Days n -> checked_seconds 86400 n
;;

let nonnegative name = function
  | Some n when n < 0 ->
    invalid_arg ("Proffer.Cache_control.public: " ^ name ^ " is negative")
  | n -> n
;;

let no_store = "no-store"

let private' ?max_age () =
  match max_age with
  | None -> "private"
  | Some s -> Printf.sprintf "private, max-age=%d" (seconds s)
;;

let public
  ~max_age
  ?s_maxage
  ?stale_while_revalidate
  ?(must_revalidate = false)
  ?(immutable = false)
  ()
  =
  let max_age = seconds max_age in
  let s_maxage = nonnegative "s_maxage" s_maxage in
  let stale_while_revalidate =
    nonnegative "stale_while_revalidate" stale_while_revalidate
  in
  let b = Buffer.create 64 in
  Buffer.add_string b (Printf.sprintf "public, max-age=%d" max_age);
  (match s_maxage with
   | None -> ()
   | Some n -> Buffer.add_string b (Printf.sprintf ", s-maxage=%d" n));
  (match stale_while_revalidate with
   | None -> ()
   | Some n -> Buffer.add_string b (Printf.sprintf ", stale-while-revalidate=%d" n));
  if must_revalidate then Buffer.add_string b ", must-revalidate";
  if immutable then Buffer.add_string b ", immutable";
  Buffer.contents b
;;

let to_string t = t
