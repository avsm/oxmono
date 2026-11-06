(* Assertion and substring helpers shared by the Arod tests. *)

(* [checks] counts the calls to {!check}, for the summary each test prints. *)
let checks = ref 0

(* [check name cond] counts a check and exits with status 1, naming it on
   stderr, when [cond] is false. *)
let check name cond =
  incr checks;
  if not cond then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* [find hay needle] is the offset of the first [needle] in [hay]. *)
let find hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i =
    if i + n > h then None
    else if String.sub hay i n = needle then Some i
    else go (i + 1)
  in
  go 0

let contains hay needle = find hay needle <> None

(* [before hay a b] holds when [a] and [b] both occur in [hay] and the first
   [a] precedes the first [b]. *)
let before hay a b =
  match (find hay a, find hay b) with Some i, Some j -> i < j | _ -> false
