let count = ref 0

let check name b =
  incr count;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let report name = Printf.printf "%s: %d checks ok\n" name !count
