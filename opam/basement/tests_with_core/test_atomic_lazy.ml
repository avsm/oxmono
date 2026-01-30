open Basement
open Expect_test_helpers_core

let%expect_test "allocation" =
  let thunk () = 1234 in
  let t =
    require_allocation_does_not_exceed (Minor_words 8) (fun () ->
      Atomic_lazy.from_fun thunk)
  in
  let result =
    require_allocation_does_not_exceed (Minor_words 2) (fun () -> Atomic_lazy.force t)
  in
  print_int result;
  [%expect {| 1234 |}];
  let result = require_no_allocation (fun () -> Atomic_lazy.force t) in
  print_int result;
  [%expect {| 1234 |}]
;;
