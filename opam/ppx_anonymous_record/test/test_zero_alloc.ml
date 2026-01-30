open! Core

let%test_unit ("[%anon.local] does not allocate" [@tags "fast-flambda"]) =
  (* Allocated values *)
  let x = Some () in
  let y = Some () in
  Expect_test_helpers_core.require_no_allocation (fun () ->
    let anonymous_record =
      (* [%anon.local] takes a (possibly) local tuple, and returns a local
         [Anonymous_record.t].

         This expect test compiling shows that [anonymous_record] is not allocated on the
         heap. *)
      [%anon.local { x; y }]
    in
    ignore
      (anonymous_record
       : ( [ `x ] * [ `y ]
           , unit option * unit option )
           Ppx_anonymous_record_runtime.Anonymous_record.t))
;;
