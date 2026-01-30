open! Core
open Filesystem_types
open Expect_test_helpers_core

include (
  On_cleanup_error :
  sig
    type 'return t = 'return On_cleanup_error.t =
      | Call of (exn -> 'return)
      | Ignore
      | Log
      | Raise
    [@@deriving sexp_of, typed_variants]
  end)

open struct
  let examples =
    List.map Typed_variant.Packed.all ~f:(function
      | { f = T Call } -> Call (fun exn -> raise_s [%message "callback" ~_:(exn : exn)])
      | { f = T Ignore } -> Ignore
      | { f = T Log } -> Log
      | { f = T Raise } -> Raise)
  ;;
end

let%expect_test "[sexp_of_t]" =
  List.iter examples ~f:(fun t -> print_s [%sexp (t : _ t)]);
  [%expect
    {|
    (Call <fun>)
    Ignore
    Log
    Raise
    |}]
;;

let on_cleanup_error = On_cleanup_error.on_cleanup_error

let%expect_test "[on_cleanup_error]" =
  let test t ~use_backtrace =
    Dynamic.with_temporarily Backtrace.elide true ~f:(fun () ->
      print_endline "";
      print_s [%sexp (t : _ t)];
      let sexp =
        match
          try raise_s [%message "Oops!"] with
          | exn ->
            let backtrace =
              if use_backtrace then Backtrace.Exn.most_recent_for_exn exn else None
            in
            on_cleanup_error
              t
              ~backtrace
              ~exn
              ~log_s:(fun sexp -> print_s [%message "logging" ~_:(sexp : Sexp.t)])
              ~path:(File_path.Absolute.of_string "/sample/file")
              ~return:Fn.id
        with
        | () -> [%sexp ()]
        | exception exn -> [%message "raise" ~_:(exn : exn)]
      in
      print_s sexp)
  in
  List.iter examples ~f:(test ~use_backtrace:true);
  [%expect
    {|
    (Call <fun>)
    (raise (callback Oops!))

    Ignore
    ()

    Log
    (logging (
      "Error while cleaning up temporary file or directory"
      (path /sample/file)
      (exn  Oops!)
      (backtrace ("<backtrace elided in test>"))))
    ()

    Raise
    (raise Oops!)
    |}];
  List.iter examples ~f:(test ~use_backtrace:false);
  [%expect
    {|
    (Call <fun>)
    (raise (callback Oops!))

    Ignore
    ()

    Log
    (logging (
      "Error while cleaning up temporary file or directory"
      (path /sample/file)
      (exn  Oops!)))
    ()

    Raise
    (raise (
      exn.ml.Reraised "Error while cleaning up temporary file or directory" Oops!))
    |}]
;;
