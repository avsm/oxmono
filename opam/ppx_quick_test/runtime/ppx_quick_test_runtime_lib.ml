open! Core
open Expect_test_helpers_base
include Types_intf
module File_corrections = File_corrections
module Location = Location
module Params = Params
module Sexp_examples = Sexp_examples

module Make (Arg : Types_intf.Arg) = struct
  module IO = Arg.IO

  let run_quick_test
    (type tup fn)
    ~here_pos
    ~config
    ~cr
    ~hide_positions
    ~(examples : tup list)
    ~trials
    ~(sexp_examples : Sexp_examples.t)
    ~(params : (tup, fn, unit IO.t) Params.t)
    ~filename_rel_to_project_root
    (f : fn)
    : unit IO.t
    =
    Printexc.record_backtrace true;
    let sexp_of_tup = unstage (Params.sexp_of_tup params) in
    match Sexp_examples.get_parsed_examples sexp_examples params with
    | Error parse_error ->
      print_cr
        ?cr
        ~here:here_pos
        [%message
          {|[ppx_quick_test] was unable to convert a sexp example string into a sexp: |}
            ~_:(parse_error : Error.t)];
      IO.return ()
    | Ok parsed_sexp_examples ->
      let all_examples = examples @ parsed_sexp_examples in
      let all_examples_sexp_set =
        List.map all_examples ~f:sexp_of_tup |> Sexp.Set.of_list
      in
      IO.map
        (Arg.run
           ~here_pos
           ~config
           ~trials
           ~examples:all_examples
           (module struct
             type t = tup

             let sexp_of_t = sexp_of_tup
             let quickcheck_generator = Params.tup_generator params
             let quickcheck_shrinker = Params.tup_shrinker params
           end)
           ~f:(fun input ->
             match
               Or_error.try_with ~backtrace:true (fun () -> Params.call params input f)
             with
             | Ok x ->
               (* NOTE: This [map] is important in the [Deferred] case as it waits for the
                  Deferred effect to finish. Otherwise there is a leak/explosion of
                  pending jobs that result in really weird test behaviors. *)
               IO.map x ~f:(fun () ->
                 match Expect_test_helpers_base.expect_test_output ~here:here_pos () with
                 | "" -> Ok ()
                 | trailing_output ->
                   Or_error.error_s
                     [%message
                       "Trailing output at end of [%quick_test] block"
                         (trailing_output : string)])
             | Error error -> IO.return (Error error)))
        ~f:(fun quickcheck_results ->
          Result.iter_error quickcheck_results ~f:(fun (input, output) ->
            let () =
              match sexp_of_tup input with
              | input_sexp ->
                let () =
                  match Set.mem all_examples_sexp_set input_sexp with
                  | true -> ()
                  | false ->
                    Sexp_examples.insertion_for_new_example sexp_examples input_sexp
                    |> Option.iter
                         ~f:(File_corrections.add_insertion ~filename_rel_to_project_root)
                in
                print_s
                  [%message
                    Ppx_quick_test_common.test_failed_message ~input:(input_sexp : Sexp.t)]
              | exception exn ->
                print_s
                  [%message
                    Ppx_quick_test_common.test_failed_message
                      ~exception_encountered_when_converting_input_to_sexp:(exn : exn)]
            in
            print_cr ?cr ?hide_positions ~here:here_pos [%sexp (output : Error.t)]))
  ;;
end
