(** The integration suite: every check in it makes a real request.

    [MATRIX_TEST_HOMESERVER] names the server. Without it this prints a SKIP
    line and exits 0, so [dune runtest] stays hermetic and CI without a
    homeserver stays green; [test/integration/synapse.sh up] starts one and
    prints the export line to set.

    This module is only the runner. The checks live in the [scenario_*] modules
    and share {!Harness}, which registers users, starts sync loops and waits for
    the server to catch up. *)

let suites =
  match Sys.getenv_opt "MATRIX_TEST_PROFILE" with
  | Some "dendrite-core" -> [ ("rooms", Scenario_rooms.dendrite_core_tests) ]
  | Some "peeking" -> [ ("rooms", Scenario_rooms.peeking_tests) ]
  | None | Some "" ->
      [
        ("rooms", Scenario_rooms.tests);
        ("e2ee", Scenario_e2ee.tests);
        ("room_list", Scenario_room_list.tests);
        ("ui", Scenario_ui.tests);
        ("bot_lib", Scenario_bot_lib.tests);
        ("bots", Scenario_bots.tests);
      ]
  | Some profile -> failwith ("unknown MATRIX_TEST_PROFILE: " ^ profile)

let () =
  match Harness.homeserver_env () with
  | None when Sys.getenv_opt "MATRIX_REQUIRE_HOMESERVER" = Some "1" ->
      prerr_endline "required MATRIX_TEST_HOMESERVER is not set";
      exit 1
  | None ->
      print_endline "SKIP: MATRIX_TEST_HOMESERVER is not set";
      exit 0
  | Some url ->
      Printf.printf "running against %s\n%!" url;
      Alcotest.run "matrix integration" suites
