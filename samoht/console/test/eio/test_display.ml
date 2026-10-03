(* Console_eio driver. Everything runs in [`History] mode onto a buffer, where
   each task line, log line and the result banner is emitted as plain text, so
   the output is deterministic and the escape-code redraw path stays out. *)

module D = Console.Display

(* Run [f] under a fresh display captured into a string, then return the
   text. *)
let capture ?header f =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Console_eio.run ~clock ~ppf ~mode:`History ?header f;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let is_infix ~affix s =
  let n = String.length affix and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = affix || at (i + 1)) in
  n = 0 || at 0

let mem ~msg needle haystack =
  Alcotest.(check bool) msg true (is_infix ~affix:needle haystack)

(* A finished task commits its label; set_result `Ok prints the verdict. *)
let test_run_banner () =
  let out =
    capture ~header:"Build" (fun t ->
        let step = D.task t "compile foo.ml" in
        D.succeed step;
        D.set_result t ~detail:"3 files, 0 errors" (`Ok "build succeeded"))
  in
  mem ~msg:"task label" "compile foo.ml" out;
  mem ~msg:"detail" "3 files, 0 errors" out;
  mem ~msg:"verdict marker" "✓" out;
  mem ~msg:"verdict message" "build succeeded" out

(* set_result `Fail prints the failure verdict. *)
let test_run_fail () =
  let out =
    capture (fun t ->
        let step = D.task t "link" in
        D.fail step;
        D.set_result t (`Fail "link failed"))
  in
  mem ~msg:"fail marker" "✗" out;
  mem ~msg:"fail message" "link failed" out

(* An unhandled exception clears the region and propagates. *)
let test_run_raises () =
  Alcotest.check_raises "propagates" (Failure "boom") (fun () ->
      ignore
        (capture (fun t ->
             let _ = D.task t "doomed" in
             failwith "boom")))

(* Logs messages are routed into the same permanent semantic history while the
   driver owns the reporter. *)
let test_reporter () =
  let out =
    capture (fun t ->
        Logs.set_level (Some Logs.Debug);
        let step = D.task t "step" in
        Logs.app (fun m -> m "application-line");
        Logs.app (fun m -> m "╭───╮\n│ x │\n╰───╯");
        Logs.warn (fun m -> m "warning-line");
        D.succeed step)
  in
  mem ~msg:"application log" "application-line" out;
  mem ~msg:"multiline log keeps physical rows" "╭───╮\n│ x │\n╰───╯\n" out;
  mem ~msg:"warning log" "warning-line" out

let suite =
  ( "display",
    [
      ("run banner", `Quick, test_run_banner);
      ("run fail", `Quick, test_run_fail);
      ("run raises", `Quick, test_run_raises);
      ("reporter", `Quick, test_reporter);
    ] )
