(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A step that counts its files, with a layer under it that counts bytes.
   [main.exe 2>&1 | cat] shows the plain lines a pipe gets,
   [main.exe --progress=plain] prints them on a terminal too, and
   [--progress=tty], [quiet] and [json] show the other modes. With [fail],
   the layer fails, so do the step and the work, which a task under them
   does not fail on its own, and the program exits with 1. *)

let build ~fail clock root =
  let step = Console.Task.start ~log_lines:4 root "RUN make" in
  let layer = Console.Task.start step "sha256:abcd..." in
  for i = 0 to 5 do
    Console.Task.set_items step ~count:i ~total:5;
    Fmt.kstr (Console.Task.log step) "  CC src/file%d.c" i;
    Eio.Time.Mono.sleep clock 0.2
  done;
  let total = 2147483648L in
  for i = 0 to 8 do
    Console.Task.set_bytes layer
      ~count:(Int64.mul (Int64.of_int i) 268435456L)
      ~total;
    Eio.Time.Mono.sleep clock 0.2
  done;
  if fail then begin
    Console.Task.log layer "checksum mismatch";
    Console.Task.fail layer;
    Console.Task.fail step;
    Console.Task.fail root
  end
  else begin
    Console.Task.succeed layer;
    Console.Task.succeed step
  end;
  not fail

let main progress fail =
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  if Console_eio.run ~clock ~progress ~label:"Building" (build ~fail clock) then
    0
  else 1

let progress =
  Cmdliner.Arg.(
    value
    & opt (enum Console.Progress.all) `Auto
    & info [ "progress" ]
        ~doc:
          "How progress shows: $(b,auto), $(b,tty), $(b,plain), $(b,quiet) or \
           $(b,json).")

let fail = Cmdliner.Arg.(value & flag & info [ "fail" ] ~doc:"Fail the layer.")

let cmd =
  Cmdliner.Cmd.(v (info "build") Cmdliner.Term.(const main $ progress $ fail))

let () = exit (Cmdliner.Cmd.eval' cmd)
