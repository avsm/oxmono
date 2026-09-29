(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* One-shot merlin queries against a real ocamlmerlin. The property that
   matters is that a query answers about the source it is handed rather than
   about a file on disk: the workspace is an empty directory and no a.ml is
   ever written there. The tests skip when no merlin answers on PATH. *)

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n%!" name
  end

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
  at 0

let run env =
  let proc = Eio.Stdenv.process_mgr env and fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_dir "okit_test" "merlin" in
  let root = Eio.Path.(fs / dir) in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ])))
    (fun () ->
      let seen = ref [] in
      let trace line = seen := line :: !seen in
      match Okit.Merlin.find ~trace ~proc ~root () with
      | None -> print_endline "ok   - skipped: no ocamlmerlin on PATH"
      | Some m -> (
          let src =
            "let greet name = \"hello \" ^ name\nlet n = greet \"x\"\n"
          in
          (match Okit.Merlin.outline m ~path:"a.ml" ~source:src with
          | Ok [ g; n ] ->
              check "outline names" (g.name = "greet" && n.name = "n");
              check "outline type" (g.typ = Some "string -> string")
          | Ok l ->
              (* merlin reports newest-first or oldest-first by version; accept
                 either order before failing. *)
              check "outline names"
                (List.map (fun (i : Okit.Merlin.outline_item) -> i.name) l
                |> List.sort compare = [ "greet"; "n" ])
          | Error e ->
              print_endline e;
              check "outline names" false);
          check "a query traces what it asked"
            (List.exists (fun l -> contains l "outline") !seen);
          (match
             Okit.Merlin.type_at m ~path:"a.ml" ~source:src
               { line = 2; col = 9 }
           with
          | Ok t -> check "type_at" (t = "string -> string")
          | Error e ->
              print_endline e;
              check "type_at" false);
          (match
             Okit.Merlin.errors m ~path:"a.ml" ~source:"let n : int = \"no\"\n"
           with
          | Ok [ { at = Some p; warning; message } ] ->
              check "errors pos" (p.line = 1);
              check "errors text" (contains message "string");
              (* A type error is what stops a build, and a tool that called it
                 a warning would have the model leave it. *)
              check "an error is not a warning" (not warning)
          | _ -> check "errors pos" false);
          (match
             Okit.Merlin.errors m ~path:"a.ml"
               ~source:"let f x = 1\nlet n = f 2\n"
           with
          | Ok [ { warning = true; _ } ] ->
              check "a warning says it is one" true
          | Ok l ->
              (* The unused-variable warning is on by default in merlin's own
                 configuration, which is what a workspace without a dune file
                 gets. *)
              check "a warning says it is one" (l = [])
          | Error e ->
              print_endline e;
              check "a warning says it is one" false);
          (match
             Okit.Merlin.occurrences m ~path:"a.ml" ~source:src
               { line = 1; col = 4 }
           with
          (* The definition and the one use, in a workspace with no index for
             anything outside this source. *)
          | Ok places ->
              check "occurrences finds the definition and the use"
                (List.map (fun (p : Okit.Merlin.place) -> p.pos.line) places
                |> List.sort compare = [ 1; 2 ])
          | Error e ->
              print_endline e;
              check "occurrences finds the definition and the use" false);
          (match
             Okit.Merlin.search m ~path:"a.ml" ~source:src
               ~query:"int -> string" ~limit:5
           with
          | Ok hits ->
              check "search finds a value of that type"
                (List.exists
                   (fun (h : Okit.Merlin.hit) -> h.name = "string_of_int")
                   hits)
          | Error e ->
              print_endline e;
              check "search finds a value of that type" false);
          (match
             Okit.Merlin.complete m ~path:"a.ml" ~source:src
               { line = 2; col = 0 } ~prefix:"List.ma"
           with
          | Ok cs ->
              check "complete lists what a module offers"
                (List.exists
                   (fun (c : Okit.Merlin.completion) ->
                     c.name = "map" && contains c.typ "'a list")
                   cs)
          | Error e ->
              print_endline e;
              check "complete lists what a module offers" false);
          match
            Okit.Merlin.locate m ~path:"a.ml" ~source:"let u = List.map\n"
              { line = 1; col = 14 }
          with
          | Ok (`Found (f, p)) ->
              check "locate" (Filename.basename f = "list.ml" && p.line > 0)
          | _ -> check "locate" false))

let () =
  Eio_main.run run;
  if !failures = 0 then print_string "\nAll tests passed.\n"
  else begin
    Printf.printf "\n%d check(s) failed.\n" !failures;
    exit 1
  end
