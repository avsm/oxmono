(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A calendar mirrored into a directory of iCalendar files, run against the
   oracle, with the actions it logs. *)

open Caldav_oracle
module Client = Caldav_eio.Client
module M = Fetch_dav.Mirror

let temp_dir () =
  Filename.concat (Filename.get_temp_dir_name ()) (unique "ics-mirror")

let files dir =
  List.sort compare
    (List.filter
       (fun f -> Filename.check_suffix f ".ics")
       (Array.to_list (Sys.readdir dir)))

let run t dir url =
  let actions = ref [] in
  let log a =
    actions := a :: !actions;
    Format.printf "  %a@." M.pp_action a
  in
  let s =
    M.run ~log (Client.dav t.client) ~collection:url
      ~dir:Eio.Path.(Eio.Stdenv.fs t.env / dir)
  in
  (s, List.rev !actions)

let has p actions = List.exists p actions

let read dir f =
  In_channel.with_open_bin (Filename.concat dir f) In_channel.input_all

let test_mirror t =
  let url = fresh_calendar t in
  let dir = temp_dir () in
  let { Client.href = a; _ } =
    ok "add"
      (Client.add Caldav.Data.ical t.client url
         (event ~summary:"Ann's review"
            (utc ~year:2026 ~month:9 ~day:1 ~hour:9 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:1 ~hour:10 ~minute:0)))
  in
  let { Client.href = b; _ } =
    ok "add"
      (Client.add Caldav.Data.ical t.client url
         (event ~summary:"Lunch"
            (utc ~year:2026 ~month:9 ~day:2 ~hour:12 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:2 ~hour:13 ~minute:0)))
  in
  let s, actions = run t dir url in
  Alcotest.(check int) "two fetched" 2 s.fetched;
  Alcotest.(check bool)
    "initial logged" true
    (has (fun a -> a = M.Initial) actions);
  Alcotest.(check (list string))
    "files"
    (List.sort compare [ M.file_of_href a; M.file_of_href b ])
    (files dir);
  (match Ical.one_of_string (read dir (M.file_of_href a)) with
  | Ok cal ->
      Alcotest.(check (option string))
        "summary" (Some "Ann's review") (summary cal)
  | Error e -> Alcotest.fail e);
  let s, _ = run t dir url in
  Alcotest.(check int) "nothing fetched" 0 s.fetched;
  ok "delete" (Client.delete t.client a);
  let uid_b = Filename.chop_suffix (Httpz_dav.basename b) ".ics" in
  let _ =
    ok "update"
      (Client.put Caldav.Data.ical t.client b
         (event ~uid:uid_b ~summary:"Long lunch"
            (utc ~year:2026 ~month:9 ~day:2 ~hour:12 ~minute:0)
            (utc ~year:2026 ~month:9 ~day:2 ~hour:14 ~minute:0)))
  in
  let s, actions = run t dir url in
  Alcotest.(check int) "fetched" 1 s.fetched;
  Alcotest.(check int) "removed" 1 s.removed;
  Alcotest.(check bool)
    "removal logged" true
    (has
       (function M.Removed (h, _) -> Httpz_dav.same_href h a | _ -> false)
       actions);
  Alcotest.(check (list string)) "files after" [ M.file_of_href b ] (files dir);
  match Ical.one_of_string (read dir (M.file_of_href b)) with
  | Ok cal ->
      Alcotest.(check (option string))
        "updated" (Some "Long lunch") (summary cal)
  | Error e -> Alcotest.fail e

let () =
  Alcotest.run "caldav-mirror"
    [ ("radicale", [ test_case "mirror a calendar" test_mirror ]) ]
