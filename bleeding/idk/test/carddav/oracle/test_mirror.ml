(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* An address book mirrored into a directory of vCard files, run against the
   oracle, with the actions it logs. *)

open Oracle_harness
module Client = Carddav_eio.Client
module M = Fetch_dav.Mirror

let temp_dir () =
  Filename.concat (Filename.get_temp_dir_name ()) (unique "vcard-mirror")

let files dir =
  List.sort compare
    (List.filter
       (fun f -> Filename.check_suffix f ".vcf")
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

let contains s sub =
  try
    ignore (Str.search_forward (Str.regexp_string sub) s 0);
    true
  with Not_found -> false

let test_mirror t =
  let url = fresh_addressbook t in
  let dir = temp_dir () in
  let { Client.href = a; _ } =
    ok "add"
      (Client.add Carddav.Data.vcard t.client url
         (vcard ~email:"a@example.com" "Ann"))
  in
  let { Client.href = b; _ } =
    ok "add" (Client.add Carddav.Data.vcard t.client url (vcard "Bob"))
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
  Alcotest.(check bool)
    "a vCard" true
    (String.starts_with ~prefix:"BEGIN:VCARD" (read dir (M.file_of_href a)));
  Alcotest.(check bool)
    "token stored" true
    (s.token <> None && Sys.file_exists (Filename.concat dir M.index_file));
  let s, actions = run t dir url in
  Alcotest.(check int) "nothing fetched" 0 s.fetched;
  Alcotest.(check bool)
    "unchanged logged" true
    (has (fun a -> a = M.Unchanged) actions);
  ok "delete" (Client.delete t.client a);
  let { Client.href = c; _ } =
    ok "add" (Client.add Carddav.Data.vcard t.client url (vcard "Cid"))
  in
  let uid_b = Filename.chop_suffix (Httpz_dav.basename b) ".vcf" in
  let _ =
    ok "update"
      (Client.put Carddav.Data.vcard t.client b (vcard ~uid:uid_b "Bob Two"))
  in
  let s, actions = run t dir url in
  Alcotest.(check int) "fetched" 2 s.fetched;
  Alcotest.(check int) "removed" 1 s.removed;
  Alcotest.(check bool)
    "removal logged" true
    (has
       (function M.Removed (h, _) -> Httpz_dav.same_href h a | _ -> false)
       actions);
  Alcotest.(check (list string))
    "files after"
    (List.sort compare [ M.file_of_href b; M.file_of_href c ])
    (files dir);
  Alcotest.(check bool)
    "updated content" true
    (contains (read dir (M.file_of_href b)) "Bob Two");
  (* A stale token rebuilds the directory, keeping the members it already
     holds and pruning what the server no longer lists. *)
  let index = read dir M.index_file in
  let lines = String.split_on_char '\n' index in
  let rewritten =
    String.concat "\n"
      (List.map
         (fun l ->
           if String.starts_with ~prefix:"token\t" l then
             "token\thttp://example.com/ns/sync/bogus"
           else l)
         lines)
    ^ url ^ "stale.vcf\t\"9\"\tstale.vcf\n"
  in
  Out_channel.with_open_bin (Filename.concat dir M.index_file) (fun oc ->
      output_string oc rewritten);
  Out_channel.with_open_bin (Filename.concat dir "stale.vcf") (fun oc ->
      output_string oc "BEGIN:VCARD\r\nEND:VCARD\r\n");
  let s, actions = run t dir url in
  Alcotest.(check bool)
    "restart logged" true
    (has (function M.Restart _ -> true | _ -> false) actions);
  Alcotest.(check bool)
    "held members skipped" true
    (has (function M.Skipped _ -> true | _ -> false) actions);
  Alcotest.(check int) "nothing refetched" 0 s.fetched;
  Alcotest.(check bool)
    "pruned" true
    (has (fun a -> a = M.Pruned "stale.vcf") actions);
  Alcotest.(check (list string))
    "files rebuilt"
    (List.sort compare [ M.file_of_href b; M.file_of_href c ])
    (files dir)

let () =
  Alcotest.run "carddav-mirror"
    [ ("radicale", [ test_case "mirror an address book" test_mirror ]) ]
