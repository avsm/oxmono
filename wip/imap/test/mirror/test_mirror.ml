open Imap.Mirror

let ok = function
  | Ok x -> x
  | Error _ -> Alcotest.fail "unexpected mirror error"

let scope = {
  endpoint="imap.example";account="alice";mailbox_key="inbox";
  raw_name="INBOX";encoding=Imap.Mailbox_name.Rev1;mailbox_id=None
}

let test_initial_empty_scope () =
  match initial {scope with account=""} with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "empty scope accepted"

let test_restore_cursor () =
  let c=initial scope in
  let restored=ok (restore ~schema_version:c.schema_version ~scope:c.scope
    ~phase:c.phase ~uidvalidity:c.uidvalidity ~generation:c.generation
    ~revision:c.revision ~anchor:c.anchor ~frontier:c.frontier
    ~inventory_ref:c.inventory_ref ~mode:c.mode) in
  Alcotest.(check bool) "round trip" true (restored=c);
  (match restore ~schema_version:c.schema_version ~scope:c.scope
    ~phase:c.phase ~uidvalidity:c.uidvalidity ~generation:c.generation
    ~revision:(Int64.succ c.revision) ~anchor:c.anchor ~frontier:c.frontier
    ~inventory_ref:c.inventory_ref ~mode:c.mode with
   | Error (Invalid _) -> ()
   | _ -> Alcotest.fail "accepted inconsistent persisted counters")

let () =
  Alcotest.run "IMAP mirror"
    ["cursor", [
      Alcotest.test_case "empty scope" `Quick test_initial_empty_scope;
      Alcotest.test_case "restored cursor" `Quick test_restore_cursor]]
