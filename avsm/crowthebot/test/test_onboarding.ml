open Crowthebot

let check label value = if not value then failwith label
let admin = "@admin:example.org"
let self = "@crow:example.org"
let group = "!group:example.org"
let dm = "!dm:example.org"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let db = Sqlite3_eio.open_memory ~sw () in
  let store = Store.create db ~admin in
  let now = ref 0. and eligible = ref true and fail_send = ref false in
  let notices = ref [] and replies = ref [] and starts = ref 0 in
  let answers = ref [] in
  let make () = Onboarding.create ~store ~self ~now:(fun () -> !now)
      ~check_room:(fun room -> room = group && !eligible)
      ~send_admin:(fun text ->
        if !fail_send then failwith "send failed";
        notices := text :: !notices)
      ~verify:(fun () -> incr starts)
      ~respond:(fun ~flow_id ~accept -> answers := (flow_id, accept) :: !answers)
  in
  let t = make () in
  let group_replies = ref [] in
  let engine = Engine.create ~store ~self ~plugins:[]
      ~config:(Config.default ~admin ~homeserver:"https://matrix.example.org")
      ~now:(fun () -> !now)
      ~complete:(Fake_model.v (fun _ _ -> failwith "unexpected model call")) in
  let group_help sender id =
    Engine.handle engine ~send:(fun s -> group_replies := s :: !group_replies)
      Engine.{ room = group; sender; id; body = "!crow help" }
  in
  group_help admin "$before";
  check "disabled group ignored" (!group_replies = []);
  let handle ?(sender = admin) ?(direct = true) ?(id = "$event") body =
    Onboarding.handle t ~direct ~send:(fun text -> replies := text :: !replies)
      Engine.{ room = dm; sender; id; body }
  in
  Onboarding.request t ~room:"!other:example.org";
  check "ineligible room is not proposed" (Store.room_requests store = []);
  fail_send := true;
  (try Onboarding.request t ~room:group; failwith "expected send failure"
   with Failure message -> check "send failed" (message = "send failed"));
  check "failed send leaves durable pending request"
    (Store.room_requests store = [group] && Store.rooms store = []);
  fail_send := false;
  Onboarding.request t ~room:group;
  Onboarding.request t ~room:group;
  check "one notice and one verification" (!starts = 1 && List.length !notices = 1);
  check "join never silently enables room" (Store.rooms store = []);
  check "group command consumed"
    (handle ~direct:false "!crow room approve !group:example.org");
  check "group cannot approve" (Store.rooms store = []);
  ignore (handle ~sender:"@friend:example.org" "room approve !group:example.org");
  check "other user cannot approve" (Store.rooms store = []);
  ignore (handle ~id:"$unknown" "room approve !unknown:example.org");
  check "unknown room cannot be enabled" (Store.rooms store = []);
  eligible := false;
  ignore (handle ~id:"$left" "room approve !group:example.org");
  check "membership rechecked at approval" (Store.rooms store = []);
  eligible := true;
  ignore (handle ~id:"$approve" "room approve !group:example.org");
  check "admin DM enables room" (Store.rooms store = [group]);
  group_help admin "$after";
  check "approved group accepts commands" (List.length !group_replies = 1);
  group_help "@stranger:example.org" "$stranger";
  check "room approval does not grant stranger access"
    (List.length !group_replies = 1);
  Store.set_person store ~actor:admin ~user:"@friend:example.org"
    ~role:Store.Friend ~allowed:true;
  group_help "@friend:example.org" "$friend";
  check "approved friends can use enabled group"
    (List.length !group_replies = 2);
  let before = List.length !replies in
  ignore (handle ~id:"$approve" "room approve !group:example.org");
  check "event deduplicated" (List.length !replies = before);
  check "conversation continues to engine" (not (handle "hello"));
  let prompt user = Matrix_eio.Verification_service.{
      their_user_id = Matrix_proto.Id.User_id.of_string_exn user;
      their_device_id = None; emoji = []; decimals = (1,2,3); we_started = true }
  in
  Onboarding.prompt t ~flow_id:"foreign" (prompt "@stranger:example.org");
  check "foreign prompt refused" (!answers = [("foreign",false)]);
  Onboarding.prompt t ~flow_id:"sas" (prompt admin);
  ignore (handle ~direct:false ~id:"$group-sas" "!crow verify yes sas");
  ignore (handle ~sender:"@friend:example.org" ~id:"$friend-sas" "verify yes sas");
  check "only admin DM may answer SAS" (List.length !answers = 1);
  ignore (handle ~id:"$bad-id" "verify yes wrong");
  check "flow ID must match" (List.length !answers = 1);
  ignore (handle ~id:"$sas" "verify yes sas");
  ignore (handle ~id:"$sas-again" "verify yes sas");
  check "SAS confirmed exactly once" (!answers = [("sas",true);("foreign",false)]);
  Onboarding.prompt t ~flow_id:"old" (prompt admin);
  now := 601.;
  ignore (handle ~id:"$old" "verify yes old");
  check "expired SAS cannot be confirmed" (List.length !answers = 2);
  Onboarding.prompt t ~flow_id:"mismatch" (prompt admin);
  ignore (handle ~id:"$mismatch" "verify no mismatch");
  check "mismatch is sent" (List.hd !answers = ("mismatch",false));
  Onboarding.prompt t ~flow_id:"cancelled" (prompt admin);
  Onboarding.result t (Matrix_eio.Verification_service.Cancelled {
      user_id = Matrix_proto.Id.User_id.of_string_exn admin;
      code = Matrix_client.Verification.Cancel_code.User });
  ignore (handle ~id:"$cancelled" "verify yes cancelled");
  check "finished flows cannot be confirmed" (List.length !answers = 3);
  (* Reopen the store to exercise schema and pending/decision persistence. *)
  Store.request_room store "!reject:example.org";
  Store.decide_room store ~actor:admin ~room:"!reject:example.org" ~approve:false;
  Store.request_room store "!pending:example.org";
  let reopened = Store.create db ~admin in
  check "pending requests survive reopen"
    (Store.room_requests reopened = ["!pending:example.org"]);
  Store.request_room reopened "!reject:example.org";
  check "rejected room does not prompt again"
    (Store.room_requests reopened = ["!pending:example.org"]);
  check "approved room stays enabled" (Store.rooms reopened = [group]);
  print_endline "crowthebot: room onboarding, authority and SAS confirmation passed"
