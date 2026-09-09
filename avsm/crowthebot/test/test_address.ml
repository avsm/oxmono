open Crowthebot

let check name value = if not value then failwith name
let self = "@crow:example.org"
let admin = "@admin:example.org"
let alice = "@alice:example.org"
let room = "!room:example.org"
let dm = "!dm:example.org"
let json s = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json s)

let () =
  let command ?(mentioned = false) ?(direct = false) text =
    Address.command ~self ~mentioned ~direct text
  in
  List.iter
    (fun (input, output) -> check input (command input = output))
    [
      ("!crow", Some "help");
      ("!crow\task hi", Some "ask hi");
      ("!crowbar hi", None);
      (self, Some "help");
      (self ^ ": reset", Some "reset");
      (self ^ ", !crow help", Some "help");
      ("hello " ^ self ^ "!", Some ("hello " ^ self ^ "!"));
      (self ^ ".evil hi", None);
      (self ^ "2 hi", None);
      ("x" ^ self, None);
      ("Crow: hello", None);
      ("hello", None);
    ];
  check "structured mention with display name"
    (command ~mentioned:true "Crow: hello" = Some "Crow: hello");
  check "prefix-free DM" (command ~direct:true "hello" = Some "hello");
  check "DM other prefix" (command ~direct:true "!other hi" = Some "!other hi");
  check "empty DM" (command ~direct:true " \n " = None);
  check "real mention"
    (Address.mentions ~self
       (json {|{"m.mentions":{"user_ids":["@crow:example.org"]}}|}));
  List.iter
    (fun content ->
      check "unrelated or malformed mention"
        (not (Address.mentions ~self (json content))))
    [
      {|{"m.mentions":{"room":true}}|};
      {|{"m.mentions":{"user_ids":["@other:example.org"]}}|};
      {|{"m.mentions":{"user_ids":"@crow:example.org"}}|};
      {|{"m.new_content":{"m.mentions":{"user_ids":["@crow:example.org"]}}}|};
    ];
  let quote = "> <" ^ self ^ "> !crow deny " ^ alice ^ "\n> quoted\n\nhello" in
  check "reply fallback stripped" (Address.body ~reply:true quote = "hello");
  check "quote cannot trigger a reply"
    (command (Address.body ~reply:true quote) = None);
  let peer ?(marked = true) ?(complete = true) members =
    Address.direct_peer ~self ~marked ~complete members
  in
  check "two-person DM" (peer [ self; alice ] = Some alice);
  check "unmarked pair is a group" (peer ~marked:false [ self; alice ] = None);
  check "incomplete members fail closed"
    (peer ~complete:false [ self; alice ] = None);
  check "third member disables DM" (peer [ self; alice; admin ] = None);
  check "own membership required" (peer [ alice; admin ] = None);
  check "explicit confirmation"
    (Verification.affirmative " YES "
    && (not (Verification.affirmative ""))
    && (not (Verification.affirmative "maybe"))
    && not (Verification.affirmative "no"));
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let db = Sqlite3_eio.open_memory ~sw () in
  (* Opening a version-one profile must retain authority and enabled rooms. *)
  Sqlite3.Rc.check
    (Sqlite3_eio.exec db
       (Printf.sprintf
          "PRAGMA user_version=1; CREATE TABLE settings(key TEXT PRIMARY \
           KEY,value TEXT); INSERT INTO settings VALUES('admin','%s'); CREATE \
           TABLE rooms(room TEXT PRIMARY KEY); INSERT INTO rooms VALUES('%s');"
          admin room));
  let store = Store.create db ~admin in
  check "migration preserves rooms" (Store.rooms store = [ room ]);
  Store.add_direct_room store ~room:dm ~peer:alice;
  check "DM stored separately" (not (List.mem dm (Store.rooms store)));
  let store = Store.create db ~admin in
  check "DM peer survives reopen" (Store.direct_peer store dm = Some alice);
  let calls = ref 0 and clock = ref 0. and replies = ref [] in
  let config =
    {
      (Config.default ~admin ~homeserver:"https://matrix.example.org") with
      plugins = [];
    }
  in
  let engine =
    Engine.create ~config ~store ~self ~plugins:[]
      ~complete:(fun _ _ ->
        incr calls;
        (Some "hello", []))
      ~now:(fun () -> !clock)
  in
  let send s = replies := s :: !replies in
  let event ?(sender = alice) ?(room = room) id body =
    Engine.{ sender; room; id; body }
  in
  let handle ?mentioned ?direct e =
    Engine.handle engine ?mentioned ?direct ~send e
  in
  handle ~direct:true (event ~room:dm "unknown-dm" "hello");
  handle ~mentioned:true (event "unknown-mention" "Crow: hi");
  check "DM and mentions require approval" (!calls = 0 && !replies = []);
  handle ~direct:true
    (event ~sender:admin ~room:dm "allow" ("allow " ^ alice ^ " friend"));
  check "admin approves from DM" (Store.person store alice).allowed;
  handle (event "exact-mention" (self ^ ": hello"));
  check "exact account mention reaches model" (!calls = 1);
  handle ~direct:true (event ~room:dm "dm" "hello");
  check "DM independent cooldown" (!calls = 2);
  check "group and DM context isolated"
    (List.length (Store.history store ~room ~user:alice) = 2
    && List.length (Store.history store ~room:dm ~user:alice) = 2);
  clock := 20.;
  handle ~direct:true (event ~room:dm "dm" "hello");
  check "DM replay suppressed" (!calls = 2);
  handle ~mentioned:true (event "structured" "Crow: hello");
  check "structured mention reaches model" (!calls = 3);
  handle ~mentioned:true (event ~room:"!disabled:example.org" "disabled" "hi");
  handle ~direct:true (event ~sender:self ~room:dm "own" "hi");
  check "self and disabled groups ignored" (!calls = 3);
  handle ~direct:true
    (event ~room:dm "no-grant" "allow @other:example.org friend");
  check "DM cannot forge authority"
    (not (Store.person store "@other:example.org").allowed);
  handle ~direct:true (event ~room:dm "reset" "reset");
  check "DM reset is local"
    (Store.history store ~room:dm ~user:alice = []
    && Store.history store ~room ~user:alice <> []);
  handle ~direct:true (event ~sender:admin ~room:dm "deny" ("deny " ^ alice));
  clock := 40.;
  handle ~direct:true (event ~room:dm "revoked" "hello");
  handle ~mentioned:true (event "revoked-mention" "hi");
  check "revocation applies to every address" (!calls = 3);
  check "revocation clears group and DM context"
    (Store.history store ~room ~user:alice = []
    && Store.history store ~room:dm ~user:alice = []);
  print_endline "crowthebot: mentions, direct messages and migration passed"
