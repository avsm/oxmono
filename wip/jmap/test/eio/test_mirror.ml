module M = Jmap.Mirror

let check name value = if not value then failwith name

let reject f =
  try
    ignore (f ());
    false
  with Invalid_argument _ -> true

let advance = function
  | M.Update u -> u
  | Restart _ -> failwith "unexpected restart"

let cursor phase state position query_state =
  M.cursor ~phase ?state ~position ?query_state ()

let test_snapshot () =
  let gets = ref [] and query_state = ref "q1" in
  let source =
    M.
      {
        get =
          (fun ~ids ->
            gets := ids :: !gets;
            {
              state = "get-later";
              items = Option.get ids;
              not_found = [];
              receipts = [ "get" ];
            });
        changes =
          (fun ~since ->
            check "anchor used" (since = "get-later");
            Ok
              {
                old_state = since;
                new_state = "delta1";
                more = false;
                created = [];
                updated = [ "b" ];
                destroyed = [ "a" ];
                receipts = [ "changes" ];
              });
        page =
          Some
            (fun ~position ->
              {
                query_state = !query_state;
                position;
                ids = (if position = 0 then [ "a" ] else [ "b" ]);
                total = Some 2;
                receipts = [ "query" ];
              });
        id = Fun.id;
        batch_size = 10;
      }
  in
  let first = advance (M.step source M.initial) in
  check "anchor acquired separately"
    (!gets = [ Some [] ] && first.cursor.phase = Listing);
  let first_page = advance (M.step source first.cursor) in
  check "short page continues"
    (first_page.cursor.position = 1 && first_page.more && not first_page.publish);
  query_state := "q2";
  check "query changes discard staging"
    (match M.step source first_page.cursor with
    | Restart [ "query" ] -> true
    | _ -> false);
  query_state := "q1";
  let second_page = advance (M.step source first_page.cursor) in
  check "snapshot stays hidden until catchup"
    (second_page.cursor.phase = Catching_up && not second_page.publish);
  let caught = advance (M.step source second_page.cursor) in
  check "publish applies races and delta cursor"
    (caught.publish && caught.cursor.phase = Live
    && caught.cursor.state = Some "delta1"
    && caught.destroyed = [ "a" ] && caught.items = [ "b" ] && not caught.more)

let test_changes () =
  let gets = ref [] in
  let source =
    M.
      {
        get =
          (fun ~ids ->
            let ids = Option.get ids in
            gets := ids :: !gets;
            {
              state = "future-get-state";
              items = List.filter (( <> ) "missing") ids;
              not_found = List.filter (( = ) "missing") ids;
              receipts = [];
            });
        page = None;
        id = Fun.id;
        batch_size = 2;
        changes =
          (fun ~since ->
            Ok
              {
                old_state = since;
                new_state = "s2";
                more = true;
                created = [ "c" ];
                updated = [ "a"; "b"; "missing" ];
                destroyed = [ "d" ];
                receipts = [];
              });
      }
  in
  let before = cursor M.Live (Some "s1") 0 None in
  let step = advance (M.step source before) in
  check "get limit respected" (List.map List.length !gets = [ 2; 2 ]);
  check "delta token survives newer get state"
    (step.cursor.state = Some "s2" && step.more);
  check "notFound races become deletions" (step.destroyed = [ "d"; "missing" ]);
  let expired =
    { source with changes = (fun ~since:_ -> Error [ "expired" ]) }
  in
  check "expired state requests new staging"
    (match M.step expired before with
    | Restart [ "expired" ] -> true
    | _ -> false);
  let mismatch =
    {
      source with
      changes =
        (fun ~since:_ ->
          Ok
            {
              old_state = "wrong";
              new_state = "s2";
              more = false;
              created = [];
              updated = [];
              destroyed = [];
              receipts = [];
            });
    }
  in
  check "mismatched sinceState rejected"
    (reject (fun () -> M.step mismatch before));
  let stalled =
    {
      source with
      changes =
        (fun ~since ->
          Ok
            {
              old_state = since;
              new_state = since;
              more = true;
              created = [];
              updated = [];
              destroyed = [];
              receipts = [];
            });
    }
  in
  check "nonadvancing delta rejected" (reject (fun () -> M.step stalled before));
  let failing =
    { source with get = (fun ~ids:_ -> failwith "connection failed") }
  in
  check "failed fetch returns no checkpoint"
    (try
       ignore (M.step failing before);
       false
     with Failure _ -> true)

let test_all () =
  let source =
    M.
      {
        get =
          (fun ~ids ->
            check "all IDs" (ids = None);
            {
              state = "s1";
              items = [ "calendar-a" ];
              not_found = [];
              receipts = [];
            });
        changes = (fun ~since:_ -> assert false);
        page = None;
        id = Fun.id;
        batch_size = 1;
      }
  in
  let step = advance (M.step source M.initial) in
  check "unpaged resources publish atomically"
    (step.publish && step.cursor.phase = Live);
  let duplicate =
    {
      source with
      get =
        (fun ~ids:_ ->
          { state = "s1"; items = [ "a"; "a" ]; not_found = []; receipts = [] });
    }
  in
  check "duplicate IDs rejected" (reject (fun () -> M.step duplicate M.initial))

let test_cursor () =
  let decode text = Jsont_bytesrw.decode_string M.cursor_jsont text in
  List.iter
    (fun cursor ->
      let text =
        Result.get_ok (Jsont_bytesrw.encode_string M.cursor_jsont cursor)
      in
      check "cursor roundtrip" (decode text = Ok cursor))
    [
      M.initial;
      M.cursor ~phase:Listing ~state:"s1" ();
      M.cursor ~phase:Listing ~state:"s1" ~position:2 ~query_state:"q1" ();
      M.cursor ~phase:Catching_up ~state:"s1" ~query_state:"q1" ();
      M.cursor ~phase:Live ~state:"s2" ();
    ];
  List.iter
    (fun text ->
      check "invalid checkpoint rejected" (Result.is_error (decode text)))
    [
      {|{"phase":"new","state":"s1","position":0}|};
      {|{"phase":"live","position":0}|};
      {|{"phase":"listing","state":"s1","position":1}|};
      {|{"phase":"catchup","state":"s1","position":0}|};
      {|{"phase":"live","state":"s1","position":-1}|};
      {|{"phase":"live","state":"s1","position":0.5}|};
      {|{"phase":"live","state":"s1","position":9007199254740992}|};
    ];
  check "constructor enforces checkpoint invariants"
    (reject (fun () -> M.cursor ~phase:Live ()));
  if Int64.of_int max_int > Jmap.Proto.Int53.Unsigned.max_value then
    check "constructor rejects positions its codec cannot encode"
      (reject (fun () -> M.cursor ~phase:Live ~state:"s1" ~position:max_int ()))

let () =
  test_cursor ();
  test_snapshot ();
  test_changes ();
  test_all ();
  print_endline "Restartable mirror tests passed."
