(** Tests for the Eio-based UI models. *)

module Ui = Matrix_ui
module Olm = Matrix_client.Olm
module Ck = Matrix_client.Crypto_key
module Backup = Matrix_client.Backup
module Recovery = Matrix_eio.Recovery
module Event = Matrix_proto.Event

let the_room = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org"
let alice = Matrix_proto.Id.User_id.of_string_exn "@alice:example.org"
let event_id id = Matrix_proto.Id.Event_id.of_string_exn id

(* Varying bytes, not a constant source: two sends off the same source must
   get two transaction ids, and the identity of a send is its. *)
let queue_for user_id =
  Matrix_client.Send_queue.create
    ~random:
      (Matrix_client.Random.of_source
         (Eio.Flow.string_source
            (String.init 4096 (fun index -> Char.chr (index * 37 mod 256)))))
    ~user_id ()

let client_over handler =
  Matrix_client.Client.create
    ~config:
      (Matrix_client.Client.config
         ~homeserver:(Uriz.of_string_exn "https://hs.example")
         ())
    ~fetch:(Fetch_mock.client handler)
    ~random:
      (Matrix_client.Random.of_source
         (Eio.Flow.string_source (String.make 4096 'r')))

(* A timeline that is only ever projected, never paginated, still needs a
   client to be built with. *)
let offline_client () =
  client_over (fun _ -> Alcotest.fail "this test makes no request")

let check_contains name needle value =
  Alcotest.(check bool) name true (Ui.Matching.contains ~haystack:value ~needle)

let test_sanitize_html () =
  let html =
    Ui.Presentation.Html.sanitize
      {|<p onclick="bad()">Hello<script>alert(1)</script><a href="javascript:bad()">bad</a><strong>world</strong></p>|}
  in
  Alcotest.(check bool)
    "script element removed" false
    (Ui.Matching.contains ~haystack:html ~needle:"<script");
  Alcotest.(check bool)
    "handler removed" false
    (Ui.Matching.contains ~haystack:html ~needle:"onclick");
  Alcotest.(check bool)
    "unsafe URL removed" false
    (Ui.Matching.contains ~haystack:html ~needle:"javascript:");
  check_contains "safe markup retained" "<strong>world</strong>" html

let test_sanitize_malformed_uris () =
  let html =
    Ui.Presentation.Html.sanitize
      ({|<a href="https://example.org/%xx">bad link</a>|}
      ^ {|<img src="mxc://example.org/%xx">|}
      ^ {|<a href="HTTPS://example.org/ok">good link</a>|})
  in
  Alcotest.(check bool)
    "malformed URI attributes removed" false
    (Ui.Matching.contains ~haystack:html ~needle:"%xx");
  check_contains "link text retained" "bad link" html;
  check_contains "valid mixed-case scheme retained" "href=" html

let test_plain_reply_and_unicode () =
  Alcotest.(check string)
    "reply fallback omitted" "Real answer"
    (Ui.Presentation.Html.to_plain
       "<mx-reply><blockquote>old message</blockquote></mx-reply><p>Real \
        answer</p>");
  Alcotest.(check bool)
    "Unicode case fold" true
    (Ui.Matching.contains ~haystack:"Straße Café" ~needle:"STRASSE CAFE\u{0301}");
  Alcotest.(check string)
    "grapheme-safe truncate" "👨‍👩‍👧‍👦"
    (Ui.Matching.truncate_graphemes ~max:1 "👨‍👩‍👧‍👦!")

let raw json =
  match Jsont_bytesrw.decode_string Matrix_proto.Event.Raw_event.jsont json with
  | Ok event -> event
  | Error message -> Alcotest.fail message

let message ?(id = "$one") ?(sender = "@alice:example.org")
    ?(ts = 1_700_000_000_000L) content =
  raw
    (Printf.sprintf
       {|{"event_id":"%s","sender":"%s","origin_server_ts":%Ld,"type":"m.room.message","content":%s}|}
       id sender ts content)

let edit ?(id = "$edit") ?(sender = "@alice:example.org")
    ?(ts = 1_700_000_001_000L) ?(target = "$one") ?(body = "edited") () =
  message ~id ~sender ~ts
    (Printf.sprintf
       {|{"msgtype":"m.text","body":"* %s","m.new_content":{"msgtype":"m.text","body":"%s"},"m.relates_to":{"rel_type":"m.replace","event_id":"%s"}}|}
       body body target)

let edit_without_new_content ?(id = "$invalid") ?(sender = "@alice:example.org")
    ?(ts = 1_700_000_002_000L) ?(target = "$one") () =
  message ~id ~sender ~ts
    (Printf.sprintf
       {|{"msgtype":"m.text","body":"invalid","m.relates_to":{"rel_type":"m.replace","event_id":"%s"}}|}
       target)

let redaction ?(id = "$redaction") ?(sender = "@alice:example.org")
    ?(ts = 1_700_000_003_000L) target =
  raw
    (Printf.sprintf
       {|{"event_id":"%s","sender":"%s","origin_server_ts":%Ld,"type":"m.room.redaction","redacts":"%s","content":{}}|}
       id sender ts target)

let test_presentation () =
  let event =
    message
      {|{"msgtype":"m.text","body":"hello","format":"org.matrix.custom.html","formatted_body":"<b onclick='x'>hello</b>"}|}
    |> Ui.Presentation.of_event
  in
  (match event.content with
  | Ui.Presentation.Message { body; formatted = Some formatted; _ } ->
      Alcotest.(check string) "body" "hello" body;
      Alcotest.(check string) "safe tag retained" "<b>hello</b>" formatted.html
  | _ -> Alcotest.fail "expected a formatted message");
  let old_redaction =
    raw
      {|{"event_id":"$r","sender":"@alice:example.org","origin_server_ts":1700000000001,"type":"m.room.redaction","redacts":"$one","content":{"reason":"spam"}}|}
    |> Ui.Presentation.of_event
  in
  match old_redaction.content with
  | Ui.Presentation.Redaction { target = Some target; reason = Some "spam" } ->
      Alcotest.(check string)
        "pre-v11 redaction target" "$one"
        (Matrix_proto.Id.Event_id.to_string target)
  | _ -> Alcotest.fail "expected a pre-v11 redaction"

(* The transition, plus whether the sender is the subject, decides what a
   membership event says. *)

let member ?(sender = "@alice:example.org") ?(state_key = "@alice:example.org")
    ?prev content =
  let unsigned =
    match prev with
    | None -> ""
    | Some prev -> Printf.sprintf {|,"unsigned":{"prev_content":%s}|} prev
  in
  raw
    (Printf.sprintf
       {|{"event_id":"$m","sender":"%s","origin_server_ts":1700000000000,"type":"m.room.member","state_key":"%s","content":%s%s}|}
       sender state_key content unsigned)
  |> Ui.Presentation.of_event

let membership_of (event : Ui.Presentation.t) =
  match event.content with
  | Ui.Presentation.Membership { change; _ } -> change
  | Ui.Presentation.Profile _ -> Alcotest.fail "expected a membership change"
  | _ -> Alcotest.fail "expected a member event"

let state_of json =
  match (Ui.Presentation.of_event (raw json)).content with
  | Ui.Presentation.State { state; _ } -> state
  | _ -> Alcotest.fail "expected a state event"

let membership body = Printf.sprintf {|{"membership":"%s"}|} body

let test_membership_changes () =
  let check name expected event =
    Alcotest.(check bool) name true (membership_of event = expected)
  in
  (* No prev_content counts as "leave", exactly as ruma does. *)
  check "a first join is Joined" Ui.Presentation.Joined
    (member (membership "join"));
  check "a join over an invite is the invitation accepted"
    Ui.Presentation.Invitation_accepted
    (member ~prev:(membership "invite") (membership "join"));
  check "leaving one's own join is Left" Ui.Presentation.Left
    (member ~prev:(membership "join") (membership "leave"));
  check "someone else writing that leave is a removal" Ui.Presentation.Kicked
    (member ~sender:"@mod:example.org" ~prev:(membership "join")
       (membership "leave"));
  check "a ban over a join is a removal and a ban"
    Ui.Presentation.Kicked_and_banned
    (member ~sender:"@mod:example.org" ~prev:(membership "join")
       (membership "ban"));
  check "a ban over a leave is a ban" Ui.Presentation.Banned
    (member ~sender:"@mod:example.org" ~prev:(membership "leave")
       (membership "ban"));
  check "a leave over a ban is an unban" Ui.Presentation.Unbanned
    (member ~sender:"@mod:example.org" ~prev:(membership "ban")
       (membership "leave"));
  check "an invite over a leave is an invitation" Ui.Presentation.Invited
    (member ~sender:"@bob:example.org" ~prev:(membership "leave")
       (membership "invite"));
  check "leaving one's own invite rejects it"
    Ui.Presentation.Invitation_rejected
    (member ~prev:(membership "invite") (membership "leave"));
  check "the inviter withdrawing it revokes it"
    Ui.Presentation.Invitation_revoked
    (member ~sender:"@bob:example.org" ~prev:(membership "invite")
       (membership "leave"));
  check "a knock over a leave is a knock" Ui.Presentation.Knocked
    (member ~prev:(membership "leave") (membership "knock"));
  check "an invite over a knock accepts it" Ui.Presentation.Knock_accepted
    (member ~sender:"@bob:example.org" ~prev:(membership "knock")
       (membership "invite"));
  check "withdrawing one's own knock" Ui.Presentation.Knock_retracted
    (member ~prev:(membership "knock") (membership "leave"));
  check "someone else denying it" Ui.Presentation.Knock_denied
    (member ~sender:"@bob:example.org" ~prev:(membership "knock")
       (membership "leave"));
  check "an unchanged join changed nothing" Ui.Presentation.No_change
    (member ~prev:(membership "join") (membership "join"));
  check "an invite over a join is not a legal transition"
    Ui.Presentation.Invalid
    (member ~sender:"@bob:example.org" ~prev:(membership "join")
       (membership "invite"));
  check "an unknown membership is retained as an unknown transition"
    Ui.Presentation.Unknown_membership
    (member ~prev:(membership "future") (membership "join"));
  Alcotest.(check (option string))
    "and a join reads as a sentence, not as a type"
    (Some "@alice:example.org joined")
    (Ui.Presentation.preview (member (membership "join")));
  Alcotest.(check (option string))
    "as does a removal" (Some "@alice:example.org was removed")
    (Ui.Presentation.preview
       (member ~sender:"@mod:example.org" ~prev:(membership "join")
          (membership "leave")))

let test_profile_change () =
  let event =
    member ~prev:{|{"membership":"join","displayname":"Alice"}|}
      {|{"membership":"join","displayname":"Alice Liddell"}|}
  in
  (match event.content with
  | Ui.Presentation.Profile { change; _ } -> (
      Alcotest.(check bool)
        "the avatar did not change" true (change.avatar_url = None);
      match change.displayname with
      | Some { previous; current } ->
          Alcotest.(check (option string))
            "the old name" (Some "Alice") previous;
          Alcotest.(check (option string))
            "the new name" (Some "Alice Liddell") current
      | None -> Alcotest.fail "expected a display-name change")
  | _ -> Alcotest.fail "expected a profile change");
  Alcotest.(check (option string))
    "which reads as a sentence"
    (Some "@alice:example.org changed their display name to Alice Liddell")
    (Ui.Presentation.preview event);
  (* ruma only calls it a profile change when the member wrote it
     themselves; a moderator rewriting it is no change at all. *)
  Alcotest.(check bool)
    "a moderator rewriting a name is not a profile change" true
    (membership_of
       (member ~sender:"@mod:example.org"
          ~prev:{|{"membership":"join","displayname":"Alice"}|}
          {|{"membership":"join","displayname":"Mallory"}|})
    = Ui.Presentation.No_change)

let test_other_state () =
  Alcotest.(check bool)
    "a name event carries the name" true
    (state_of
       {|{"event_id":"$n","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.name","state_key":"","content":{"name":"Kitchen"}}|}
    = Ui.Presentation.Room_name (Some "Kitchen"));
  Alcotest.(check (option string))
    "and reads with the actor and the new value"
    (Some "@alice:example.org changed the room name to Kitchen")
    (Ui.Presentation.preview
       (Ui.Presentation.of_event
          (raw
             {|{"event_id":"$n","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.name","state_key":"","content":{"name":"Kitchen"}}|})));
  Alcotest.(check bool)
    "an unmodelled state type keeps its type" true
    (state_of
       {|{"event_id":"$x","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"com.example.thing","state_key":"","content":{}}|}
    = Ui.Presentation.Other_state_type "com.example.thing")

let test_beacon_info_presentation () =
  let beacon state_key content =
    Ui.Presentation.of_event
      (raw
         (Printf.sprintf
            {|{"event_id":"$beacon","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"org.matrix.msc3672.beacon_info",%s"content":%s}|}
            (match state_key with
            | None -> ""
            | Some key -> Printf.sprintf "\"state_key\":\"%s\", " key)
            content))
  in
  let start =
    beacon (Some "@alice:example.org")
      {|{"description":"Alice's walk","live":true,"timeout":60000,"org.matrix.msc3488.ts":1700000000000}|}
  in
  (match start.content with
  | Ui.Presentation.State
      { state = Ui.Presentation.Beacon_info info; state_key; _ } ->
      Alcotest.(check string) "beacon state key" "@alice:example.org" state_key;
      Alcotest.(check (option string))
        "beacon description" (Some "Alice's walk") info.description;
      Alcotest.(check bool) "beacon is live" true info.live
  | _ -> Alcotest.fail "expected a typed beacon_info state");
  Alcotest.(check (option string))
    "beacon preview"
    (Some "@alice:example.org started sharing their location: Alice's walk")
    (Ui.Presentation.preview start);
  Alcotest.(check bool)
    "live beacon is preview-worthy" true
    (Ui.Presentation.is_preview_worthy start);
  let stop =
    beacon (Some "@alice:example.org")
      {|{"description":"Alice's walk","live":false,"timeout":60000,"org.matrix.msc3488.ts":1700000000000}|}
  in
  Alcotest.(check bool)
    "stopped beacon remains preview-worthy" true
    (Ui.Presentation.is_preview_worthy stop);
  let malformed =
    beacon (Some "@alice:example.org") {|{"description":"broken","live":true}|}
  in
  (match malformed.content with
  | Ui.Presentation.Malformed { event_type; _ } ->
      Alcotest.(check string)
        "malformed beacon type" "org.matrix.msc3672.beacon_info" event_type
  | _ -> Alcotest.fail "malformed beacon_info must remain malformed");
  Alcotest.(check bool)
    "malformed beacon is not preview-worthy" false
    (Ui.Presentation.is_preview_worthy malformed);
  let invalid_key =
    beacon (Some "not-a-user-id") {|{"live":true,"timeout":60000}|}
  in
  match invalid_key.content with
  | Ui.Presentation.Malformed { reason; _ } ->
      Alcotest.(check string)
        "invalid beacon state key" "beacon_info state key is not a user id"
        reason
  | _ -> Alcotest.fail "invalid beacon state key must remain malformed"

(* The predicate a room list uses to pick a preview: matrix-rust-sdk's
   [filter_timeline_event]. *)
let test_preview_worthy () =
  let worthy json =
    Ui.Presentation.is_preview_worthy (Ui.Presentation.of_event (raw json))
  in
  Alcotest.(check bool)
    "a message is" true
    (Ui.Presentation.is_preview_worthy
       (Ui.Presentation.of_event
          (message {|{"msgtype":"m.text","body":"hello"}|})));
  Alcotest.(check bool)
    "a verification request is not" false
    (Ui.Presentation.is_preview_worthy
       (Ui.Presentation.of_event
          (message
             {|{"msgtype":"m.key.verification.request","body":"request","methods":[]}|})));
  Alcotest.(check bool)
    "an edit is not" false
    (Ui.Presentation.is_preview_worthy
       (Ui.Presentation.of_event
          (message
             {|{"msgtype":"m.text","body":"* new","m.new_content":{"msgtype":"m.text","body":"new"},"m.relates_to":{"rel_type":"m.replace","event_id":"$one"}}|})));
  Alcotest.(check bool)
    "a membership change is not" false
    (worthy
       {|{"event_id":"$m","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.member","state_key":"@alice:example.org","content":{"membership":"join"}}|});
  Alcotest.(check bool)
    "a name change is not" false
    (worthy
       {|{"event_id":"$n","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.name","state_key":"","content":{"name":"Kitchen"}}|});
  Alcotest.(check bool)
    "a reaction is not" false
    (worthy
       {|{"event_id":"$r","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.reaction","content":{"m.relates_to":{"rel_type":"m.annotation","event_id":"$one","key":"a"}}}|});
  Alcotest.(check bool)
    "a redacted message is not" false
    (worthy
       {|{"event_id":"$d","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{}}|});
  Alcotest.(check bool)
    "an undecrypted event is not" false
    (worthy
       {|{"event_id":"$e","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.encrypted","content":{"algorithm":"m.megolm.v1.aes-sha2"}}|});
  Alcotest.(check bool)
    "a sticker is" true
    (worthy
       {|{"event_id":"$s","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.sticker","content":{"body":"hi","url":"mxc://x/y"}}|})

let unique values = List.sort_uniq Int.compare values

let reconcile_property =
  let open QCheck in
  Test.make ~name:"observable reconciliation reaches target" ~count:500
    (pair (list nat_small) (list nat_small))
    (fun (before, after) ->
      Eio_main.run @@ fun _ ->
      let before = unique before and after = unique after in
      let observable = Ui.Observable.List.create before in
      Ui.Observable.List.reconcile_by ~key:Fun.id ~equal:Int.equal observable
        after;
      Array.to_list (Ui.Observable.List.snapshot observable) = after)

let test_reconcile_reset_and_granular_diffs () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let before = List.init 300 Fun.id in
  let after = List.rev before in
  let large = Ui.Observable.List.create before in
  let initial, subscription = Ui.Observable.List.subscribe ~sw large in
  Ui.Observable.List.reconcile_by ~key:Fun.id ~equal:Int.equal large after;
  let diffs = Option.get (Ui.Observable.List.next subscription) in
  Alcotest.(check bool)
    "large reversal publishes a reset" true
    (List.exists
       (function Ui.Observable.List.Reset _ -> true | _ -> false)
       diffs);
  Alcotest.(check int)
    "large reversal publishes one reset" 1
    (List.length
       (List.filter
          (function Ui.Observable.List.Reset _ -> true | _ -> false)
          diffs));
  Alcotest.(check (list int))
    "reset applies to the target" after
    (Ui.Observable.List.apply_all initial diffs |> Array.to_list);
  let small = Ui.Observable.List.create [ 1; 2; 3 ] in
  let initial, subscription = Ui.Observable.List.subscribe ~sw small in
  Ui.Observable.List.reconcile_by ~key:Fun.id ~equal:Int.equal small [ 2; 1; 3 ];
  let diffs = Option.get (Ui.Observable.List.next subscription) in
  Alcotest.(check bool)
    "small reorder stays granular" false
    (List.exists
       (function Ui.Observable.List.Reset _ -> true | _ -> false)
       diffs);
  Alcotest.(check (list int))
    "small reorder applies" [ 2; 1; 3 ]
    (Ui.Observable.List.apply_all initial diffs |> Array.to_list);
  let long_prepend = Ui.Observable.List.create before in
  let initial, subscription = Ui.Observable.List.subscribe ~sw long_prepend in
  let prepended = -1 :: before in
  Ui.Observable.List.reconcile_by ~key:Fun.id ~equal:Int.equal long_prepend
    prepended;
  let diffs = Option.get (Ui.Observable.List.next subscription) in
  Alcotest.(check bool)
    "one change to a long list stays granular" false
    (List.exists
       (function Ui.Observable.List.Reset _ -> true | _ -> false)
       diffs);
  Alcotest.(check (list int))
    "long-list granular change applies" prepended
    (Ui.Observable.List.apply_all initial diffs |> Array.to_list);
  Ui.Observable.List.reconcile_by ~key:Fun.id ~equal:Int.equal small [ 2; 1; 3 ];
  Alcotest.(check (list int))
    "no-op leaves the snapshot unchanged" [ 2; 1; 3 ]
    (Ui.Observable.List.snapshot small |> Array.to_list);
  let duplicates = Ui.Observable.List.create [ 1; 1; 2 ] in
  Ui.Observable.List.reconcile_by ~key:Fun.id ~equal:Int.equal duplicates
    [ 1; 2; 1 ];
  Alcotest.(check (list int))
    "duplicate keys retain their values" [ 1; 2; 1 ]
    (Ui.Observable.List.snapshot duplicates |> Array.to_list)

let test_timeline_aggregation () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org" in
  let base = message {|{"msgtype":"m.text","body":"old"}|} in
  let reaction =
    raw
      {|{"event_id":"$reaction","sender":"@bob:example.org","origin_server_ts":1700000001000,"type":"m.reaction","content":{"m.relates_to":{"rel_type":"m.annotation","event_id":"$one","key":"👍"}}}|}
  in
  let edit =
    message ~id:"$edit" ~ts:1_700_000_002_000L
      {|{"msgtype":"m.text","body":"new","m.new_content":{"msgtype":"m.text","body":"new"},"m.relates_to":{"rel_type":"m.replace","event_id":"$one"}}|}
  in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.prepend cache room_id ~events:[ base; reaction; edit ]
    ~prev_batch:None;
  let timeline =
    Ui.Room_timeline.create ~sw ~client:(offline_client ())
      ~send_queue:(queue_for alice) cache room_id
  in
  let events =
    Ui.Room_timeline.snapshot timeline
    |> Array.to_list
    |> List.filter_map (function
      | Ui.Room_timeline.Event event -> Some event
      | _ -> None)
  in
  match events with
  | [ event ] ->
      Alcotest.(check bool) "edited" true event.edited;
      Alcotest.(check int) "one reaction" 1 (List.length event.reactions);
      Alcotest.(check int) "reaction count" 1 (List.hd event.reactions).count;
      Alcotest.(check (option string))
        "edited body" (Some "new")
        (Ui.Presentation.preview event.event);
      Alcotest.(check (option string))
        "edit preserves the target event id" (Some "$one")
        (Option.map Matrix_proto.Id.Event_id.to_string event.event.event_id)
  | _ ->
      Alcotest.failf "expected one displayed event, got %d" (List.length events)

let test_timeline_relation_index_refresh () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let base = message {|{"msgtype":"m.text","body":"old"}|} in
  let reaction =
    raw
      {|{"event_id":"$reaction-index","sender":"@bob:example.org","origin_server_ts":1700000001000,"type":"m.reaction","content":{"m.relates_to":{"rel_type":"m.annotation","event_id":"$one","key":"👍"}}}|}
  in
  let initial_edit = edit ~id:"$edit-index" ~body:"new" () in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.prepend cache the_room
    ~events:[ base; reaction; initial_edit ]
    ~prev_batch:None;
  let timeline =
    Ui.Room_timeline.create ~sw ~client:(offline_client ())
      ~send_queue:(queue_for alice) cache the_room
  in
  let displayed () =
    Ui.Room_timeline.snapshot timeline
    |> Array.to_list
    |> List.find_map (function
      | Ui.Room_timeline.Event event
        when Option.equal String.equal
               (Option.map Matrix_proto.Id.Event_id.to_string
                  event.event.event_id)
               (Some "$one") ->
          Some event
      | _ -> None)
  in
  let initial = Option.get (displayed ()) in
  Alcotest.(check int)
    "initial reaction is indexed" 1
    (List.length initial.reactions);
  Alcotest.(check bool) "initial edit is indexed" true initial.edited;
  Ui.Event_cache.prepend cache the_room
    ~events:[ redaction ~id:"$redact-reaction-index" "$reaction-index" ]
    ~prev_batch:None;
  Ui.Room_timeline.refresh timeline;
  let after_reaction_redaction = Option.get (displayed ()) in
  Alcotest.(check int)
    "redacted reaction leaves the index" 0
    (List.length after_reaction_redaction.reactions);
  Alcotest.(check bool)
    "redacting a relation preserves edit projection" true
    after_reaction_redaction.edited;
  let older_same_sender =
    raw
      {|{"event_id":"$reaction-index-older","sender":"@bob:example.org","origin_server_ts":1699999999000,"type":"m.reaction","content":{"m.relates_to":{"rel_type":"m.annotation","event_id":"$one","key":"👍"}}}|}
  in
  let older_other_sender =
    raw
      {|{"event_id":"$reaction-index-carol","sender":"@carol:example.org","origin_server_ts":1699999998000,"type":"m.reaction","content":{"m.relates_to":{"rel_type":"m.annotation","event_id":"$one","key":"👍"}}}|}
  in
  Ui.Event_cache.prepend cache the_room
    ~events:
      [
        older_other_sender;
        older_same_sender;
        edit ~id:"$edit-index-tie" ~ts:1_700_000_003_000L ~body:"tie-older" ();
        edit ~id:"$edit-index-newer" ~ts:1_700_000_003_000L ~body:"newer" ();
      ]
    ~prev_batch:None;
  Ui.Room_timeline.refresh timeline;
  let after_add = Option.get (displayed ()) in
  Alcotest.(check int)
    "added reactions are indexed" 1
    (List.length after_add.reactions);
  let reaction = List.hd after_add.reactions in
  Alcotest.(check int) "same-sender reactions remain unique" 2 reaction.count;
  Alcotest.(check (list string))
    "reaction senders stay newest first"
    [ "@bob:example.org"; "@carol:example.org" ]
    (List.map Matrix_proto.Id.User_id.to_string reaction.senders);
  Alcotest.(check (option string))
    "newest edit wins" (Some "newer")
    (Ui.Presentation.preview after_add.event);
  Ui.Event_cache.prepend cache the_room
    ~events:[ redaction ~id:"$redact-target-index" "$one" ]
    ~prev_batch:None;
  Ui.Room_timeline.refresh timeline;
  let after_target_redaction = Option.get (displayed ()) in
  Alcotest.(check bool)
    "target redaction is indexed" true after_target_redaction.redacted;
  Ui.Event_cache.forget_room cache the_room;
  Ui.Room_timeline.refresh timeline;
  Alcotest.(check bool)
    "forget clears relation sources" true
    (Option.is_none (displayed ()));
  Ui.Event_cache.prepend cache the_room ~events:[ base ] ~prev_batch:None;
  Ui.Room_timeline.refresh timeline;
  let after_reinsert = Option.get (displayed ()) in
  Alcotest.(check int)
    "reinserted target has no stale reactions" 0
    (List.length after_reinsert.reactions);
  Alcotest.(check bool)
    "reinserted target has no stale edit" false after_reinsert.edited;
  Alcotest.(check bool)
    "reinserted target has no stale redaction" false after_reinsert.redacted

let revision_ids timeline =
  Ui.Room_timeline.edit_revisions timeline ~event_id:(event_id "$one")
  |> List.map (fun (revision : Ui.Presentation.t) ->
      Option.map Matrix_proto.Id.Event_id.to_string revision.event_id)

let make_timeline sw cache =
  Ui.Room_timeline.create ~sw ~client:(offline_client ())
    ~send_queue:(queue_for alice) cache the_room

let test_edit_revisions_order_and_duplicates () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.prepend cache the_room
    ~events:
      [
        message ~ts:1_700_000_000_000L {|{"msgtype":"m.text","body":"old"}|};
        edit ~id:"$late" ~ts:1_700_000_003_000L ~body:"late" ();
        edit ~id:"$early" ~ts:1_700_000_001_000L ~body:"early" ();
        edit ~id:"$early" ~ts:1_700_000_001_000L ~body:"early" ();
      ]
    ~prev_batch:None;
  let timeline = make_timeline sw cache in
  Alcotest.(check (list (option string)))
    "original and revisions are chronological and unique"
    [ Some "$one"; Some "$early"; Some "$late" ]
    (revision_ids timeline)

let test_edit_revisions_skip_invalid_and_redacted () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.prepend cache the_room
    ~events:
      [
        message {|{"msgtype":"m.text","body":"old"}|};
        edit ~id:"$valid" ~body:"valid" ();
        edit_without_new_content ();
        edit ~id:"$wrong-sender" ~sender:"@mallory:example.org" ();
        redaction "$valid";
      ]
    ~prev_batch:None;
  let timeline = make_timeline sw cache in
  Alcotest.(check (list (option string)))
    "invalid and redacted edits are omitted" [ Some "$one" ]
    (revision_ids timeline)

let encrypted_event ?(id = "$one") ?(ts = 1_700_000_000_000L) () =
  raw
    (Printf.sprintf
       {|{"event_id":"%s","sender":"@alice:example.org","origin_server_ts":%Ld,"type":"m.room.encrypted","content":{"algorithm":"m.megolm.v1.aes-sha2","ciphertext":"ciphertext","device_id":"ALICE","session_id":"session"}}|}
       id ts)

let test_edit_revisions_encrypted_provenance () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let check name ~edit_wire ~edit_plain expected =
    let cache = Ui.Event_cache.create () in
    let original_wire = encrypted_event () in
    Ui.Event_cache.prepend cache the_room
      ~events:[ original_wire; edit_wire ]
      ~prev_batch:None;
    Alcotest.(check bool)
      (name ^ " original decrypted")
      true
      (Ui.Event_cache.set_decrypted cache the_room ~encrypted:original_wire
         ~plaintext:(message ~id:"$one" {|{"msgtype":"m.text","body":"old"}|}));
    (match edit_plain with
    | Some plaintext ->
        Alcotest.(check bool)
          (name ^ " edit decrypted") true
          (Ui.Event_cache.set_decrypted cache the_room ~encrypted:edit_wire
             ~plaintext)
    | None -> ());
    let timeline = make_timeline sw cache in
    Alcotest.(check (list (option string)))
      name expected (revision_ids timeline)
  in
  check "encrypted edit is accepted"
    ~edit_wire:(encrypted_event ~id:"$encrypted-edit" ~ts:1_700_000_001_000L ())
    ~edit_plain:
      (Some
         (edit ~id:"$encrypted-edit" ~ts:1_700_000_001_000L ~body:"encrypted" ()))
    [ Some "$one"; Some "$encrypted-edit" ];
  check "plaintext edit of encrypted event is rejected"
    ~edit_wire:(edit ~id:"$plaintext-edit" ~ts:1_700_000_001_000L ())
    ~edit_plain:None [ Some "$one" ]

let state_event ~id ~type_ content =
  raw
    (Printf.sprintf
       {|{"event_id":"%s","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"%s","state_key":"","content":%s}|}
       id type_ content)

let timeline_events timeline =
  Ui.Room_timeline.snapshot timeline
  |> Array.to_list
  |> List.filter_map (function
    | Ui.Room_timeline.Event event -> Some event
    | Ui.Room_timeline.Virtual _ -> None)

(* Adding state types composes with the default filter: relation events remain
   aggregate-only unless the caller explicitly admits them. *)
let test_timeline_custom_state_filter () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let base = message {|{"msgtype":"m.text","body":"old"}|} in
  let reaction =
    raw
      {|{"event_id":"$reaction","sender":"@bob:example.org","origin_server_ts":1700000001000,"type":"m.reaction","content":{"m.relates_to":{"rel_type":"m.annotation","event_id":"$one","key":"👍"}}}|}
  in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.prepend cache the_room
    ~events:
      [
        state_event ~id:"$create" ~type_:"m.room.create" {|{}|};
        state_event ~id:"$power" ~type_:"m.room.power_levels" {|{}|};
        state_event ~id:"$space" ~type_:"m.space.child" {|{"via":[]}|};
        base;
        reaction;
      ]
    ~prev_batch:None;
  let event_filter event =
    match event.Ui.Presentation.content with
    | Ui.Presentation.State _ -> true
    | _ -> Ui.Room_timeline.default_event_filter event
  in
  let timeline =
    Ui.Room_timeline.create ~sw ~client:(offline_client ())
      ~send_queue:(queue_for alice) ~event_filter cache the_room
  in
  let events = timeline_events timeline in
  Alcotest.(check (list string))
    "state events are included while reactions stay folded"
    [ "$create"; "$power"; "$space"; "$one" ]
    (List.filter_map
       (fun (event : Ui.Room_timeline.event_item) ->
         Option.map Matrix_proto.Id.Event_id.to_string event.event.event_id)
       events);
  Alcotest.(check int)
    "reaction remains aggregated" 1
    (List.length (List.hd (List.rev events)).reactions)

let test_timeline_exclusion_filter () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.prepend cache the_room
    ~events:[ message {|{"msgtype":"m.text","body":"hidden"}|} ]
    ~prev_batch:None;
  let event_filter event =
    match event.Ui.Presentation.content with
    | Ui.Presentation.Message _ -> false
    | _ -> Ui.Room_timeline.default_event_filter event
  in
  let timeline =
    Ui.Room_timeline.create ~sw ~client:(offline_client ())
      ~send_queue:(queue_for alice) ~event_filter cache the_room
  in
  Alcotest.(check int)
    "excluded messages produce no event items" 0
    (List.length (timeline_events timeline))

let test_timeline_filter_keeps_unable_to_decrypt () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.prepend cache the_room
    ~events:
      [
        raw
          {|{"event_id":"$encrypted","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.encrypted","content":{"algorithm":"m.megolm.v1.aes-sha2"}}|};
      ]
    ~prev_batch:None;
  let timeline =
    Ui.Room_timeline.create ~sw ~client:(offline_client ())
      ~send_queue:(queue_for alice)
      ~event_filter:(fun _ -> false)
      cache the_room
  in
  match timeline_events timeline with
  | [ event ] ->
      Alcotest.(check bool)
        "ciphertext remains an unable-to-decrypt item" true
        (match event.event.Ui.Presentation.content with
        | Ui.Presentation.Unable_to_decrypt -> true
        | _ -> false)
  | events ->
      Alcotest.failf "expected one unable-to-decrypt event, got %d"
        (List.length events)

let test_local_echo () =
  Eio_main.run @@ fun _ ->
  let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org" in
  let user_id = Matrix_proto.Id.User_id.of_string_exn "@alice:example.org" in
  let random =
    Matrix_client.Random.of_source
      (Eio.Flow.string_source (String.make 128 'k'))
  in
  let queue = Matrix_client.Send_queue.create ~random ~user_id () in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.track_send_queue cache queue;
  let request =
    Matrix_client.Send_queue.send_text queue ~room_id ~body:"pending"
  in
  let events = Ui.Event_cache.snapshot cache room_id in
  Alcotest.(check int) "local echo inserted" 1 (Array.length events);
  Alcotest.(check bool)
    "queued state" true
    (match events.(0).delivery with
    | Ui.Event_cache.Queued -> true
    | _ -> false);
  Alcotest.(check bool)
    "cancel accepted" true
    (Matrix_client.Send_queue.cancel queue request = `Cancelled);
  Alcotest.(check int)
    "cancel removes echo" 0
    (Array.length (Ui.Event_cache.snapshot cache room_id))

let test_attachment_caption_echo_updates () =
  Eio_main.run @@ fun _ ->
  let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org" in
  let user_id = Matrix_proto.Id.User_id.of_string_exn "@alice:example.org" in
  let queue = queue_for user_id in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.track_send_queue cache queue;
  let base_content =
    Jsont.Json.object'
      [
        Jsont.Json.mem (Jsont.Json.name "msgtype") (Jsont.Json.string "m.image");
        Jsont.Json.mem (Jsont.Json.name "body")
          (Jsont.Json.string "old caption");
        Jsont.Json.mem (Jsont.Json.name "url")
          (Jsont.Json.string "mxc://hs.example/image");
      ]
  in
  let attachment =
    Matrix_client.Send_queue.send_attachment queue ~room_id ~base_content
      ~original:
        (Matrix_client.Send_queue.attachment_upload ~content_type:"image/png"
           ~data:"image bytes" ())
      ()
  in
  let body () =
    let events = Ui.Event_cache.snapshot cache room_id in
    Alcotest.(check int) "one attachment echo" 1 (Array.length events);
    Matrix_proto.Json.find_string "body" events.(0).event.content
  in
  Alcotest.(check (option string))
    "initial caption" (Some "old caption") (body ());
  (match
     Matrix_client.Send_queue.edit_attachment_caption queue attachment
       ~caption:(Some "new caption")
   with
  | Ok Matrix_client.Send_queue.Updated -> ()
  | Ok result ->
      Alcotest.failf "caption edit was not applied (%s)"
        (match result with
        | Matrix_client.Send_queue.Deferred -> "deferred"
        | Matrix_client.Send_queue.Already_sent -> "already sent"
        | Matrix_client.Send_queue.Updated -> "updated")
  | Error error ->
      Alcotest.failf "caption edit failed: %s"
        (Matrix_client.Error.to_string error));
  Alcotest.(check (option string))
    "updated caption" (Some "new caption") (body ());
  (match
     Matrix_client.Send_queue.edit_attachment_caption queue attachment
       ~caption:(Some "last caption")
   with
  | Ok Matrix_client.Send_queue.Updated -> ()
  | Ok _ -> Alcotest.fail "second caption edit was deferred or already sent"
  | Error error ->
      Alcotest.failf "second caption edit failed: %s"
        (Matrix_client.Error.to_string error));
  Alcotest.(check (option string))
    "last edit wins" (Some "last caption") (body ());
  let response =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        (Printf.sprintf
           {|{"next_batch":"caption-sync","rooms":{"join":{"!room:example.org":{"timeline":{"events":[{"event_id":"$attachment","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{"msgtype":"m.image","body":"server caption","url":"mxc://hs.example/image"},"unsigned":{"transaction_id":%S}}]}}}}}|}
           (Matrix_client.Send_queue.txn_id attachment))
    with
    | Ok response -> response
    | Error error -> Alcotest.failf "bad caption sync response: %s" error
  in
  let state = Matrix_client.Base_client.create ~user_id () in
  let _, changes = Matrix_client.Base_client.apply state response in
  List.iter (Ui.Event_cache.apply_room_change cache) changes.room_changes;
  Alcotest.(check bool)
    "server event is now synced" true
    (match (Ui.Event_cache.snapshot cache room_id).(0).event.event_id with
    | Some _ -> true
    | None -> false);
  Alcotest.(check bool)
    "server event delivery is synced" true
    ((Ui.Event_cache.snapshot cache room_id).(0).delivery
   = Ui.Event_cache.Synced);
  (match
     Matrix_client.Send_queue.edit_attachment_caption queue attachment
       ~caption:(Some "local must not replace server")
   with
  | Ok Matrix_client.Send_queue.Updated -> ()
  | Ok _ -> Alcotest.fail "synced attachment edit was not applied"
  | Error error ->
      Alcotest.failf "synced attachment edit failed: %s"
        (Matrix_client.Error.to_string error));
  Alcotest.(check (option string))
    "synced event content is immutable" (Some "server caption") (body ());

  (* A sync response can race the final send callback. The callback's event ID
     must not overwrite the server event already installed in the cache. *)
  let late_queue = queue_for alice in
  let late_cache = Ui.Event_cache.create () in
  Ui.Event_cache.track_send_queue late_cache late_queue;
  let late_request =
    Matrix_client.Send_queue.send_message late_queue ~room_id
      ~event_type:"m.room.message"
      ~content:
        (Jsont.Json.object'
           [
             Jsont.Json.mem
               (Jsont.Json.name "msgtype")
               (Jsont.Json.string "m.text");
             Jsont.Json.mem (Jsont.Json.name "body")
               (Jsont.Json.string "local late ack");
           ])
  in
  let late_txn = Matrix_client.Send_queue.txn_id late_request in
  let late_response =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        (Printf.sprintf
           {|{"next_batch":"late-sync","rooms":{"join":{"!room:example.org":{"timeline":{"events":[{"event_id":"$server-event","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{"msgtype":"m.text","body":"server wins"},"unsigned":{"transaction_id":%S}}]}}}}}|}
           late_txn)
    with
    | Ok response -> response
    | Error error -> Alcotest.failf "bad late-ack sync response: %s" error
  in
  let late_state = Matrix_client.Base_client.create ~user_id:alice () in
  let _, late_changes =
    Matrix_client.Base_client.apply late_state late_response
  in
  List.iter
    (Ui.Event_cache.apply_room_change late_cache)
    late_changes.room_changes;
  (match
     Matrix_client.Send_queue.send_one late_queue
       ~send:(fun _ _ -> Ok (event_id "$different-ack"))
       (offline_client ()) late_request
   with
  | Matrix_client.Send_queue.Sent_ok _ -> ()
  | _ -> Alcotest.fail "late-ack request did not send");
  let late_event = (Ui.Event_cache.snapshot late_cache room_id).(0) in
  Alcotest.(check (option string))
    "late ack preserves server event id" (Some "$server-event")
    (Option.map Matrix_proto.Id.Event_id.to_string late_event.event.event_id);
  Alcotest.(check (option string))
    "late ack preserves server content" (Some "server wins")
    (Matrix_proto.Json.find_string "body" late_event.event.content)

let test_attachment_caption_echo_migration () =
  Eio_main.run @@ fun _ ->
  let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org" in
  let queue = queue_for alice in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.track_send_queue cache queue;
  let attachment =
    Matrix_client.Send_queue.send_attachment queue ~room_id
      ~base_content:
        (Jsont.Json.object'
           [
             Jsont.Json.mem
               (Jsont.Json.name "msgtype")
               (Jsont.Json.string "m.image");
             Jsont.Json.mem (Jsont.Json.name "body")
               (Jsont.Json.string "before migration");
           ])
      ~original:
        (Matrix_client.Send_queue.attachment_upload ~content_type:"image/png"
           ~data:"image bytes" ())
      ()
  in
  let old_txn = Matrix_client.Send_queue.txn_id attachment in
  let upload =
    List.find
      (fun request ->
        match Matrix_client.Send_queue.kind request with
        | Matrix_client.Send_queue.Upload_request _ -> true
        | _ -> false)
      (Matrix_client.Send_queue.requests queue)
  in
  let client = offline_client () in
  let mxc =
    Result.get_ok (Matrix_client.Media.Mxc.of_string "mxc://hs.example/image")
  in
  ignore
    (Matrix_client.Send_queue.send_one queue
       ~upload:(fun ?on_progress:_ _ _ ->
         Ok (Matrix_client.Send_queue.Clear_upload { mxc }))
       client upload);
  let edit_result = ref None in
  let outcome =
    Matrix_client.Send_queue.send_one queue
      ~send:(fun _ _ ->
        edit_result :=
          Some
            (Matrix_client.Send_queue.edit_attachment_caption queue attachment
               ~caption:(Some "after migration"));
        Ok (event_id "$attachment"))
      client attachment
  in
  Alcotest.(check bool)
    "the parent send enters the retrying edit state" true
    (match outcome with
    | Matrix_client.Send_queue.Retry_in _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "the in-flight edit is deferred" true
    (match !edit_result with
    | Some (Ok Matrix_client.Send_queue.Deferred) -> true
    | _ -> false);
  let new_txn = Matrix_client.Send_queue.txn_id attachment in
  Alcotest.(check bool)
    "the replacement gets a new transaction id" true
    (not (String.equal old_txn new_txn));
  let events = Ui.Event_cache.snapshot cache room_id in
  Alcotest.(check int)
    "migration leaves exactly one local echo" 1 (Array.length events);
  Alcotest.(check string)
    "the echo follows the replacement transaction" ("txn:" ^ new_txn)
    events.(0).stable_id;
  Alcotest.(check (option string))
    "the replacement caption is visible" (Some "* after migration")
    (Matrix_proto.Json.find_string "body" events.(0).event.content)

let test_in_flight_redaction_replaces_echo () =
  Eio_main.run @@ fun _ ->
  let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org" in
  let queue = queue_for alice in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.track_send_queue cache queue;
  let request =
    Matrix_client.Send_queue.send_text queue ~room_id ~body:"cancel me"
  in
  let client = offline_client () in
  let outcome =
    Matrix_client.Send_queue.send_one queue
      ~send:(fun _ request ->
        Alcotest.(check bool)
          "cancel races the send" true
          (Matrix_client.Send_queue.cancel queue request = `In_flight);
        Ok (event_id "$landed:example.org"))
      client request
  in
  Alcotest.(check bool)
    "the landed send is converted to a redaction" true
    (match outcome with
    | Matrix_client.Send_queue.Retry_in _ -> true
    | _ -> false);
  Alcotest.(check int)
    "the redaction reuses the old echo rather than duplicating it" 1
    (Array.length (Ui.Event_cache.snapshot cache room_id));
  let event = (Ui.Event_cache.snapshot cache room_id).(0).event in
  Alcotest.(check bool)
    "the remaining echo is the compensating redaction" true
    (Event.Event_type.equal event.type_ Event.Event_type.Room_redaction)

let test_send_queue_forget_persists_deletion () =
  Eio_main.run @@ fun _ ->
  let store = Matrix_client.Store.memory () in
  let queue =
    Matrix_client.Send_queue.create
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 4096 'q')))
      ~user_id:alice ~store ()
  in
  let parent =
    Matrix_client.Send_queue.send_text queue ~room_id:the_room ~body:"parent"
  in
  ignore
    (Matrix_client.Send_queue.send_message queue ~room_id:the_room
       ~depends_on:[ Matrix_client.Send_queue.id parent ]
       ~event_type:"m.room.message"
       ~content:
         (Jsont.Json.object'
            [
              Jsont.Json.mem
                (Jsont.Json.name "msgtype")
                (Jsont.Json.string "m.text");
              Jsont.Json.mem (Jsont.Json.name "body")
                (Jsont.Json.string "child");
            ]));
  Matrix_client.Send_queue.forget_room queue the_room;
  Alcotest.(check bool)
    "forgotten graph is detached" true
    (Matrix_client.Send_queue.is_empty queue);
  let restored =
    Matrix_client.Send_queue.create
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 4096 'r')))
      ~user_id:alice ~store ()
  in
  Alcotest.(check bool)
    "forgotten graph is absent after restart" true
    (Matrix_client.Send_queue.is_empty restored)

let test_send_queue_forget_in_flight_event () =
  Eio_main.run @@ fun _ ->
  let store = Matrix_client.Store.memory () in
  let queue =
    Matrix_client.Send_queue.create
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 4096 'f')))
      ~user_id:alice ~store ()
  in
  let request =
    Matrix_client.Send_queue.send_text queue ~room_id:the_room ~body:"landed"
  in
  let restart_empty = ref false in
  let outcome =
    Matrix_client.Send_queue.send_one queue
      ~send:(fun _ _ ->
        Matrix_client.Send_queue.forget_room queue the_room;
        let restarted =
          Matrix_client.Send_queue.create
            ~random:
              (Matrix_client.Random.of_source
                 (Eio.Flow.string_source (String.make 4096 's')))
            ~user_id:alice ~store ()
        in
        restart_empty := Matrix_client.Send_queue.is_empty restarted;
        Ok (event_id "$forgotten"))
      (offline_client ()) request
  in
  Alcotest.(check bool)
    "detached event is not redacted" true
    (match outcome with
    | Matrix_client.Send_queue.Failed _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "detached event is terminally cancelled" true
    (Matrix_client.Send_queue.status request
    = Matrix_client.Send_queue.Cancelled);
  Alcotest.(check bool)
    "detached event is not queued" true
    (Matrix_client.Send_queue.is_empty queue);
  Alcotest.(check bool)
    "detached event is absent during callback restart" true !restart_empty;
  Alcotest.(check bool)
    "request remains an event" true
    (match Matrix_client.Send_queue.kind request with
    | Matrix_client.Send_queue.Event _ -> true
    | _ -> false)

let test_send_queue_forget_in_flight_upload () =
  Eio_main.run @@ fun _ ->
  let media_store = Matrix_client.Media_store.memory () in
  let queue =
    Matrix_client.Send_queue.create
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 4096 'u')))
      ~user_id:alice ~media_store ()
  in
  let request =
    Matrix_client.Send_queue.upload queue ~room_id:the_room ~role:`Original
      ~content_type:"text/plain" ~data:"upload bytes" ()
  in
  let key =
    Matrix_client.Media_store.
      {
        uri = local_uri ~txn_id:(Matrix_client.Send_queue.txn_id request);
        format = File;
      }
  in
  Alcotest.(check (option string))
    "upload is cached before sending" (Some "upload bytes")
    (Result.get_ok
       (Matrix_client.Media_store.get ~now:Ptime.epoch media_store key));
  let outcome =
    Matrix_client.Send_queue.send_one queue
      ~upload:(fun ?on_progress:_ _ _ ->
        Matrix_client.Send_queue.forget_room queue the_room;
        Ok
          (Matrix_client.Send_queue.Clear_upload
             {
               mxc =
                 Result.get_ok
                   (Matrix_client.Media.Mxc.of_string "mxc://hs.example/upload");
             }))
      (offline_client ()) request
  in
  Alcotest.(check bool)
    "detached upload is terminally cancelled" true
    (match outcome with
    | Matrix_client.Send_queue.Failed _ -> true
    | _ -> false);
  Alcotest.(check bool)
    "detached upload is not queued" true
    (Matrix_client.Send_queue.is_empty queue);
  Alcotest.(check (option string))
    "detached upload cache is removed" None
    (Result.get_ok
       (Matrix_client.Media_store.get ~now:Ptime.epoch media_store key))

let sync_response ?(limited = false) ~batch ~prev_batch events =
  let events =
    List.map
      (fun (id, body) ->
        Printf.sprintf
          {|{"event_id":"%s","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{"msgtype":"m.text","body":"%s"}}|}
          id body)
      events
    |> String.concat ","
  in
  let json =
    Printf.sprintf
      {|{"next_batch":"%s","rooms":{"join":{"!room:example.org":{"timeline":{"events":[%s],"limited":%b,"prev_batch":"%s"}}}}}|}
      batch events limited prev_batch
  in
  match Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont json with
  | Ok response -> response
  | Error message -> Alcotest.fail message

let sync_response_ephemeral_json ~batch rooms =
  let rooms =
    List.map
      (fun (room_id, events) ->
        Printf.sprintf
          "%S:{\"timeline\":{\"events\":[]},\"ephemeral\":{\"events\":[%s]}}"
          (Matrix_proto.Id.Room_id.to_string room_id)
          (String.concat "," events))
      rooms
    |> String.concat ","
  in
  Printf.sprintf {|{"next_batch":%S,"rooms":{"join":{%s}}}|} batch rooms

let typing_event content =
  Printf.sprintf {|{"type":"m.typing","content":%s}|} content

let bodies cache room_id =
  Ui.Event_cache.snapshot cache room_id
  |> Array.to_list
  |> List.map (fun (event : Ui.Event_cache.event) ->
      Option.value ~default:"?"
        (Ui.Presentation.preview (Ui.Presentation.of_event event.event)))

(* A limited sync whose window shares no synced event with the history the
   cache holds means the server skipped events in between. The history stays
   where it is, behind a gap chunk carrying the response's [prev_batch], and
   the window goes after it. *)
let test_limited_sync () =
  Eio_main.run @@ fun _ ->
  let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org" in
  let user_id = Matrix_proto.Id.User_id.of_string_exn "@alice:example.org" in
  let cache = Ui.Event_cache.create () in
  let state = Matrix_client.Base_client.create ~user_id () in
  let step state response =
    let state, changes = Matrix_client.Base_client.apply state response in
    List.iter (Ui.Event_cache.apply_room_change cache) changes.room_changes;
    state
  in
  let state =
    step state
      (sync_response ~batch:"s1" ~prev_batch:"p1" [ ("$a", "a"); ("$b", "b") ])
  in
  let state =
    step state (sync_response ~batch:"s2" ~prev_batch:"p2" [ ("$c", "c") ])
  in
  Alcotest.(check (list string))
    "contiguous syncs accumulate" [ "a"; "b"; "c" ] (bodies cache room_id);
  Alcotest.(check (option string))
    "the oldest token is kept while contiguous" (Some "p1")
    (Ui.Observable.Value.get (Ui.Event_cache.prev_batch cache room_id));
  let random =
    Matrix_client.Random.of_source
      (Eio.Flow.string_source (String.make 128 'k'))
  in
  let queue = Matrix_client.Send_queue.create ~random ~user_id () in
  Ui.Event_cache.track_send_queue cache queue;
  let _ = Matrix_client.Send_queue.send_text queue ~room_id ~body:"pending" in
  let _ =
    step state
      (sync_response ~limited:true ~batch:"s3" ~prev_batch:"p3"
         [ ("$x", "x"); ("$y", "y") ])
  in
  Alcotest.(check (list string))
    "a disjoint limited sync keeps the history and the echo stays last"
    [ "a"; "b"; "c"; "x"; "y"; "pending" ]
    (bodies cache room_id);
  Alcotest.(check (option string))
    "the room's oldest edge is still the first window's token" (Some "p1")
    (Ui.Observable.Value.get (Ui.Event_cache.prev_batch cache room_id));
  Alcotest.(check bool)
    "the hole is marked" true
    (Ui.Observable.Value.get (Ui.Event_cache.has_gap cache room_id));
  match Ui.Observable.Value.get (Ui.Event_cache.gaps cache room_id) with
  | [ gap ] ->
      Alcotest.(check int) "the gap sits after the old history" 3 gap.index;
      Alcotest.(check string)
        "and carries the limited response's token" "p3" gap.token
  | gaps -> Alcotest.failf "expected one gap, got %d" (List.length gaps)

(* Filling that gap splices the missing events into it, and closes it once a
   page reaches events the room already held on the older side. *)
let test_gap_fill () =
  Eio_main.run @@ fun _ ->
  let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org" in
  let user_id = Matrix_proto.Id.User_id.of_string_exn "@alice:example.org" in
  let cache = Ui.Event_cache.create () in
  let state = Matrix_client.Base_client.create ~user_id () in
  let step state response =
    let state, changes = Matrix_client.Base_client.apply state response in
    List.iter (Ui.Event_cache.apply_room_change cache) changes.room_changes;
    state
  in
  let state =
    step state
      (sync_response ~batch:"s1" ~prev_batch:"p1" [ ("$a", "a"); ("$b", "b") ])
  in
  let _ =
    step state
      (sync_response ~limited:true ~batch:"s2" ~prev_batch:"p2"
         [ ("$y", "y"); ("$z", "z") ])
  in
  let gap () =
    match Ui.Observable.Value.get (Ui.Event_cache.gaps cache room_id) with
    | [ gap ] -> Some gap
    | _ -> None
  in
  let id = (Option.get (gap ())).id in
  (* The first page brings back the middle of the hole, so the gap survives,
     retargeted at the response's end. *)
  Ui.Event_cache.resolve_gap cache room_id ~gap:id
    ~events:[ message ~id:"$w" {|{"msgtype":"m.text","body":"w"}|} ]
    ~prev_batch:(Some "p1b");
  Alcotest.(check (list string))
    "the page lands between the history and the window"
    [ "a"; "b"; "w"; "y"; "z" ]
    (bodies cache room_id);
  (match gap () with
  | Some gap -> Alcotest.(check string) "retargeted" "p1b" gap.token
  | None -> Alcotest.fail "the gap should have survived a partial page");
  (* The second page reaches an event held on the older side, which closes
     the hole; the duplicate is not stored twice. *)
  Ui.Event_cache.resolve_gap cache room_id ~gap:id
    ~events:
      [
        message ~id:"$b" {|{"msgtype":"m.text","body":"b"}|};
        message ~id:"$v" {|{"msgtype":"m.text","body":"v"}|};
      ]
    ~prev_batch:(Some "p0");
  Alcotest.(check (list string))
    "the hole is filled in the server's order without duplicates"
    [ "a"; "b"; "v"; "w"; "y"; "z" ]
    (bodies cache room_id);
  Alcotest.(check bool)
    "and the gap is gone" false
    (Ui.Observable.Value.get (Ui.Event_cache.has_gap cache room_id));
  Alcotest.(check (option string))
    "the room's oldest edge is untouched" (Some "p1")
    (Ui.Observable.Value.get (Ui.Event_cache.prev_batch cache room_id));
  Alcotest.(check bool)
    "a gap another pagination already closed is ignored" true
    (try
       Ui.Event_cache.resolve_gap cache room_id ~gap:id ~events:[]
         ~prev_batch:None;
       true
     with _ -> false)

(* A room-wide receipt recount is safe only when the cached receipt target has
   no physical hole after it. A hole before (or at) the target is harmless to
   the suffix being counted. *)
let test_receipt_reconcile_gap_safety () =
  Eio_main.run @@ fun _ ->
  let cache = Ui.Event_cache.create () in
  let state = ref (Matrix_client.Base_client.create ~user_id:alice ()) in
  let step response =
    let next, changes = Matrix_client.Base_client.apply !state response in
    state := next;
    List.iter (Ui.Event_cache.apply_room_change cache) changes.room_changes
  in
  step
    (sync_response ~batch:"s1" ~prev_batch:"p1"
       [ ("$target", "target"); ("$before", "before"); ("$after", "after") ]);
  step
    (sync_response ~limited:true ~batch:"s2" ~prev_batch:"p2"
       [ ("$later", "later") ]);
  let events, gaps = Ui.Event_cache.snapshot_with_gaps cache the_room in
  let target_position =
    Array.find_index
      (fun (event : Ui.Event_cache.event) ->
        event.event.event_id = Some (event_id "$target"))
      events
  in
  let target_position = Option.get target_position in
  Alcotest.(check bool)
    "a hole after a cached target is unsafe" true
    (List.exists
       (fun (gap : Ui.Event_cache.gap) -> gap.index > target_position)
       gaps);
  let trimmed =
    Ui.Event_cache.create ~max_events_per_room:2 ~chunk_capacity:1 ()
  in
  let state = ref (Matrix_client.Base_client.create ~user_id:alice ()) in
  let step response =
    let next, changes = Matrix_client.Base_client.apply !state response in
    state := next;
    List.iter (Ui.Event_cache.apply_room_change trimmed) changes.room_changes
  in
  step (sync_response ~batch:"t1" ~prev_batch:"p1" [ ("$old", "old") ]);
  step (sync_response ~batch:"t2" ~prev_batch:"p2" [ ("$target", "target") ]);
  step (sync_response ~batch:"t3" ~prev_batch:"p3" [ ("$new", "new") ]);
  let events, gaps = Ui.Event_cache.snapshot_with_gaps trimmed the_room in
  let target_position =
    Array.find_index
      (fun (event : Ui.Event_cache.event) ->
        event.event.event_id = Some (event_id "$target"))
      events
  in
  Alcotest.(check bool)
    "a hole at or before a cached target is safe" true
    (match target_position with
    | None -> false
    | Some position ->
        List.for_all
          (fun (gap : Ui.Event_cache.gap) -> gap.index <= position)
          gaps)

let test_room_reconcile_ignores_local_echo_and_thread_reply () =
  let local_echo =
    message ~sender:"@alice:example.org" {|{"msgtype":"m.text","body":"echo"}|}
  in
  let thread_reply =
    raw
      {|{"event_id":"$reply","sender":"@bob:example.org","origin_server_ts":1700000000001,"type":"m.room.message","content":{"msgtype":"m.text","body":"thread","m.relates_to":{"rel_type":"m.thread","event_id":"$root"}}}|}
  in
  let notification =
    Matrix_client.Push_evaluator.notification_for_event
      (Matrix_proto.Push.default_ruleset ~user_id:alice)
      (Matrix_client.Push_evaluator.Context.v ~user_id:alice ~room_id:the_room
         ~display_name:"Alice" ~member_count:2 ())
  in
  let counts =
    Matrix_client.Read_state.count_unread ~user_id:alice ~notification
      Matrix_client.Read_state.empty
      [ local_echo; thread_reply ]
  in
  Alcotest.(check int)
    "local echo and thread reply do not affect room unread" 0 counts.unread;
  Alcotest.(check int)
    "local echo and thread reply do not affect room notifications" 0
    counts.notifications

let test_event_cache_snapshot_validation_is_atomic () =
  Eio_main.run @@ fun _ ->
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.prepend cache the_room
    ~events:[ message ~id:"$atomic-a" {|{"msgtype":"m.text","body":"a"}|} ]
    ~prev_batch:None;
  let events, gaps = Ui.Event_cache.snapshot_with_gaps cache the_room in
  let called = ref 0 in
  let result =
    Ui.Event_cache.with_snapshot_if_current cache the_room events gaps
      (fun () ->
        incr called;
        17)
  in
  Alcotest.(check (option int))
    "current snapshot invokes callback" (Some 17) result;
  Alcotest.(check int) "callback invoked once" 1 !called;
  Ui.Event_cache.prepend cache the_room
    ~events:[ message ~id:"$atomic-b" {|{"msgtype":"m.text","body":"b"}|} ]
    ~prev_batch:None;
  let stale =
    Ui.Event_cache.with_snapshot_if_current cache the_room events gaps
      (fun () ->
        incr called;
        19)
  in
  Alcotest.(check (option int)) "stale snapshot skips callback" None stale;
  Alcotest.(check int) "stale callback is not interleaved" 1 !called

let test_physical_decryption_notifies_and_changes_room_count () =
  Eio_main.run @@ fun _ ->
  let cache = Ui.Event_cache.create () in
  let encrypted =
    raw
      {|{"event_id":"$late-thread","sender":"@bob:example.org","origin_server_ts":1700000000000,"type":"m.room.encrypted","content":{"algorithm":"m.megolm.v1.aes-sha2"}}|}
  in
  let plaintext =
    raw
      {|{"event_id":"$late-thread","sender":"@bob:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{"msgtype":"m.text","body":"thread reply","m.relates_to":{"rel_type":"m.thread","event_id":"$root"}}}|}
  in
  Ui.Event_cache.prepend cache the_room ~events:[ encrypted ] ~prev_batch:None;
  let notification _ =
    {
      Matrix_client.Push_evaluator.notify = true;
      highlight = false;
      sound = None;
    }
  in
  let count () =
    Ui.Event_cache.snapshot cache the_room
    |> Array.to_list
    |> List.map Ui.Event_cache.effective
    |> List.filter Matrix_client.Read_state.is_main_timeline_event
    |> Matrix_client.Read_state.count_unread ~user_id:alice ~notification
         Matrix_client.Read_state.empty
  in
  Alcotest.(check int)
    "undecrypted message is initially unread" 1 (count ()).unread;
  let notifications = ref 0 in
  let unsubscribe =
    Ui.Event_cache.subscribe_physical_decryption cache (fun room_id ->
        Alcotest.(check bool)
          "decryption notification names room" true (room_id = the_room);
        incr notifications)
  in
  Alcotest.(check bool)
    "physical decryption changes record" true
    (Ui.Event_cache.set_decrypted cache the_room ~encrypted ~plaintext);
  Alcotest.(check int) "thread reply leaves room count" 0 (count ()).unread;
  Alcotest.(check int) "physical change notifies once" 1 !notifications;
  Alcotest.(check bool)
    "same plaintext is a no-op" false
    (Ui.Event_cache.set_decrypted cache the_room ~encrypted ~plaintext);
  Alcotest.(check int) "no duplicate notification" 1 !notifications;
  unsubscribe ()

(* Over budget the oldest chunk goes, and what replaces it is a gap carrying
   the token that re-fetches exactly what was cut — not the token from
   before it, which would page past the hole and never close it. *)
let test_trim_leaves_a_gap () =
  Eio_main.run @@ fun _ ->
  let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org" in
  let user_id = Matrix_proto.Id.User_id.of_string_exn "@alice:example.org" in
  let cache =
    Ui.Event_cache.create ~max_events_per_room:2 ~chunk_capacity:1 ()
  in
  let state = ref (Matrix_client.Base_client.create ~user_id ()) in
  let step response =
    let next, changes = Matrix_client.Base_client.apply !state response in
    state := next;
    List.iter (Ui.Event_cache.apply_room_change cache) changes.room_changes
  in
  step (sync_response ~batch:"s1" ~prev_batch:"p1" [ ("$a", "a") ]);
  step (sync_response ~batch:"s2" ~prev_batch:"p2" [ ("$b", "b") ]);
  Alcotest.(check (list string))
    "under budget nothing is cut" [ "a"; "b" ] (bodies cache room_id);
  step (sync_response ~batch:"s3" ~prev_batch:"p3" [ ("$c", "c") ]);
  Alcotest.(check (list string))
    "the oldest chunk is cut" [ "b"; "c" ] (bodies cache room_id);
  Alcotest.(check (option string))
    "and the edge names the position it was cut at" (Some "p2")
    (Ui.Observable.Value.get (Ui.Event_cache.prev_batch cache room_id));
  match Ui.Observable.Value.get (Ui.Event_cache.gaps cache room_id) with
  | [ gap ] ->
      Alcotest.(check int) "the gap is the room's head" 0 gap.index;
      Alcotest.(check string) "carrying that token" "p2" gap.token;
      (* Paginating it re-fetches what was cut, and nothing that is held. *)
      Ui.Event_cache.resolve_gap cache room_id ~gap:gap.id
        ~events:[ message ~id:"$a" {|{"msgtype":"m.text","body":"a"}|} ]
        ~prev_batch:None;
      Alcotest.(check (list string))
        "which restores it exactly once" [ "a"; "b"; "c" ]
        (bodies cache room_id)
  | gaps -> Alcotest.failf "expected one gap, got %d" (List.length gaps)

(* A limited window that overlaps the cached history is contiguous with it:
   the server truncated the timeline, it did not skip anything the cache
   does not already hold. This is what the first sync of a process that
   reloaded its cache from disk looks like, and dropping there would throw
   the reloaded history away. *)
let test_limited_sync_overlap () =
  Eio_main.run @@ fun _ ->
  let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org" in
  let user_id = Matrix_proto.Id.User_id.of_string_exn "@alice:example.org" in
  let cache = Ui.Event_cache.create () in
  let state = Matrix_client.Base_client.create ~user_id () in
  let step state response =
    let state, changes = Matrix_client.Base_client.apply state response in
    List.iter (Ui.Event_cache.apply_room_change cache) changes.room_changes;
    state
  in
  let state =
    step state
      (sync_response ~batch:"s1" ~prev_batch:"p1"
         [ ("$a", "a"); ("$b", "b"); ("$c", "c") ])
  in
  let _ =
    step state
      (sync_response ~limited:true ~batch:"s2" ~prev_batch:"p2"
         [ ("$c", "c"); ("$d", "d") ])
  in
  Alcotest.(check (list string))
    "an overlapping limited window merges into the history"
    [ "a"; "b"; "c"; "d" ] (bodies cache room_id);
  Alcotest.(check (option string))
    "and leaves the older pagination token alone" (Some "p1")
    (Ui.Observable.Value.get (Ui.Event_cache.prev_batch cache room_id));
  Alcotest.(check bool)
    "so there is no gap" false
    (Ui.Observable.Value.get (Ui.Event_cache.has_gap cache room_id))

(* The head of the timeline says what lies before it, and says exactly one
   thing: a [Gap] while there is history to fetch, [Timeline_start] once
   there is not. Paginating to the room's beginning turns the one into the
   other. *)
let test_timeline_start () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org" in
  let user_id = Matrix_proto.Id.User_id.of_string_exn "@alice:example.org" in
  let cache = Ui.Event_cache.create () in
  let state = Matrix_client.Base_client.create ~user_id () in
  let _, changes =
    Matrix_client.Base_client.apply state
      (sync_response ~limited:true ~batch:"s1" ~prev_batch:"p1" [ ("$a", "a") ])
  in
  List.iter (Ui.Event_cache.apply_room_change cache) changes.room_changes;
  let timeline =
    Ui.Room_timeline.create ~sw ~client:(offline_client ())
      ~send_queue:(queue_for alice) cache room_id
  in
  let heads () =
    Ui.Room_timeline.snapshot timeline
    |> Array.to_list
    |> List.filter_map (function
      | Ui.Room_timeline.Virtual { content = Ui.Room_timeline.Gap _; _ } ->
          Some "gap"
      | Ui.Room_timeline.Virtual
          { content = Ui.Room_timeline.Timeline_start; _ } ->
          Some "start"
      | _ -> None)
  in
  Alcotest.(check (list string))
    "a limited sync leaves a gap and no start marker" [ "gap" ] (heads ());
  (* A page that comes back without a token is the room's beginning. The
     timeline re-projects in a fiber of its own, so let it run. *)
  Ui.Event_cache.prepend cache room_id ~events:[] ~prev_batch:None;
  for _ = 1 to 8 do
    Eio.Fiber.yield ()
  done;
  Alcotest.(check (list string))
    "reaching the beginning replaces it with the start marker" [ "start" ]
    (heads ())

module Model = Ui.Event_store.Internal

let events_chunk ?(chunk_id = 0) ?prev_token ?next_token events =
  Model.Events { chunk_id; prev_token; next_token; events }

let stored_event ?(delivery = Model.Synced) ?clear_event stable_id event =
  { Model.stable_id; event; clear_event; delivery }

let with_temp_store f =
  let path = Filename.temp_file "matrix-ui-" ".sqlite3" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun suffix ->
          let file = path ^ suffix in
          if Sys.file_exists file then Sys.remove file)
        [ ""; "-wal"; "-shm" ])
    (fun () -> f path)

let test_sqlite_roundtrip () =
  Eio_main.run @@ fun _ ->
  with_temp_store @@ fun path ->
  let store =
    match Matrix_ui_sqlite.create ~plaintext_policy:Store_plaintext path with
    | Ok store -> store
    | Error error -> Alcotest.fail (Ui.Event_store.Error.to_string error)
  in
  let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org" in
  let event = message {|{"msgtype":"m.text","body":"persisted"}|} in
  let older = message ~id:"$old" {|{"msgtype":"m.text","body":"older"}|} in
  let room : Model.room =
    {
      next_chunk_id = 3;
      chunks =
        [
          events_chunk ~chunk_id:0 ~prev_token:"first" ~next_token:"hole"
            [ stored_event "event:$old" older ];
          Model.Gap { gap_id = 1; token = "hole" };
          events_chunk ~chunk_id:2
            [ stored_event ~clear_event:event "event:$one" event ];
        ];
      external_events = [];
    }
  in
  (match Ui.Event_store.save_room store room_id room with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Ui.Event_store.Error.to_string error));
  Ui.Event_store.close store;
  let store = Result.get_ok (Matrix_ui_sqlite.create path) in
  let loaded = Result.get_ok (Ui.Event_store.load_room store room_id) in
  Ui.Event_store.close store;
  match loaded with
  | None -> Alcotest.fail "room was not persisted"
  | Some loaded ->
      Alcotest.(check int) "the chunk counter survives" 3 loaded.next_chunk_id;
      Alcotest.(check int) "three chunks" 3 (List.length loaded.chunks);
      Alcotest.(check (option string))
        "the oldest edge keeps its token" (Some "first")
        (Model.room_prev_batch loaded);
      Alcotest.(check bool)
        "and the hole is still a hole" true
        (Model.room_has_gap loaded);
      (match loaded.chunks with
      | [ Model.Events first; Model.Gap gap; _ ] ->
          Alcotest.(check (option string))
            "the right edge token too" (Some "hole") first.next_token;
          Alcotest.(check string) "the gap's token" "hole" gap.token
      | _ -> Alcotest.fail "the chunk shape did not survive");
      Alcotest.(check (list string))
        "the events come back in order"
        [ "event:$old"; "event:$one" ]
        (List.map
           (fun (event : Model.event) -> event.stable_id)
           (Model.room_events loaded));
      Alcotest.(check bool)
        "plaintext not exposed under new policy" true
        (List.for_all
           (fun (event : Model.event) -> Option.is_none event.clear_event)
           (Model.room_events loaded))

(* A store written by an older schema is dropped rather than migrated: the
   cache is derived data the next sync refills. *)
let test_sqlite_schema_bump () =
  Eio_main.run @@ fun _ ->
  with_temp_store @@ fun path ->
  let db = Sqlite3.db_open path in
  Sqlite3.Rc.check
    (Sqlite3.exec db
       "CREATE TABLE ui_rooms (room_id TEXT PRIMARY KEY, prev_batch TEXT, \
        has_gap INTEGER NOT NULL); INSERT INTO ui_rooms VALUES('!r:x','p',1)");
  ignore (Sqlite3.db_close db);
  let store =
    match Matrix_ui_sqlite.create path with
    | Ok store -> store
    | Error error ->
        Alcotest.failf "reopening a stale store: %s"
          (Ui.Event_store.Error.to_string error)
  in
  let room_id = Matrix_proto.Id.Room_id.of_string_exn "!r:x" in
  let loaded = Result.get_ok (Ui.Event_store.load_room store room_id) in
  Ui.Event_store.close store;
  Alcotest.(check bool)
    "the stale room is gone rather than half-read" true (Option.is_none loaded)

(* Every change the cache makes is written as a delta. A second cache over
   the same store has to see exactly what the first held. *)
let test_incremental_persistence () =
  Eio_main.run @@ fun _ ->
  let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org" in
  let user_id = Matrix_proto.Id.User_id.of_string_exn "@alice:example.org" in
  let store = Ui.Event_store.memory ~plaintext_policy:Store_plaintext () in
  let cache = Ui.Event_cache.create ~store () in
  let state = ref (Matrix_client.Base_client.create ~user_id ()) in
  let step response =
    let next, changes = Matrix_client.Base_client.apply !state response in
    state := next;
    List.iter (Ui.Event_cache.apply_room_change cache) changes.room_changes
  in
  step (sync_response ~batch:"s1" ~prev_batch:"p1" [ ("$a", "a") ]);
  step
    (sync_response ~limited:true ~batch:"s2" ~prev_batch:"p2"
       [ ("$c", "c"); ("$d", "d") ]);
  let random =
    Matrix_client.Random.of_source
      (Eio.Flow.string_source (String.make 128 'k'))
  in
  let queue = Matrix_client.Send_queue.create ~random ~user_id () in
  Ui.Event_cache.track_send_queue cache queue;
  let request =
    Matrix_client.Send_queue.send_text queue ~room_id ~body:"pending"
  in
  let reloaded () =
    let cold = Ui.Event_cache.create ~store () in
    (bodies cold room_id, cold)
  in
  let persisted_bodies () =
    match Result.get_ok (Ui.Event_store.load_room store room_id) with
    | None -> []
    | Some room ->
        Model.room_events room
        |> List.map (fun (event : Model.event) ->
            Option.value ~default:"?"
              (Ui.Presentation.preview (Ui.Presentation.of_event event.event)))
  in
  let bodies_now, cold = reloaded () in
  Alcotest.(check (list string))
    "the delta writes carry the whole persisted room"
    [ "a"; "c"; "d"; "pending" ]
    (persisted_bodies ());
  Alcotest.(check (list string))
    "a cold cache decodes only the resident tail" [ "c"; "d"; "pending" ]
    bodies_now;
  Alcotest.(check bool)
    "including the hole" true
    (Ui.Observable.Value.get (Ui.Event_cache.has_gap cold room_id));
  Alcotest.(check (option string))
    "and the resident edge uses the hole token" (Some "p2")
    (Ui.Observable.Value.get (Ui.Event_cache.prev_batch cold room_id));
  ignore (Matrix_client.Send_queue.cancel queue request);
  Alcotest.(check (list string))
    "a cancelled echo leaves the lazy resident tail" [ "c"; "d" ]
    (fst (reloaded ()));
  Alcotest.(check (list string))
    "a cancelled echo is deleted from the whole store" [ "a"; "c"; "d" ]
    (persisted_bodies ())

let test_event_cache_forget_room () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let store = Ui.Event_store.memory ~plaintext_policy:Store_plaintext () in
  let cache = Ui.Event_cache.create ~store () in
  Ui.Event_cache.prepend cache the_room
    ~events:[ message ~id:"$forget" {|{"body":"forget me"}|} ]
    ~prev_batch:(Some "before");
  let events_handle = Ui.Event_cache.events cache the_room in
  let events, events_subscription =
    Ui.Observable.List.subscribe ~sw events_handle
  in
  let token, token_subscription =
    Ui.Observable.Value.subscribe ~sw (Ui.Event_cache.prev_batch cache the_room)
  in
  Alcotest.(check int) "the room starts populated" 1 (Array.length events);
  let queue = queue_for alice in
  Ui.Event_cache.track_send_queue cache queue;
  let pending =
    Matrix_client.Send_queue.send_text queue ~room_id:the_room ~body:"queued"
  in
  Ui.Event_cache.forget_room cache the_room;
  Alcotest.(check int)
    "existing cache handle is emptied" 0
    (Array.length (Ui.Observable.List.snapshot events_handle));
  Alcotest.(check (option string))
    "token is cleared" None
    (Ui.Observable.Value.get (Ui.Event_cache.prev_batch cache the_room));
  Alcotest.(check bool)
    "gap projection is cleared" false
    (Ui.Observable.Value.get (Ui.Event_cache.has_gap cache the_room));
  let diffs = Option.get (Ui.Observable.List.next events_subscription) in
  Alcotest.(check int)
    "list publishes removal" 0
    (Array.length (Ui.Observable.List.apply_all events diffs));
  Alcotest.(check (option string))
    "token publishes None" None
    (Option.get (Ui.Observable.Value.next token_subscription));
  Alcotest.(check bool)
    "store room is removed" true
    (Option.is_none (Result.get_ok (Ui.Event_store.load_room store the_room)));
  Alcotest.(check int)
    "cancelled queue callback does not recreate room" 0
    (match Matrix_client.Send_queue.cancel queue pending with
    | `Cancelled -> Array.length (Ui.Event_cache.snapshot cache the_room)
    | `Already_sent | `In_flight -> -1);
  let replacement =
    Matrix_client.Send_queue.send_text queue ~room_id:the_room ~body:"new"
  in
  Alcotest.(check int)
    "a deliberate new pending send recreates the room" 1
    (Array.length (Ui.Event_cache.snapshot cache the_room));
  ignore (Matrix_client.Send_queue.cancel queue replacement)

let test_event_cache_forget_in_flight_request () =
  Eio_main.run @@ fun _env ->
  let queue = queue_for alice in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.track_send_queue cache queue;
  let old =
    Matrix_client.Send_queue.send_text queue ~room_id:the_room ~body:"old"
  in
  let replacement = ref None in
  let outcome =
    Matrix_client.Send_queue.send_one queue
      ~send:(fun _ _ ->
        Ui.Event_cache.forget_room cache the_room;
        replacement :=
          Some
            (Matrix_client.Send_queue.send_text queue ~room_id:the_room
               ~body:"new");
        Ok (event_id "$old"))
      (offline_client ()) old
  in
  Alcotest.(check bool)
    "old request completes" true
    (match outcome with
    | Matrix_client.Send_queue.Sent_ok _ -> true
    | _ -> false);
  Alcotest.(check (list string))
    "late completion cannot reappear after a new send" [ "new" ]
    (bodies cache the_room);
  Option.iter
    (fun request -> ignore (Matrix_client.Send_queue.cancel queue request))
    !replacement

(* The marker goes after the fully-read event, past the run of the own
   user's events that follows it, and is not shown at all when that lands at
   the end or when the event is not in the timeline. *)

let marker_position timeline =
  let items = Array.to_list (Ui.Room_timeline.snapshot timeline) in
  let rec find index = function
    | [] -> None
    | Ui.Room_timeline.Virtual { content = Ui.Room_timeline.Read_marker; _ }
      :: _ ->
        Some index
    | _ :: rest -> find (index + 1) rest
  in
  find 0 items

let test_read_marker () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org" in
  let own = Matrix_proto.Id.User_id.of_string_exn "@alice:example.org" in
  let cache = Ui.Event_cache.create () in
  let event id sender body =
    message ~id ~sender
      (Printf.sprintf {|{"msgtype":"m.text","body":"%s"}|} body)
  in
  Ui.Event_cache.prepend cache room_id
    ~events:
      [
        event "$one" "@bob:example.org" "one";
        event "$two" "@alice:example.org" "two";
        event "$three" "@bob:example.org" "three";
      ]
    ~prev_batch:None;
  let marker = ref None in
  let timeline =
    Ui.Room_timeline.create ~sw ~client:(offline_client ())
      ~send_queue:(queue_for alice) ~own_user:own
      ~read_marker:(fun () -> !marker)
      cache room_id
  in
  Alcotest.(check (option int))
    "no marker while nothing is read" None (marker_position timeline);
  (* The items are [Timeline_start; Date_divider; one; two; three]: $one is
     read and $two is ours, so the marker skips past it. *)
  marker := Some (Matrix_proto.Id.Event_id.of_string_exn "$one");
  Ui.Room_timeline.refresh timeline;
  Alcotest.(check (option int))
    "the marker skips the run of our own events" (Some 4)
    (marker_position timeline);
  marker := Some (Matrix_proto.Id.Event_id.of_string_exn "$three");
  Ui.Room_timeline.refresh timeline;
  Alcotest.(check (option int))
    "a marker at the very end is not shown" None (marker_position timeline);
  marker := Some (Matrix_proto.Id.Event_id.of_string_exn "$absent");
  Ui.Room_timeline.refresh timeline;
  Alcotest.(check (option int))
    "nor is one whose event the timeline does not hold" None
    (marker_position timeline)

(* Every gap the cache holds is an item at its own position, with an id that
   does not move while the gap lives. *)
let test_gap_items () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org" in
  let user_id = Matrix_proto.Id.User_id.of_string_exn "@alice:example.org" in
  let cache = Ui.Event_cache.create () in
  let state = ref (Matrix_client.Base_client.create ~user_id ()) in
  let step response =
    let next, changes = Matrix_client.Base_client.apply !state response in
    state := next;
    List.iter (Ui.Event_cache.apply_room_change cache) changes.room_changes
  in
  step (sync_response ~batch:"s1" ~prev_batch:"p1" [ ("$a", "a") ]);
  step
    (sync_response ~limited:true ~batch:"s2" ~prev_batch:"p2" [ ("$c", "c") ]);
  let timeline =
    Ui.Room_timeline.create ~sw ~client:(offline_client ())
      ~send_queue:(queue_for alice) cache room_id
  in
  let shape () =
    Ui.Room_timeline.snapshot timeline
    |> Array.to_list
    |> List.filter_map (function
      | Ui.Room_timeline.Event event ->
          Some (Option.value ~default:"?" (Ui.Presentation.preview event.event))
      | Ui.Room_timeline.Virtual { id; content = Ui.Room_timeline.Gap _ } ->
          Some id
      | Ui.Room_timeline.Virtual
          { content = Ui.Room_timeline.Timeline_start; _ } ->
          Some "start"
      | Ui.Room_timeline.Virtual
          { content = Ui.Room_timeline.Date_divider _; _ } ->
          None
      | Ui.Room_timeline.Virtual { content = Ui.Room_timeline.Read_marker; _ }
        ->
          None)
  in
  let gap_id =
    match Ui.Observable.Value.get (Ui.Event_cache.gaps cache room_id) with
    | [ gap ] -> gap.id
    | _ -> Alcotest.fail "expected one gap"
  in
  Alcotest.(check (list string))
    "the gap is an item between the two windows"
    [ "a"; "virtual:gap:" ^ Ui.Event_cache.Gap_id.to_string gap_id; "c" ]
    (shape ());
  Ui.Event_cache.resolve_gap cache room_id ~gap:gap_id ~events:[]
    ~prev_batch:None;
  for _ = 1 to 8 do
    Eio.Fiber.yield ()
  done;
  Alcotest.(check (list string))
    "and goes when the hole closes" [ "a"; "c" ] (shape ())

(* A server timeline of twenty events; the client is fed windows of it —
   contiguous, or limited over a hole — and then fills every gap and pages
   back until neither is left. Whatever the sequence, what it holds is the
   server's order with nothing missing and nothing twice. *)

let server_size = 20

let server_json index =
  Printf.sprintf
    {|{"event_id":"$e%02d","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{"msgtype":"m.text","body":"e%02d"}}|}
    index index

let server_event index = raw (server_json index)
let server_body index = Printf.sprintf "e%02d" index
let server_token index = Printf.sprintf "tok:%d" index
let token_index token = Scanf.sscanf token "tok:%d" Fun.id

(* A backwards page from [token]: the [limit] events before it, oldest
   first, and the token before those unless that is the room's beginning. *)
let server_page token limit =
  let stop = token_index token in
  let start = max 0 (stop - limit) in
  ( List.init (stop - start) (fun offset -> server_event (start + offset)),
    if start = 0 then None else Some (server_token start) )

let window_response ~limited ~from ~len ~batch =
  let events =
    List.init len (fun offset -> server_json (from + offset))
    |> String.concat ","
  in
  let json =
    Printf.sprintf
      {|{"next_batch":"%s","rooms":{"join":{"!room:example.org":{"timeline":{"events":[%s],"limited":%b,"prev_batch":"%s"}}}}}|}
      batch events limited (server_token from)
  in
  match Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont json with
  | Ok response -> response
  | Error message -> Alcotest.fail message

let splice_property =
  let open QCheck in
  Test.make ~name:"chunk splices reproduce the server's order" ~count:200
    (list_size (Gen.int_range 1 6) (pair (int_range 0 4) (int_range 1 5)))
    (fun steps ->
      Eio_main.run @@ fun _ ->
      let room_id = Matrix_proto.Id.Room_id.of_string_exn "!room:example.org" in
      let user_id =
        Matrix_proto.Id.User_id.of_string_exn "@alice:example.org"
      in
      let cache = Ui.Event_cache.create () in
      let state = ref (Matrix_client.Base_client.create ~user_id ()) in
      let step response =
        let next, changes = Matrix_client.Base_client.apply !state response in
        state := next;
        List.iter (Ui.Event_cache.apply_room_change cache) changes.room_changes
      in
      (* Each step skips [skip] events — a hole, when it is not zero — and
         delivers [len] of them. *)
      let frontier = ref 0 in
      let batch = ref 0 in
      List.iter
        (fun (skip, len) ->
          let from = min (server_size - 1) (!frontier + skip) in
          let len = min len (server_size - from) in
          if len > 0 then (
            incr batch;
            step
              (window_response
                 ~limited:(skip > 0 || !frontier = 0)
                 ~from ~len
                 ~batch:(Printf.sprintf "s%d" !batch));
            frontier := from + len))
        steps;
      let holds () =
        Ui.Event_cache.snapshot cache room_id
        |> Array.to_list
        |> List.map (fun (event : Ui.Event_cache.event) ->
            Option.value ~default:"?"
              (Ui.Presentation.preview (Ui.Presentation.of_event event.event)))
      in
      let rec ordered = function
        | left :: (right :: _ as rest) ->
            String.compare left right < 0 && ordered rest
        | _ -> true
      in
      ordered (holds ())
      &&
      (* Now fill everything: each gap, then the room's oldest edge, until
         neither is left. *)
      let rec settle rounds =
        if rounds > 0 then
          match Ui.Observable.Value.get (Ui.Event_cache.gaps cache room_id) with
          | gap :: _ ->
              let events, prev_batch = server_page gap.token 3 in
              Ui.Event_cache.resolve_gap cache room_id ~gap:gap.id ~events
                ~prev_batch;
              settle (rounds - 1)
          | [] -> (
              match
                Ui.Observable.Value.get
                  (Ui.Event_cache.prev_batch cache room_id)
              with
              | None -> ()
              | Some token ->
                  let events, prev_batch = server_page token 3 in
                  Ui.Event_cache.prepend cache room_id ~events ~prev_batch;
                  settle (rounds - 1))
      in
      settle 400;
      holds () = List.init !frontier server_body)

(* The models a bot drives: what it sends, how it finds what it sent, the
   room it wants to join, and how it asks what arrived while it was away.
   The live counterpart is [test/integration/scenario_ui.ml]. *)

let json_of text =
  match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json text with
  | Ok json -> json
  | Error message -> Alcotest.fail message

let text_of json =
  match Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json json with
  | Ok text -> text
  | Error message -> Alcotest.fail message

let has_substring ~needle haystack =
  let width = String.length needle and length = String.length haystack in
  let rec scan index =
    index + width <= length
    && (String.equal (String.sub haystack index width) needle
       || scan (index + 1))
  in
  scan 0

let test_timeline_receipts () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let calls = ref [] in
  let handler request =
    let body =
      match request.Fetch.Middleware.body with
      | Fetch.Empty -> None
      | Fetch.String body -> Some body
      | Fetch.Stream _ -> Some "<stream>"
    in
    calls :=
      ( Http.Method.to_string request.meth,
        Fetch.Middleware.Url.to_string request.url,
        body )
      :: !calls;
    Fetch_mock.respond "{}" request
  in
  let base = client_over handler in
  let client =
    Matrix_client.Client.with_session base
      {
        Matrix_client.Client.user_id = alice;
        access_token = "test-token";
        device_id = Matrix_proto.Id.Device_id.of_string_exn "ALICEDEV";
        refresh_token = None;
      }
  in
  let root =
    message ~id:"$root" ~sender:"@bob:example.org"
      {|{"msgtype":"m.text","body":"root"}|}
  in
  let reply_old =
    message ~id:"$reply-old" ~sender:"@bob:example.org"
      {|{"msgtype":"m.text","body":"old","m.relates_to":{"rel_type":"m.thread","event_id":"$root"}}|}
  in
  let reply =
    message ~id:"$reply" ~sender:"@bob:example.org"
      {|{"msgtype":"m.text","body":"reply","m.relates_to":{"rel_type":"m.thread","event_id":"$root"}}|}
  in
  let edit = edit ~id:"$edit" ~sender:"@bob:example.org" ~target:"$root" () in
  let final =
    message ~id:"$final" ~sender:"@bob:example.org"
      {|{"msgtype":"m.text","body":"final"}|}
  in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.prepend cache the_room
    ~events:[ root; reply_old; reply; final; edit ]
    ~prev_batch:None;
  let state = ref (Matrix_client.Read_state.v ()) in
  let timeline =
    Ui.Room_timeline.create ~sw ~client ~send_queue:(queue_for alice)
      ~own_user:alice
      ~read_state:(fun () -> !state)
      cache the_room
  in
  (* The edit is folded into its target by the default filter, but the server
     still counts it. Marking read therefore advances to the hidden relation,
     as Rust's receipt-capable raw-event position does. *)
  (match Ui.Room_timeline.mark_as_read timeline Ui.Room_timeline.Read with
  | Ok true -> ()
  | Ok false -> Alcotest.fail "mark_as_read was unexpectedly suppressed"
  | Error error ->
      Alcotest.failf "mark_as_read failed: %s"
        (Matrix_client.Error.to_string error));
  let receipt_url event_id =
    List.exists
      (fun (_, url, _) ->
        has_substring ~needle:("/receipt/m.read/" ^ event_id) url)
      !calls
  in
  Alcotest.(check bool)
    "hidden relation advances the receipt" true (receipt_url "$edit");
  (match
     Ui.Room_timeline.send_single_receipt timeline Ui.Room_timeline.Read_private
       ~event_id:(event_id "$final")
   with
  | Ok false -> ()
  | Ok true -> Alcotest.fail "a private receipt regressed behind public read"
  | Error error ->
      Alcotest.failf "private/public monotonic check failed: %s"
        (Matrix_client.Error.to_string error));
  Alcotest.(check bool)
    "private read cannot lag public read" false
    (List.exists
       (fun (_, url, _) -> has_substring ~needle:"/receipt/m.read.private/" url)
       !calls);
  (* A caller explicitly scopes a threaded receipt to its root. *)
  (match
     Ui.Room_timeline.send_single_receipt timeline ~thread_id:(event_id "$root")
       Ui.Room_timeline.Read ~event_id:(event_id "$reply")
   with
  | Ok true -> ()
  | Ok false -> Alcotest.fail "thread receipt was unexpectedly suppressed"
  | Error error ->
      Alcotest.failf "thread receipt failed: %s"
        (Matrix_client.Error.to_string error));
  let threaded_body =
    List.find_map
      (fun (_, url, body) ->
        if has_substring ~needle:"/receipt/m.read/$reply" url then body
        else None)
      !calls
  in
  Alcotest.(check bool)
    "thread root is encoded" true
    (match threaded_body with
    | Some body -> has_substring ~needle:{|"thread_id":"$root"|} body
    | None -> false);
  let call_count = List.length !calls in
  (match
     Ui.Room_timeline.send_single_receipt timeline ~thread_id:(event_id "$root")
       Ui.Room_timeline.Read ~event_id:(event_id "$reply-old")
   with
  | Ok false -> ()
  | Ok true -> Alcotest.fail "a regressive threaded receipt was sent"
  | Error error ->
      Alcotest.failf "regressive receipt check failed: %s"
        (Matrix_client.Error.to_string error));
  Alcotest.(check int)
    "regressive receipt is suppressed" call_count (List.length !calls);
  Alcotest.(check (option string))
    "latest threaded receipt" (Some "$reply")
    (Option.map Matrix_proto.Id.Event_id.to_string
       (Ui.Room_timeline.latest_user_read_receipt timeline
          ~thread_id:(event_id "$root") ()))

let test_timeline_receipt_own_event_targets () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let calls = ref [] in
  let handler request =
    let body =
      match request.Fetch.Middleware.body with
      | Fetch.Empty -> None
      | Fetch.String body -> Some body
      | Fetch.Stream _ -> Some "<stream>"
    in
    calls := (Fetch.Middleware.Url.to_string request.url, body) :: !calls;
    Fetch_mock.respond "{}" request
  in
  let client =
    Matrix_client.Client.with_session (client_over handler)
      {
        Matrix_client.Client.user_id = alice;
        access_token = "test-token";
        device_id = Matrix_proto.Id.Device_id.of_string_exn "ALICEDEV";
        refresh_token = None;
      }
  in
  let other =
    message ~id:"$other" ~sender:"@bob:example.org"
      {|{"msgtype":"m.text","body":"other"}|}
  in
  let own =
    message ~id:"$own" ~sender:"@alice:example.org"
      {|{"msgtype":"m.text","body":"own"}|}
  in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.prepend cache the_room ~events:[ other; own ] ~prev_batch:None;
  let timeline =
    Ui.Room_timeline.create ~sw ~client ~send_queue:(queue_for alice)
      ~own_user:alice cache the_room
  in
  (* A normal receipt aimed at our own event is redirected to the preceding
     event from another user. *)
  (match Ui.Room_timeline.mark_as_read timeline Ui.Room_timeline.Read with
  | Ok true -> ()
  | Ok false -> Alcotest.fail "mark_as_read did not send the other user's event"
  | Error error ->
      Alcotest.failf "mark_as_read failed: %s"
        (Matrix_client.Error.to_string error));
  Alcotest.(check bool)
    "own event is redirected" true
    (List.exists
       (fun (url, _) -> has_substring ~needle:"/receipt/m.read/$other" url)
       !calls);
  let calls_before = List.length !calls in
  (match
     Ui.Room_timeline.send_single_receipt timeline Ui.Room_timeline.Read
       ~event_id:(event_id "$own")
   with
  | Ok false -> ()
  | Ok true -> Alcotest.fail "duplicate redirected receipt was sent"
  | Error error ->
      Alcotest.failf "duplicate redirected receipt failed: %s"
        (Matrix_client.Error.to_string error));
  Alcotest.(check int)
    "redirected duplicate is suppressed" (calls_before + 1) (List.length !calls);
  (* With only own events, an ordinary send is a no-op, but mark_as_read uses
     the special override and sends the own event so the server recomputes
     badges. *)
  let own_only_cache = Ui.Event_cache.create () in
  Ui.Event_cache.prepend own_only_cache the_room ~events:[ own ]
    ~prev_batch:None;
  let own_only =
    Ui.Room_timeline.create ~sw ~client ~send_queue:(queue_for alice)
      ~own_user:alice own_only_cache the_room
  in
  (match
     Ui.Room_timeline.send_single_receipt own_only Ui.Room_timeline.Read
       ~event_id:(event_id "$own")
   with
  | Ok false -> ()
  | Ok true -> Alcotest.fail "own-only ordinary receipt was sent"
  | Error error ->
      Alcotest.failf "own-only ordinary receipt failed: %s"
        (Matrix_client.Error.to_string error));
  (match Ui.Room_timeline.mark_as_read own_only Ui.Room_timeline.Read with
  | Ok true -> ()
  | Ok false -> Alcotest.fail "own-only mark_as_read was suppressed"
  | Error error ->
      Alcotest.failf "own-only mark_as_read failed: %s"
        (Matrix_client.Error.to_string error));
  Alcotest.(check bool)
    "own-only mark uses own event" true
    (List.exists
       (fun (url, _) -> has_substring ~needle:"/receipt/m.read/$own" url)
       !calls);
  (* Fully-read and public-read positions share the atomic read-marker
     endpoint; the private receipt remains a separate request. *)
  let combined_cache = Ui.Event_cache.create () in
  Ui.Event_cache.prepend combined_cache the_room ~events:[ other ]
    ~prev_batch:None;
  let combined =
    Ui.Room_timeline.create ~sw ~client ~send_queue:(queue_for alice)
      combined_cache the_room
  in
  let before_combined = List.length !calls in
  (match
     Ui.Room_timeline.send_multiple_receipts combined
       [
         {
           Ui.Room_timeline.receipt_type = Ui.Room_timeline.Fully_read;
           event_id = event_id "$other";
         };
         { receipt_type = Ui.Room_timeline.Read; event_id = event_id "$other" };
         {
           receipt_type = Ui.Room_timeline.Read_private;
           event_id = event_id "$other";
         };
       ]
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "combined receipts failed: %s"
        (Matrix_client.Error.to_string error));
  let rec drop count values =
    if count = 0 then values
    else match values with [] -> [] | _ :: rest -> drop (count - 1) rest
  in
  let combined_calls = drop before_combined (List.rev !calls) in
  Alcotest.(check bool)
    "combined marker contains fully read and public read" true
    (List.exists
       (fun (url, body) ->
         has_substring ~needle:"/read_markers" url
         &&
         match body with
         | Some body ->
             has_substring ~needle:"m.fully_read" body
             && has_substring ~needle:"m.read" body
             && has_substring ~needle:"m.read.private" body
         | None -> false)
       combined_calls)

(* Everything [send_message] can put in an [m.room.message], read back from
   the request the queue holds — through the wire text, so that what is
   checked is the encoding and not only the record that produced it. The
   body carries the quote, the backslash and the markup that a content
   assembled by splicing strings together would let out. *)
let test_send_message () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let queue = queue_for alice in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.track_send_queue cache queue;
  let timeline =
    Ui.Room_timeline.create ~sw ~client:(offline_client ()) ~send_queue:queue
      cache the_room
  in
  let target = event_id "$target" in
  let body = {|he said "hi", \ then <b>left</b> & went|} in
  let request =
    Ui.Room_timeline.send_message timeline ~msgtype:`Notice
      ~formatted:{|<b>bold</b><script>alert("x")</script><i>italic</i>|}
      ~reply_to:target ~body ()
  in
  let event_type, content =
    match Matrix_client.Send_queue.payload request with
    | Matrix_client.Send_queue.Send { event_type; content } ->
        (event_type, content)
    | Matrix_client.Send_queue.Redact _
    | Matrix_client.Send_queue.Upload_payload _ ->
        Alcotest.fail "a message request should not be a redaction"
  in
  Alcotest.(check string) "an m.room.message" "m.room.message" event_type;
  let echoed =
    {
      (message {|{"msgtype":"m.text","body":"placeholder"}|}) with
      Matrix_proto.Event.Raw_event.content = json_of (text_of content);
    }
  in
  let presented = Ui.Presentation.of_event echoed in
  (match presented.Ui.Presentation.content with
  | Ui.Presentation.Message message -> (
      Alcotest.(check bool)
        "an m.notice, which is what stops two bots answering each other" true
        (message.kind = Ui.Presentation.Notice);
      Alcotest.(check string)
        "the body survives the round trip" body message.body;
      match message.formatted with
      | None -> Alcotest.fail "expected a formatted body"
      | Some formatted ->
          Alcotest.(check bool)
            "the safe tags are kept" true
            (has_substring ~needle:"<b>bold</b>" formatted.html
            && has_substring ~needle:"<i>italic</i>" formatted.html);
          Alcotest.(check bool)
            "and the script is gone" false
            (has_substring ~needle:"script" formatted.html
            || has_substring ~needle:"alert" formatted.html))
  | _ -> Alcotest.fail "expected a message");
  (match presented.Ui.Presentation.relation with
  | Some { target = related; kind = Ui.Presentation.Reply } ->
      Alcotest.(check string)
        "the reply names the event it answers"
        (Matrix_proto.Id.Event_id.to_string target)
        (Matrix_proto.Id.Event_id.to_string related)
  | _ -> Alcotest.fail "expected a reply relation");
  Ui.Room_timeline.refresh timeline;
  match Ui.Room_timeline.item_of_request timeline request with
  | None -> Alcotest.fail "the send produced no item"
  | Some item ->
      Alcotest.(check (option string))
        "and the timeline reads the relation back"
        (Some (Matrix_proto.Id.Event_id.to_string target))
        (Option.map Matrix_proto.Id.Event_id.to_string item.reply_to)

let test_send_location () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let queue = queue_for alice in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.track_send_queue cache queue;
  let timeline =
    Ui.Room_timeline.create ~sw ~client:(offline_client ()) ~send_queue:queue
      cache the_room
  in
  let target = event_id "$where" in
  let request =
    Ui.Room_timeline.send_location timeline ~description:"Elizabeth Tower"
      ~zoom_level:10 ~asset:Ui.Room_timeline.Pin ~reply_to:target
      ~geo_uri:"geo:51.5008,-0.1247" ~body:"Big Ben" ()
  in
  let content =
    match Matrix_client.Send_queue.payload request with
    | Matrix_client.Send_queue.Send { event_type; content } ->
        Alcotest.(check string) "event type" "m.room.message" event_type;
        content
    | Matrix_client.Send_queue.Redact _
    | Matrix_client.Send_queue.Upload_payload _ ->
        Alcotest.fail "a location request should not be a redaction"
  in
  let module Json = Matrix_proto.Json in
  Alcotest.(check (option string))
    "message type" (Some "m.location")
    (Json.find_string "msgtype" content);
  Alcotest.(check (option string))
    "fallback body" (Some "Big Ben")
    (Json.find_string "body" content);
  Alcotest.(check (option string))
    "legacy geo URI" (Some "geo:51.5008,-0.1247")
    (Json.find_string "geo_uri" content);
  let location =
    match Json.find_mem "org.matrix.msc3488.location" content with
    | Some location -> location
    | None -> Alcotest.fail "missing extensible location"
  in
  Alcotest.(check (option string))
    "extensible geo URI" (Some "geo:51.5008,-0.1247")
    (Json.find_string "uri" location);
  Alcotest.(check (option string))
    "description" (Some "Elizabeth Tower")
    (Json.find_string "description" location);
  Alcotest.(check (option int))
    "zoom level" (Some 10)
    (Json.find_int "zoom_level" location);
  let asset =
    match Json.find_mem "org.matrix.msc3488.asset" content with
    | Some asset -> asset
    | None -> Alcotest.fail "missing location asset"
  in
  Alcotest.(check (option string))
    "asset type" (Some "m.pin")
    (Json.find_string "type" asset);
  let relation =
    Option.bind
      (Json.find_mem "m.relates_to" content)
      (Json.find_mem "m.in_reply_to")
  in
  Alcotest.(check (option string))
    "reply target" (Some "$where")
    (Option.bind relation (Json.find_string "event_id"));
  let before = Matrix_client.Send_queue.pending_count queue in
  Alcotest.check_raises "invalid zoom is rejected"
    (Invalid_argument "Matrix_ui.Room_timeline.send_location: zoom_level")
    (fun () ->
      ignore
        (Ui.Room_timeline.send_location timeline ~zoom_level:21
           ~geo_uri:"geo:0,0" ~body:"invalid" ()));
  Alcotest.(check int)
    "invalid send was not queued" before
    (Matrix_client.Send_queue.pending_count queue)

let test_send_edit () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let queue = queue_for alice in
  let cache = Ui.Event_cache.create () in
  let target = event_id "$target" in
  Ui.Event_cache.prepend cache the_room
    ~events:[ message ~id:"$target" {|{"msgtype":"m.text","body":"old"}|} ]
    ~prev_batch:None;
  Ui.Event_cache.track_send_queue cache queue;
  let timeline =
    Ui.Room_timeline.create ~sw ~client:(offline_client ()) ~send_queue:queue
      cache the_room
  in
  let request =
    Ui.Room_timeline.send_edit timeline ~event_id:target
      ~formatted:{|<b>fixed</b><script>bad()</script>|} ~body:"fixed" ()
  in
  let event_type, content =
    match Matrix_client.Send_queue.payload request with
    | Matrix_client.Send_queue.Send { event_type; content } ->
        (event_type, content)
    | Matrix_client.Send_queue.Redact _
    | Matrix_client.Send_queue.Upload_payload _ ->
        Alcotest.fail "an edit request should not be a redaction"
  in
  Alcotest.(check string)
    "an edit is a room message" "m.room.message" event_type;
  let echoed =
    {
      (message {|{"msgtype":"m.text","body":"placeholder"}|}) with
      Matrix_proto.Event.Raw_event.content = json_of (text_of content);
    }
  in
  (match Ui.Presentation.of_event echoed with
  | { Ui.Presentation.content = Ui.Presentation.Message message; _ } ->
      Alcotest.(check string) "the edit fallback body" "* fixed" message.body;
      Alcotest.(check bool)
        "unsafe formatted markup is removed" true
        (match message.formatted with
        | Some formatted ->
            has_substring ~needle:"<b>fixed</b>" formatted.html
            && not (has_substring ~needle:"script" formatted.html)
        | None -> false)
  | _ -> Alcotest.fail "expected an edit message");
  Ui.Room_timeline.refresh timeline;
  match
    Array.to_list (Ui.Room_timeline.snapshot timeline)
    |> List.filter_map (function
      | Ui.Room_timeline.Event event -> Some event
      | Ui.Room_timeline.Virtual _ -> None)
  with
  | [ item ] ->
      Alcotest.(check string)
        "the target is optimistically edited" "fixed"
        (Option.value (Ui.Presentation.preview item.event) ~default:"");
      Alcotest.(check bool) "the target is marked edited" true item.edited
  | items ->
      Alcotest.failf "expected one edited item, got %d" (List.length items)

let formatted_html event =
  match event.Ui.Presentation.content with
  | Ui.Presentation.Message { formatted = Some formatted; _ } ->
      Some formatted.html
  | _ -> None

let test_timeline_resolves_media_consistently () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let media = "mxc://hs.example/image" in
  let resolve_mxc value =
    if String.equal value media then Some "https://cdn.example/image" else None
  in
  let base =
    message ~id:"$one"
      {|{"msgtype":"m.text","body":"old","format":"org.matrix.custom.html","formatted_body":"<img src=\"mxc://hs.example/image\">"}|}
  in
  let edit =
    message ~id:"$edit" ~ts:1_700_000_001_000L
      {|{"msgtype":"m.text","body":"* new","format":"org.matrix.custom.html","formatted_body":"<img src=\"mxc://hs.example/image\">","m.new_content":{"msgtype":"m.text","body":"new","format":"org.matrix.custom.html","formatted_body":"<img src=\"mxc://hs.example/image\">"},"m.relates_to":{"rel_type":"m.replace","event_id":"$one"}}|}
  in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.prepend cache the_room ~events:[ base; edit ] ~prev_batch:None;
  let timeline =
    Ui.Room_timeline.create ~sw ~client:(offline_client ())
      ~send_queue:(queue_for alice) ~resolve_mxc cache the_room
  in
  let item_for id =
    timeline_events timeline
    |> List.find_opt (fun (item : Ui.Room_timeline.event_item) ->
        Option.map Matrix_proto.Id.Event_id.to_string item.event.event_id
        = Some id)
  in
  let check_resolved label item =
    match Option.bind item formatted_html with
    | Some html ->
        check_contains label "https://cdn.example/image" html;
        Alcotest.(check bool)
          (label ^ " has no unresolved MXC")
          false
          (Ui.Matching.contains ~haystack:html ~needle:media)
    | None -> Alcotest.failf "%s has no formatted presentation" label
  in
  check_resolved "initial edited presentation"
    (Option.map
       (fun (item : Ui.Room_timeline.event_item) -> item.event)
       (item_for "$one"));
  (match
     Ui.Room_timeline.edit_revisions timeline ~event_id:(event_id "$one")
   with
  | _original :: revision :: _ -> check_resolved "edit revision" (Some revision)
  | _ -> Alcotest.fail "expected an original and an edit revision");
  let fresh =
    message ~id:"$two" ~ts:1_700_000_002_000L
      {|{"msgtype":"m.text","body":"fresh","format":"org.matrix.custom.html","formatted_body":"<img src=\"mxc://hs.example/image\">"}|}
  in
  Ui.Event_cache.prepend cache the_room ~events:[ fresh ] ~prev_batch:None;
  for _ = 1 to 8 do
    Eio.Fiber.yield ()
  done;
  check_resolved "refreshed presentation"
    (Option.map
       (fun (item : Ui.Room_timeline.event_item) -> item.event)
       (item_for "$two"))

(* A caller that holds a request asks the timeline what became of it,
   rather than reconstructing the id the event cache filed it under. *)
let test_request_lookup () =
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let queue = queue_for alice in
  let cache = Ui.Event_cache.create () in
  Ui.Event_cache.track_send_queue cache queue;
  let timeline =
    Ui.Room_timeline.create ~sw ~client:(offline_client ()) ~send_queue:queue
      cache the_room
  in
  let request = Ui.Room_timeline.send_text timeline ~body:"queued" in
  let elsewhere =
    Matrix_client.Send_queue.send_text queue
      ~room_id:(Matrix_proto.Id.Room_id.of_string_exn "!other:example.org")
      ~body:"elsewhere"
  in
  Ui.Room_timeline.refresh timeline;
  Alcotest.(check bool)
    "the send is queued" true
    (Ui.Room_timeline.delivery timeline request = Some Ui.Event_cache.Queued);
  Alcotest.(check (option string))
    "and its item carries the body" (Some "queued")
    (Option.bind (Ui.Room_timeline.item_of_request timeline request)
       (fun item -> Ui.Presentation.preview item.Ui.Room_timeline.event));
  Alcotest.(check bool)
    "a request for another room is not this timeline's" true
    (Ui.Room_timeline.delivery timeline elsewhere = None);
  Alcotest.(check bool)
    "cancelling was accepted" true
    (Matrix_client.Send_queue.cancel queue request = `Cancelled);
  Ui.Room_timeline.refresh timeline;
  Alcotest.(check bool)
    "and the cancelled send has no item left" true
    (Ui.Room_timeline.item_of_request timeline request = None)

let fold_sync cache responses =
  List.fold_left
    (fun state response ->
      let state, changes = Matrix_client.Base_client.apply state response in
      List.iter (Ui.Event_cache.apply_room_change cache) changes.room_changes;
      state)
    (Matrix_client.Base_client.create ~user_id:alice ())
    responses

(* The lookup is over every room the state knows, not over the filtered
   list: a caller holding a room id is asking about that room. *)
let test_room_list_find () =
  Eio_main.run @@ fun _ ->
  let cache = Ui.Event_cache.create () in
  let state =
    fold_sync cache
      [ sync_response ~batch:"s1" ~prev_batch:"p1" [ ("$a", "a") ] ]
  in
  let list = Ui.Room_list.create cache state in
  Alcotest.(check (option string))
    "the room is found by id"
    (Some (Matrix_proto.Id.Room_id.to_string the_room))
    (Option.map
       (fun (room : Ui.Room_list.room) ->
         Matrix_proto.Id.Room_id.to_string room.id)
       (Ui.Room_list.find list the_room));
  Ui.Room_list.set_filter list Ui.Room_list.Filter.Nothing;
  Alcotest.(check int)
    "a filter that admits nothing empties the published list" 0
    (Array.length (Ui.Observable.List.snapshot (Ui.Room_list.rooms list)));
  Alcotest.(check bool)
    "and the lookup still answers" true
    (Ui.Room_list.find list the_room <> None);
  Alcotest.(check bool)
    "a room the state does not know" true
    (Ui.Room_list.find list
       (Matrix_proto.Id.Room_id.of_string_exn "!absent:example.org")
    = None)

(* "What arrived while I was away": the position of the event last handled,
   and the cached events after it. *)
let test_position () =
  Eio_main.run @@ fun _ ->
  let cache = Ui.Event_cache.create () in
  let _ =
    fold_sync cache
      [
        sync_response ~batch:"s1" ~prev_batch:"p1" [ ("$a", "a"); ("$b", "b") ];
        sync_response ~batch:"s2" ~prev_batch:"p2" [ ("$c", "c") ];
      ]
  in
  Alcotest.(check (option int))
    "the oldest event is first" (Some 0)
    (Ui.Event_cache.position cache the_room (event_id "$a"));
  Alcotest.(check (option int))
    "and the newest last" (Some 2)
    (Ui.Event_cache.position cache the_room (event_id "$c"));
  Alcotest.(check (option int))
    "an event the cache does not hold" None
    (Ui.Event_cache.position cache the_room (event_id "$never"));
  let after id =
    match Ui.Event_cache.position cache the_room (event_id id) with
    | None -> []
    | Some index ->
        let events = Ui.Event_cache.snapshot cache the_room in
        Array.sub events (index + 1) (Array.length events - index - 1)
        |> Array.to_list
        |> List.map (fun (event : Ui.Event_cache.event) ->
            Option.value ~default:"?"
              (Ui.Presentation.preview (Ui.Presentation.of_event event.event)))
  in
  Alcotest.(check (list string))
    "everything after the event last handled" [ "c" ] (after "$b");
  Alcotest.(check (list string)) "and nothing after the newest" [] (after "$c")

let mock_client ~sw ~env handler =
  Matrix_eio.Client.create ~sw ~env
    ~homeserver:(Uriz.of_string_exn "https://hs.example")
    ~fetch:(Fetch_mock.client handler)
    ()

let runtime_client ~sw ~env handler =
  let client = mock_client ~sw ~env handler in
  Matrix_eio.Client.with_session client
    {
      Matrix_client.Client.user_id = alice;
      device_id = Matrix_proto.Id.Device_id.of_string_exn "ALICEDEV";
      access_token = "test-token";
      refresh_token = None;
    }

let test_runtime_recovery_manager_seed_and_release () =
  Eio_main.run @@ fun env ->
  let observable = ref None in
  let manager = ref None in
  let () =
    Eio.Switch.run @@ fun sw ->
    let client =
      runtime_client ~sw ~env (fun request -> Fetch_mock.respond "{}" request)
    in
    let sync = Matrix_eio.Sync_service.of_user ~user_id:alice () in
    let encryption =
      Matrix_eio.Encryption.create
        ~random:
          (Matrix_client.Random.of_source
             (Eio.Flow.string_source (String.make 400_000 'r')))
        ~user_id:alice
        ~device_id:(Matrix_proto.Id.Device_id.of_string_exn "ALICEDEV")
        ()
    in
    let recovery_manager =
      Recovery.Manager.create client ~encryption
        ~base:(Matrix_eio.Sync_service.state sync)
    in
    manager := Some recovery_manager;
    let runtime =
      Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client ~sync
        ~encryption ~recovery_manager ~send_queue:(queue_for alice) ()
    in
    observable := Ui.Runtime.recovery_state runtime;
    (match !observable with
    | Some state ->
        Alcotest.(check bool)
          "observable is seeded immediately" true
          (Ui.Observable.Value.get state = Recovery.Unknown)
    | None -> Alcotest.fail "recovery observable was not created");
    let present_store = Matrix_client.Store.memory () in
    Matrix_client.Store.set_account_data present_store
      Matrix_client.Secrets.default_key_event_type
      (Jsont.Json.object'
         [ Jsont.Json.mem (Jsont.Json.name "key") (Jsont.Json.string "key-id") ]);
    let present =
      Matrix_client.Base_client.of_store present_store ~user_id:alice ()
    in
    ignore (Recovery.Manager.refresh_from_base recovery_manager present);
    (match !observable with
    | Some state ->
        Alcotest.(check bool)
          "base projection reaches UI" true
          (Ui.Observable.Value.get state = Recovery.Incomplete)
    | None -> Alcotest.fail "recovery observable disappeared");
    ()
  in
  match (!manager, !observable) with
  | Some recovery_manager, Some state ->
      ignore
        (Recovery.Manager.refresh_from_base recovery_manager
           (Matrix_client.Base_client.create ~user_id:alice ()));
      Alcotest.(check bool)
        "release unsubscribes runtime" true
        (Ui.Observable.Value.get state = Recovery.Incomplete)
  | _ -> Alcotest.fail "recovery manager or observable disappeared"

let test_runtime_typing_users () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let other_room = Matrix_proto.Id.Room_id.of_string_exn "!other:example.org" in
  let response_strings =
    [
      sync_response_ephemeral_json ~batch:"typing-a-1"
        [
          ( the_room,
            [
              typing_event
                {|{"user_ids":["@bob:example.org","@alice:example.org","@carol:example.org"]}|};
            ] );
        ];
      sync_response_ephemeral_json ~batch:"typing-b"
        [
          ( other_room,
            [
              typing_event
                {|{"user_ids":["@carol:example.org","@alice:example.org"]}|};
            ] );
        ];
      sync_response_ephemeral_json ~batch:"typing-a-2"
        [
          ( the_room,
            [
              typing_event
                {|{"user_ids":["@carol:example.org","@alice:example.org","@bob:example.org"]}|};
            ] );
        ];
      sync_response_ephemeral_json ~batch:"receipt-only"
        [ (the_room, [ {|{"type":"m.receipt","content":{}}|} ]) ];
      sync_response_ephemeral_json ~batch:"malformed"
        [ (the_room, [ typing_event {|{"user_ids":"not-a-list"}|} ]) ];
      sync_response_ephemeral_json ~batch:"typing-clear"
        [ (the_room, [ typing_event {|{"user_ids":[]}|} ]) ];
      sync_response_ephemeral_json ~batch:"typing-before-forget"
        [ (the_room, [ typing_event {|{"user_ids":["@carol:example.org"]}|} ]) ];
    ]
  in
  let responses = ref response_strings in
  let stopped, stopped_resolver = Eio.Promise.create () in
  let client =
    runtime_client ~sw ~env (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        let method_ = Http.Method.to_string request.meth in
        if has_substring ~needle:"/sync" url then (
          match !responses with
          | response :: rest ->
              responses := rest;
              Fetch_mock.respond response request
          | [] ->
              Eio.Promise.await stopped;
              Fetch_mock.respond "{}" request)
        else if method_ = "POST" && has_substring ~needle:"/forget" url then
          Fetch_mock.respond "{}" request
        else Alcotest.failf "unexpected %s %s" method_ url)
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ~send_queue:(queue_for alice) ()
  in
  let a_typing = Ui.Runtime.typing_users runtime the_room in
  let b_typing = Ui.Runtime.typing_users runtime other_room in
  let strings users =
    List.map Matrix_proto.Id.User_id.to_string (Ui.Observable.Value.get users)
  in
  Alcotest.(check (list string)) "room A starts empty" [] (strings a_typing);
  Alcotest.(check (list string)) "room B starts empty" [] (strings b_typing);
  let count = List.length response_strings in
  let notifications = Array.init count (fun _ -> Eio.Promise.create ()) in
  let gates = Array.init count (fun _ -> Eio.Promise.create ()) in
  let seen = ref 0 in
  Ui.Runtime.start
    ~on_error:(fun error ->
      Alcotest.failf "typing sync failed: %a" Matrix_eio.Error.pp_err error)
    ~on_change:(fun _ _ ->
      let index = !seen in
      incr seen;
      if index >= count then Alcotest.fail "too many typing sync responses";
      Eio.Promise.resolve (snd notifications.(index)) ();
      if index = count - 1 then (
        Ui.Runtime.stop runtime;
        Eio.Promise.resolve stopped_resolver ())
      else Eio.Promise.await (fst gates.(index)))
    runtime;
  let step index check =
    Eio.Promise.await (fst notifications.(index));
    check ();
    Eio.Promise.resolve (snd gates.(index)) ()
  in
  step 0 (fun () ->
      Alcotest.(check (list string))
        "filters the current user, preserving order"
        [ "@bob:example.org"; "@carol:example.org" ]
        (strings a_typing));
  step 1 (fun () ->
      Alcotest.(check (list string))
        "room B has its own typing users" [ "@carol:example.org" ]
        (strings b_typing);
      Alcotest.(check (list string))
        "room A is isolated from room B"
        [ "@bob:example.org"; "@carol:example.org" ]
        (strings a_typing));
  step 2 (fun () ->
      Alcotest.(check (list string))
        "a later valid update replaces room A"
        [ "@carol:example.org"; "@bob:example.org" ]
        (strings a_typing));
  step 3 (fun () ->
      Alcotest.(check (list string))
        "without m.typing the state is retained"
        [ "@carol:example.org"; "@bob:example.org" ]
        (strings a_typing));
  step 4 (fun () ->
      Alcotest.(check (list string))
        "malformed m.typing is ignored"
        [ "@carol:example.org"; "@bob:example.org" ]
        (strings a_typing));
  step 5 (fun () ->
      Alcotest.(check (list string))
        "empty user_ids clears the room" [] (strings a_typing));
  step 6 (fun () ->
      Alcotest.(check (list string))
        "typing is populated before forget" [ "@carol:example.org" ]
        (strings a_typing));
  Alcotest.(check int) "all typing responses were applied" count !seen;
  (match Ui.Runtime.forget runtime the_room with
  | Error error -> Alcotest.fail (Matrix_client.Error.to_string error)
  | Ok () -> ());
  Alcotest.(check (list string))
    "forget clears the existing observable" [] (strings a_typing);
  let fresh = Ui.Runtime.typing_users runtime the_room in
  Alcotest.(check (list string))
    "a fresh handle after forget is empty" [] (strings fresh)

let direct_sync_response () =
  match
    Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
      {|{"next_batch":"direct","account_data":{"events":[{"type":"m.direct","content":{"@bob:example.org":["!room:example.org"],"@other:example.org":["!keep:example.org"]}}]},"rooms":{"join":{"!room:example.org":{"timeline":{"events":[],"limited":false}}}}}|}
  with
  | Ok response -> response
  | Error error -> Alcotest.failf "bad direct-room response: %s" error

let request_body (request : Fetch.Middleware.request) =
  match request.body with
  | Fetch.String body -> body
  | Fetch.Empty | Fetch.Stream _ -> Alcotest.fail "expected a string body"

let test_runtime_forget_direct_updates_account_data () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let state_store = Matrix_client.Store.memory () in
  let initial, _ =
    Matrix_client.Base_client.apply
      (Matrix_client.Base_client.create ~user_id:alice ())
      (direct_sync_response ())
  in
  Matrix_client.Base_client.persist state_store initial;
  let sync =
    Matrix_eio.Sync_service.of_store ~store:state_store ~user_id:alice ()
  in
  let put_bodies = ref [] in
  let client =
    runtime_client ~sw ~env (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        match Http.Method.to_string request.meth with
        | "GET" when has_substring ~needle:"/account_data/m.direct" url ->
            Fetch_mock.respond
              {|{"@bob:example.org":["!room:example.org"],"@other:example.org":["!keep:example.org"]}|}
              request
        | "PUT" when has_substring ~needle:"/account_data/m.direct" url ->
            put_bodies := request_body request :: !put_bodies;
            Fetch_mock.respond "{}" request
        | "POST" when has_substring ~needle:"/forget" url ->
            Fetch_mock.respond "{}" request
        | method_ -> Alcotest.failf "unexpected %s %s" method_ url)
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client ~sync
      ~send_queue:(queue_for alice) ()
  in
  Alcotest.(check bool)
    "room starts direct" true
    (match
       Matrix_client.Base_client.find_room
         (Matrix_eio.Sync_service.state sync)
         the_room
     with
    | Some info -> info.is_dm
    | None -> false);
  (match Ui.Runtime.forget runtime the_room with
  | Error error -> Alcotest.fail (Matrix_client.Error.to_string error)
  | Ok () -> ());
  Alcotest.(check int) "one m.direct write" 1 (List.length !put_bodies);
  let written =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json
        (List.hd !put_bodies)
    with
    | Ok json -> json
    | Error error -> Alcotest.failf "bad m.direct write: %s" error
  in
  Alcotest.(check bool)
    "forgotten room is removed from written m.direct" true
    (Matrix_proto.Json.find_mem "@bob:example.org" written = None);
  Alcotest.(check bool)
    "unrelated m.direct association survives" true
    (Matrix_proto.Json.find_mem "@other:example.org" written <> None);
  Alcotest.(check bool)
    "local account data is updated" true
    (match
       Matrix_client.Base_client.find_account_data
         (Matrix_eio.Sync_service.state sync)
         "m.direct"
     with
    | Some json -> Matrix_proto.Json.find_mem "@bob:example.org" json = None
    | None -> false);
  Alcotest.(check bool)
    "updated account data is persisted" true
    (match Matrix_client.Store.find_account_data state_store "m.direct" with
    | Some json -> Matrix_proto.Json.find_mem "@bob:example.org" json = None
    | None -> false)

let test_runtime_forget_direct_cleanup_failure_still_forgets () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let state_store = Matrix_client.Store.memory () in
  let initial, _ =
    Matrix_client.Base_client.apply
      (Matrix_client.Base_client.create ~user_id:alice ())
      (direct_sync_response ())
  in
  Matrix_client.Base_client.persist state_store initial;
  let sync =
    Matrix_eio.Sync_service.of_store ~store:state_store ~user_id:alice ()
  in
  let puts = ref 0 in
  let client =
    runtime_client ~sw ~env (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        match Http.Method.to_string request.meth with
        | "GET" when has_substring ~needle:"/account_data/m.direct" url ->
            Fetch_mock.respond ~status:503 "{}" request
        | "PUT" when has_substring ~needle:"/account_data/m.direct" url ->
            incr puts;
            Fetch_mock.respond "{}" request
        | "POST" when has_substring ~needle:"/forget" url ->
            Fetch_mock.respond "{}" request
        | method_ -> Alcotest.failf "unexpected %s %s" method_ url)
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client ~sync
      ~send_queue:(queue_for alice) ()
  in
  (match Ui.Runtime.forget runtime the_room with
  | Error error -> Alcotest.fail (Matrix_client.Error.to_string error)
  | Ok () -> ());
  Alcotest.(check int) "failed cleanup does not issue a PUT" 0 !puts;
  Alcotest.(check bool)
    "room is still forgotten locally" true
    (Matrix_client.Base_client.find_room
       (Matrix_eio.Sync_service.state sync)
       the_room
    = None);
  Alcotest.(check bool)
    "failed cleanup leaves m.direct for retry" true
    (match
       Matrix_client.Base_client.find_account_data
         (Matrix_eio.Sync_service.state sync)
         "m.direct"
     with
    | Some json -> Matrix_proto.Json.find_mem "@bob:example.org" json <> None
    | None -> false)

let test_runtime_sync_offline () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let attempts = ref 0 in
  let release_second, release_second_resolver = Eio.Promise.create () in
  let client =
    runtime_client ~sw ~env (fun request ->
        incr attempts;
        if !attempts = 1 then Fetch_mock.respond ~status:503 "{}" request
        else if !attempts = 2 then (
          Eio.Promise.await release_second;
          Fetch_mock.respond {|{"next_batch":"live"}|} request)
        else (
          (* Keep the loop from issuing an unbounded stream of successful
             requests before the test has observed [Live]. *)
          Eio.Time.sleep clock 60.;
          Fetch_mock.respond {|{"next_batch":"held"}|} request))
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock ~client
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ()
  in
  Ui.Runtime.start
    ~on_error:(fun _ -> Matrix_eio.Sync_service.Retry_after 0.01)
    runtime;
  (* The second request is held, leaving the state observably [Offline] until
     this test releases it. *)
  Eio.Time.sleep clock 0.02;
  Alcotest.(check bool)
    "a retry publishes Offline" true
    (Ui.Observable.Value.get (Ui.Runtime.sync_state runtime)
    = Ui.Runtime.Offline);
  Eio.Promise.resolve release_second_resolver ();
  let rec wait_live rounds =
    if rounds > 0 then (
      match Ui.Observable.Value.get (Ui.Runtime.sync_state runtime) with
      | Ui.Runtime.Live _ -> ()
      | _ ->
          Eio.Time.sleep clock 0.001;
          wait_live (rounds - 1))
    else Alcotest.fail "successful retry did not publish Live"
  in
  wait_live 1000;
  Ui.Runtime.stop runtime;
  Alcotest.(check bool) "the failed request was retried" true (!attempts >= 2)

let test_runtime_sync_terminal_failure () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let client =
    runtime_client ~sw ~env (fun request ->
        Fetch_mock.respond ~status:401
          {|{"errcode":"M_UNKNOWN_TOKEN","error":"gone"}|} request)
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock ~client
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ()
  in
  let failed_seen = ref false in
  Ui.Runtime.start
    ~on_error:(fun _ ->
      (failed_seen :=
         match Ui.Observable.Value.get (Ui.Runtime.sync_state runtime) with
         | Ui.Runtime.Failed _ -> true
         | _ -> false);
      Eio.Fiber.yield ();
      Matrix_eio.Sync_service.Stop)
    runtime;
  Alcotest.(check bool) "terminal failure is observable" true !failed_seen;
  let rec wait_stopped rounds =
    if rounds > 0 then
      if
        Ui.Observable.Value.get (Ui.Runtime.sync_state runtime)
        = Ui.Runtime.Stopped
      then true
      else (
        Eio.Time.sleep clock 0.001;
        wait_stopped (rounds - 1))
    else false
  in
  Alcotest.(check bool) "terminal stop is observable" true (wait_stopped 1000);
  Alcotest.(check bool)
    "terminal failure does not become Offline" false
    (Ui.Observable.Value.get (Ui.Runtime.sync_state runtime)
    = Ui.Runtime.Offline)

let test_runtime_stop_while_offline () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let client =
    runtime_client ~sw ~env (fun request ->
        Fetch_mock.respond ~status:503 "{}" request)
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock ~client
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ()
  in
  Ui.Runtime.start
    ~on_error:(fun _ -> Matrix_eio.Sync_service.Retry_after 60.)
    runtime;
  Eio.Time.sleep clock 0.02;
  Alcotest.(check bool)
    "the failed loop is offline" true
    (Ui.Observable.Value.get (Ui.Runtime.sync_state runtime)
    = Ui.Runtime.Offline);
  Ui.Runtime.stop runtime;
  Alcotest.(check bool)
    "stop overrides Offline" true
    (Ui.Observable.Value.get (Ui.Runtime.sync_state runtime)
    = Ui.Runtime.Stopped);
  Eio.Fiber.yield ();
  Alcotest.(check bool)
    "cancellation keeps it stopped" true
    (Ui.Observable.Value.get (Ui.Runtime.sync_state runtime)
    = Ui.Runtime.Stopped)

let test_runtime_forget_drops_in_flight_sync () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let sync_started, sync_started_resolver = Eio.Promise.create () in
  let release_sync, release_sync_resolver = Eio.Promise.create () in
  let attempts = ref 0 in
  let published = ref 0 in
  let client =
    runtime_client ~sw ~env (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        if has_substring ~needle:"/sync" url then (
          incr attempts;
          if !attempts = 1 then (
            if not (Eio.Promise.is_resolved sync_started) then
              Eio.Promise.resolve sync_started_resolver ();
            Eio.Promise.await release_sync;
            Fetch_mock.respond
              {|{"next_batch":"stale","rooms":{"join":{"!room:example.org":{"timeline":{"events":[{"event_id":"$stale","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{"msgtype":"m.text","body":"stale"}}]}}}}}|}
              request)
          else
            Fetch_mock.respond ~status:401
              {|{"errcode":"M_UNKNOWN_TOKEN","error":"stop"}|} request)
        else Fetch_mock.respond "{}" request)
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock ~client
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ()
  in
  Ui.Runtime.start
    ~on_error:(fun _ -> Matrix_eio.Sync_service.Stop)
    ~on_change:(fun _ _ -> incr published)
    runtime;
  Eio.Promise.await sync_started;
  (match Ui.Runtime.forget runtime the_room with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Matrix_client.Error.to_string error));
  Eio.Promise.resolve release_sync_resolver ();
  let rec wait_stopped rounds =
    if rounds = 0 then Alcotest.fail "stale sync did not finish"
    else if
      Ui.Observable.Value.get (Ui.Runtime.sync_state runtime)
      = Ui.Runtime.Stopped
    then ()
    else (
      Eio.Time.sleep clock 0.001;
      wait_stopped (rounds - 1))
  in
  wait_stopped 1000;
  Alcotest.(check bool)
    "a response fetched before forget cannot resurrect the room" true
    (Option.is_none
       (Matrix_client.Base_client.find_room
          (Matrix_eio.Sync_service.state (Ui.Runtime.sync_service runtime))
          the_room));
  Alcotest.(check int) "the stale response was not published" 0 !published

(* The network race above drops a response before entering [apply]. This one
   exercises the callback re-entrancy boundary: the initial key upload fails
   and its error callback synchronously forgets the room while the response is
   still in the yielding crypto prelude. The stale response must be discarded
   without deadlocking or publishing a room. *)
let test_runtime_forget_drops_sync_after_crypto_prelude () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let sync_attempts = ref 0 in
  let published = ref 0 in
  let handler request =
    let url = Fetch.Middleware.Url.to_string request.Fetch.Middleware.url in
    if has_substring ~needle:"/sync" url then (
      incr sync_attempts;
      if !sync_attempts = 1 then
        Fetch_mock.respond
          {|{"next_batch":"stale","rooms":{"join":{"!room:example.org":{"timeline":{"events":[{"event_id":"$stale-crypto","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{"msgtype":"m.text","body":"stale"}}]}}}}}|}
          request
      else
        Fetch_mock.respond ~status:401
          {|{"errcode":"M_UNKNOWN_TOKEN","error":"stop"}|} request)
    else if has_substring ~needle:"/keys/upload" url then
      Fetch_mock.respond ~status:503
        {|{"errcode":"M_UNAVAILABLE","error":"key upload held"}|} request
    else Fetch_mock.respond "{}" request
  in
  let client = runtime_client ~sw ~env handler in
  let encryption =
    Matrix_eio.Encryption.create
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 400_000 'p')))
      ~user_id:alice
      ~device_id:(Matrix_proto.Id.Device_id.of_string_exn "ALICEDEV")
      ()
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock ~client
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ~encryption ~send_queue:(queue_for alice) ()
  in
  Ui.Runtime.start
    ~on_error:(fun _ -> Matrix_eio.Sync_service.Stop)
    ~on_encryption_error:(fun _ ->
      match Ui.Runtime.forget runtime the_room with
      | Ok () -> ()
      | Error error -> Alcotest.fail (Matrix_client.Error.to_string error))
    ~on_change:(fun _ _ -> incr published)
    runtime;
  let rec wait_stopped rounds =
    if rounds = 0 then Alcotest.fail "crypto-prelude race did not finish"
    else if
      Ui.Observable.Value.get (Ui.Runtime.sync_state runtime)
      = Ui.Runtime.Stopped
    then ()
    else (
      Eio.Time.sleep clock 0.001;
      wait_stopped (rounds - 1))
  in
  wait_stopped 1000;
  Alcotest.(check bool)
    "crypto-prelude forget prevents the stale room" true
    (Option.is_none
       (Matrix_client.Base_client.find_room
          (Matrix_eio.Sync_service.state (Ui.Runtime.sync_service runtime))
          the_room));
  Alcotest.(check int) "crypto-prelude response was not folded" 0 !published

(* The staged transaction must also leave the old token untouched when a
   yielding post-fold keys query invalidates the response. The retry then
   receives unrelated room B and must not lose it while forgetting room A. *)
let test_runtime_forget_drops_sync_after_crypto_postfold () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let sync_attempts = ref 0 in
  let query_attempts = ref 0 in
  let published = ref 0 in
  let stale_response =
    {|{"next_batch":"stale-post","rooms":{"join":{"!room:example.org":{"state":{"events":[{"type":"m.room.encryption","state_key":"","sender":"@alice:example.org","event_id":"$enc-a","origin_server_ts":1700000000000,"content":{"algorithm":"m.megolm.v1.aes-sha2"}},{"type":"m.room.member","state_key":"@bob:example.org","sender":"@alice:example.org","event_id":"$member-a","origin_server_ts":1700000000000,"content":{"membership":"join"}}]},"timeline":{"events":[{"event_id":"$stale-a","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{"msgtype":"m.text","body":"stale A"}}]}},"!other:example.org":{"timeline":{"events":[{"event_id":"$stale-b","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{"msgtype":"m.text","body":"stale B"}}]}}}}}|}
  in
  let fresh_response =
    {|{"next_batch":"fresh-post","rooms":{"join":{"!other:example.org":{"timeline":{"events":[{"event_id":"$fresh-b","sender":"@alice:example.org","origin_server_ts":1700000000001,"type":"m.room.message","content":{"msgtype":"m.text","body":"fresh B"}}]}}}}}|}
  in
  let handler request =
    let url = Fetch.Middleware.Url.to_string request.Fetch.Middleware.url in
    if has_substring ~needle:"/sync" url then (
      incr sync_attempts;
      match !sync_attempts with
      | 1 -> Fetch_mock.respond stale_response request
      | 2 -> Fetch_mock.respond fresh_response request
      | _ ->
          Fetch_mock.respond ~status:401
            {|{"errcode":"M_UNKNOWN_TOKEN","error":"stop"}|} request)
    else if has_substring ~needle:"/keys/upload" url then
      Fetch_mock.respond "{}" request
    else if has_substring ~needle:"/keys/query" url then (
      incr query_attempts;
      if !query_attempts = 1 then
        Fetch_mock.respond ~status:503
          {|{"errcode":"M_UNAVAILABLE","error":"query held"}|} request
      else Fetch_mock.respond "{}" request)
    else Fetch_mock.respond "{}" request
  in
  let client = runtime_client ~sw ~env handler in
  let encryption =
    Matrix_eio.Encryption.create
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 400_000 'q')))
      ~user_id:alice
      ~device_id:(Matrix_proto.Id.Device_id.of_string_exn "ALICEDEV")
      ()
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock ~client
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ~encryption ~send_queue:(queue_for alice) ()
  in
  Ui.Runtime.start
    ~on_error:(fun _ -> Matrix_eio.Sync_service.Stop)
    ~on_encryption_error:(fun _ ->
      match Ui.Runtime.forget runtime the_room with
      | Ok () -> ()
      | Error error -> Alcotest.fail (Matrix_client.Error.to_string error))
    ~on_change:(fun _ _ -> incr published)
    runtime;
  let rec wait_stopped rounds =
    if rounds = 0 then Alcotest.fail "crypto post-fold race did not finish"
    else if
      Ui.Observable.Value.get (Ui.Runtime.sync_state runtime)
      = Ui.Runtime.Stopped
    then ()
    else (
      Eio.Time.sleep clock 0.001;
      wait_stopped (rounds - 1))
  in
  wait_stopped 1000;
  Alcotest.(check bool)
    "post-fold forget leaves room A absent" true
    (Option.is_none
       (Matrix_client.Base_client.find_room
          (Matrix_eio.Sync_service.state (Ui.Runtime.sync_service runtime))
          the_room));
  Alcotest.(check int)
    "post-fold test made three sync requests" 3 !sync_attempts;
  Alcotest.(check int) "post-fold test retried the keys query" 2 !query_attempts;
  let other_room = Matrix_proto.Id.Room_id.of_string_exn "!other:example.org" in
  Alcotest.(check bool)
    "retry applies unrelated room B" true
    (Option.is_some
       (Matrix_client.Base_client.find_room
          (Matrix_eio.Sync_service.state (Ui.Runtime.sync_service runtime))
          other_room));
  Alcotest.(check int) "stale post-fold response was not published" 1 !published

let test_runtime_send_queue_dependencies () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let queue_store = Matrix_client.Store.memory () in
  let media_store = Matrix_client.Media_store.memory () in
  let client =
    runtime_client ~sw ~env (fun _ ->
        Alcotest.fail "runtime test made a request")
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ~send_queue_store:queue_store ~send_queue_media_store:media_store
      ~send_queue_media_owner:"runtime-owner" ()
  in
  let queue = Ui.Runtime.send_queue runtime in
  Alcotest.(check bool)
    "queue store is forwarded" true
    (Matrix_client.Send_queue.store queue = Some queue_store);
  let request =
    Matrix_client.Send_queue.upload queue ~room_id:the_room ~role:`Original
      ~content_type:"text/plain" ~data:"persisted upload" ()
  in
  let key =
    Matrix_client.Media_store.
      {
        uri = local_uri ~txn_id:(Matrix_client.Send_queue.txn_id request);
        format = File;
      }
  in
  Alcotest.(check (option string))
    "media store is forwarded" (Some "persisted upload")
    (Result.get_ok
       (Matrix_client.Media_store.get ~now:Ptime.epoch media_store key));
  let older_than =
    Option.get (Ptime.add_span Ptime.epoch (Ptime.Span.of_int_s 1))
  in
  Alcotest.(check unit)
    "media owner is forwarded" ()
    (Result.get_ok
       (Matrix_client.Media_store.prune_local ~owner:"runtime-owner" ~keep:[]
          ~older_than media_store));
  Alcotest.(check (option string))
    "owned media is reclaimable" None
    (Result.get_ok
       (Matrix_client.Media_store.get ~now:Ptime.epoch media_store key));
  let explicit = queue_for alice in
  Alcotest.check_raises "construction args reject explicit queue"
    (Invalid_argument
       "Matrix_ui.Runtime.create: send_queue construction arguments cannot be \
        combined with an explicit send_queue") (fun () ->
      ignore
        (Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client
           ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
           ~send_queue:explicit ~send_queue_store:queue_store ()))

let runtime_encryption_fixture () =
  let random =
    Matrix_client.Random.of_source
      (Eio.Flow.string_source (String.make 400_000 'e'))
  in
  let _, sender_key = Ck.Curve25519.generate ~random () in
  let outbound = Olm.Megolm.Outbound.create ~random ~room_id:the_room () in
  let session_id = Olm.Megolm.Outbound.session_id outbound in
  let session_key = Olm.Megolm.Outbound.exported_session_key outbound in
  let payload =
    Printf.sprintf
      {|{"room_id":"%s","type":"m.room.message","content":{"msgtype":"m.text","body":"late"}}|}
      (Matrix_proto.Id.Room_id.to_string the_room)
  in
  let ciphertext = Olm.Megolm.Outbound.encrypt outbound payload in
  let sender_key = Ck.Curve25519.Public.to_base64 sender_key in
  let content =
    Printf.sprintf
      {|{"algorithm":"m.megolm.v1.aes-sha2","sender_key":"%s","session_id":"%s","ciphertext":"%s"}|}
      sender_key
      (Matrix_proto.Id.Session_id.to_string session_id)
      ciphertext.ciphertext
  in
  let encrypted =
    raw
      (Printf.sprintf
         {|{"event_id":"$late","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.encrypted","room_id":"!room:example.org","content":%s}|}
         content)
  in
  (encrypted, session_id, session_key, sender_key)

let runtime_for ~sw ~env ?encryption ~on_room_key_request () =
  let device_id = Matrix_proto.Id.Device_id.of_string_exn "ALICEDEV" in
  let client =
    mock_client ~sw ~env (fun _ -> Alcotest.fail "runtime test made a request")
    |> fun client ->
    Matrix_eio.Client.with_session client
      {
        Matrix_client.Client.user_id = alice;
        device_id;
        access_token = "test-token";
        refresh_token = None;
      }
  in
  Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client
    ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
    ?encryption ~on_room_key_request ~send_queue:(queue_for alice) ()

let test_runtime_timeline_resolver_isolation () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let runtime = runtime_for ~sw ~env ~on_room_key_request:(fun _ -> ()) () in
  let resolve_a value =
    if String.equal value "mxc://hs.example/image" then
      Some "https://a.example/image"
    else None
  in
  let resolve_b value =
    if String.equal value "mxc://hs.example/image" then
      Some "https://b.example/image"
    else None
  in
  Ui.Event_cache.prepend
    (Ui.Runtime.event_cache runtime)
    the_room
    ~events:
      [
        message ~id:"$resolver"
          {|{"msgtype":"m.text","body":"resolver","format":"org.matrix.custom.html","formatted_body":"<img src=\"mxc://hs.example/image\">"}|};
      ]
    ~prev_batch:None;
  let first = Ui.Runtime.timeline ~resolve_mxc:resolve_a runtime the_room in
  let second = Ui.Runtime.timeline ~resolve_mxc:resolve_b runtime the_room in
  Alcotest.(check bool) "existing timeline is reused" true (first == second);
  match timeline_events first with
  | [ item ] -> (
      match formatted_html item.event with
      | Some html ->
          check_contains "original resolver remains fixed"
            "https://a.example/image" html;
          Alcotest.(check bool)
            "reused timeline does not use new resolver" false
            (Ui.Matching.contains ~haystack:html
               ~needle:"https://b.example/image")
      | None -> Alcotest.fail "resolver test has no formatted presentation")
  | _ -> Alcotest.fail "expected one resolver test event"

let test_runtime_stop_closes_timelines () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let encryption =
    Matrix_eio.Encryption.create
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 400_000 's')))
      ~user_id:alice
      ~device_id:(Matrix_proto.Id.Device_id.of_string_exn "ALICEDEV")
      ()
  in
  let runtime =
    runtime_for ~sw ~env ~encryption ~on_room_key_request:(fun _ -> ()) ()
  in
  let cache = Ui.Runtime.event_cache runtime in
  Ui.Event_cache.prepend cache the_room
    ~events:[ message ~id:"$before-stop" {|{"body":"before"}|} ]
    ~prev_batch:None;
  let timeline = Ui.Runtime.timeline runtime the_room in
  let before = Array.length (Ui.Room_timeline.snapshot timeline) in
  Alcotest.(check bool)
    "the timeline has the event before stop" true (before > 0);
  Ui.Runtime.stop runtime;
  Ui.Event_cache.prepend cache the_room
    ~events:[ message ~id:"$after-stop" {|{"body":"after"}|} ]
    ~prev_batch:None;
  Eio.Fiber.yield ();
  Alcotest.(check int)
    "a stopped timeline no longer observes the cache" before
    (Array.length (Ui.Room_timeline.snapshot timeline));
  Alcotest.(check bool)
    "the runtime forgets the closed timeline" false
    (Ui.Runtime.timeline runtime the_room == timeline)

let test_runtime_thread_list_lifecycle () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let root = message ~id:"$runtime-thread" {|{"body":"root"}|} in
  let client =
    runtime_client ~sw ~env (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        if has_substring ~needle:"/threads" url then
          Fetch_mock.respond
            (Printf.sprintf {|{"chunk":[%s]}|}
               (Result.get_ok
                  (Jsont_bytesrw.encode_string Event.Raw_event.jsont root)))
            request
        else Fetch_mock.respond "{}" request)
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ~send_queue:(queue_for alice) ()
  in
  let first = Ui.Runtime.thread_list runtime the_room in
  Alcotest.(check bool)
    "repeated calls return the same thread list" true
    (first == Ui.Runtime.thread_list runtime the_room);
  (match Ui.Thread_list.next_page first () with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Matrix_client.Error.to_string error));
  Alcotest.(check int)
    "the thread list admits a fetched root" 1
    (Array.length (Ui.Thread_list.snapshot first));
  let root_id = Option.get root.Event.Raw_event.event_id in
  Alcotest.(check bool)
    "runtime wires thread roots into the shared cache" true
    (Option.is_some
       (Ui.Event_cache.find_event
          (Ui.Runtime.event_cache runtime)
          the_room root_id));
  (match Ui.Runtime.forget runtime the_room with
  | Error error -> Alcotest.fail (Matrix_client.Error.to_string error)
  | Ok () -> ());
  Alcotest.(check int)
    "forget closes the old thread list" 0
    (Array.length (Ui.Thread_list.snapshot first));
  Alcotest.(check bool)
    "forget clears the runtime's detached root" false
    (Option.is_some
       (Ui.Event_cache.find_event
          (Ui.Runtime.event_cache runtime)
          the_room root_id));
  Ui.Thread_info.ingest_root
    (Ui.Runtime.thread_info runtime)
    ~room_id:the_room root;
  Alcotest.(check int)
    "a closed thread list ignores later summary updates" 0
    (Array.length (Ui.Thread_list.snapshot first));
  let replacement = Ui.Runtime.thread_list runtime the_room in
  Alcotest.(check bool)
    "forget creates a fresh thread list" false (replacement == first);
  (match Ui.Thread_list.next_page replacement () with
  | Ok () -> ()
  | Error error -> Alcotest.fail (Matrix_client.Error.to_string error));
  Ui.Runtime.stop runtime;
  Alcotest.(check int)
    "stop closes the replacement thread list" 0
    (Array.length (Ui.Thread_list.snapshot replacement));
  Ui.Thread_info.ingest_root
    (Ui.Runtime.thread_info runtime)
    ~room_id:the_room root;
  Alcotest.(check int)
    "a stopped thread list cannot be repopulated" 0
    (Array.length (Ui.Thread_list.snapshot replacement));
  Alcotest.(check bool)
    "stop removes the registry entry" false
    (Ui.Runtime.thread_list runtime the_room == replacement);
  (* The accessor deliberately remains usable after [stop], like [timeline].
     A second stop proves that newly-created handle is covered as well. *)
  Ui.Runtime.stop runtime

let backup_for_fixture ~random ~backup_key ~session_key ~sender_key ~session_id
    =
  let session_data =
    match
      Backup.encrypt_room_key ~random
        (Backup.Decryption_key.public backup_key)
        ~session_key ~sender_key
    with
    | Ok data -> data
    | Error (`Msg message) -> Alcotest.fail message
  in
  [
    ( Matrix_proto.Id.Room_id.to_string the_room,
      [
        ( Matrix_proto.Id.Session_id.to_string session_id,
          {
            Backup.first_message_index = 0;
            forwarded_count = 0;
            is_verified = false;
            session_data;
          } );
      ] );
  ]

let test_runtime_unknown_session_requests_once () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let encrypted, session_id, session_key, sender_key =
    runtime_encryption_fixture ()
  in
  let encryption =
    Matrix_eio.Encryption.create
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 400_000 'r')))
      ~user_id:alice
      ~device_id:(Matrix_proto.Id.Device_id.of_string_exn "ALICEDEV")
      ()
  in
  let requests = ref 0 in
  let runtime =
    runtime_for ~sw ~env ~encryption
      ~on_room_key_request:(fun _ -> incr requests)
      ()
  in
  Ui.Event_cache.prepend
    (Ui.Runtime.event_cache runtime)
    the_room ~events:[ encrypted ] ~prev_batch:None;
  ignore (Ui.Runtime.timeline runtime the_room);
  Ui.Runtime.close_timeline runtime the_room;
  ignore (Ui.Runtime.timeline runtime the_room);
  Alcotest.(check int)
    "one request for repeated unknown-session observations" 1 !requests;
  Alcotest.(check int)
    "the encrypted event remains one cache item" 1
    (Array.length
       (Ui.Event_cache.snapshot (Ui.Runtime.event_cache runtime) the_room));
  (* Importing the late session and opening the room again exercises the same
     retry path used after a successful sync. *)
  let backup_key =
    Backup.Decryption_key.generate
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 400_000 'b')))
  in
  Matrix_eio.Encryption.enable_backup encryption ~version:"v1"
    ~decryption_key:backup_key
    (Backup.Decryption_key.public backup_key);
  let rooms =
    backup_for_fixture
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 400_000 'k')))
      ~backup_key ~session_key ~sender_key ~session_id
  in
  (match
     Matrix_client.Encryption.import_backup
       (Matrix_eio.Encryption.machine encryption)
       rooms
   with
  | Ok 1 -> ()
  | Ok count -> Alcotest.failf "imported %d sessions, expected one" count
  | Error error -> Alcotest.fail (Matrix_client.Error.to_string error));
  Ui.Runtime.close_timeline runtime the_room;
  ignore (Ui.Runtime.timeline runtime the_room);
  let cached =
    Ui.Event_cache.snapshot (Ui.Runtime.event_cache runtime) the_room
  in
  Alcotest.(check int) "late decryption does not append" 1 (Array.length cached);
  Alcotest.(check bool)
    "late key installs plaintext in place" true
    (Option.is_some cached.(0).clear_event);
  Alcotest.(check int)
    "successful recovery clears the request identity" 1 !requests

let test_sync_encryption_error_callback_isolated () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    mock_client ~sw ~env (fun request ->
        Fetch_mock.respond ~status:503
          {|{"errcode":"M_UNAVAILABLE","error":"offline"}|} request)
    |> fun client ->
    Matrix_eio.Client.with_session client
      {
        Matrix_client.Client.user_id = alice;
        device_id = Matrix_proto.Id.Device_id.of_string_exn "ALICEDEV";
        access_token = "test-token";
        refresh_token = None;
      }
  in
  let encryption =
    Matrix_eio.Encryption.create
      ~random:
        (Matrix_client.Random.of_source
           (Eio.Flow.string_source (String.make 400_000 'e')))
      ~user_id:alice
      ~device_id:(Matrix_proto.Id.Device_id.of_string_exn "ALICEDEV")
      ()
  in
  let service = Matrix_eio.Sync_service.of_user ~user_id:alice () in
  let callback_calls = ref 0 in
  let _changes =
    Matrix_eio.Sync_service.apply ~encryption
      ~on_encryption_error:(fun _ ->
        incr callback_calls;
        failwith "observer failure")
      client service
      (sync_response ~batch:"callback" ~prev_batch:"p" [])
  in
  Alcotest.(check int) "request error callback invoked" 1 !callback_calls;
  Alcotest.(check (option string))
    "response folded after callback failure" (Some "callback")
    (Matrix_client.Base_client.next_batch
       (Matrix_eio.Sync_service.state service))

let messages_page ~id ~body ~end_ =
  let event =
    Printf.sprintf
      {|{"event_id":"%s","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{"msgtype":"m.text","body":"%s"}}|}
      id body
  in
  match end_ with
  | Some token ->
      Printf.sprintf {|{"start":"from","end":"%s","chunk":[%s]}|} token event
  | None -> Printf.sprintf {|{"start":"from","chunk":[%s]}|} event

let receipt_backfill_sync_response ?(target = "$receipt:example.org") () =
  Printf.sprintf
    {|{"next_batch":"receipt-sync","rooms":{"join":{"!room:example.org":{"timeline":{"events":[{"event_id":"$new:example.org","sender":"@bob:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{"msgtype":"m.text","body":"new"}}],"limited":true,"prev_batch":"b"},"ephemeral":{"events":[{"type":"m.receipt","content":{%S:{"m.read":{"@alice:example.org":{"ts":1}}}}}]}}}}}|}
    target

let no_receipt_sync_response () =
  let json =
    {|{"next_batch":"no-receipt-sync","rooms":{"join":{"!room:example.org":{"timeline":{"events":[{"event_id":"$no-receipt","sender":"@bob:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{"msgtype":"m.text","body":"unread"}}]}}}}}|}
  in
  match Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont json with
  | Ok response -> response
  | Error message -> Alcotest.fail message

let test_runtime_recounts_without_receipt () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let first = ref true in
  let blocked, _blocked_resolver = Eio.Promise.create () in
  let done_, resolver = Eio.Promise.create () in
  let handler (request : Fetch.Middleware.request) =
    let url = Fetch.Middleware.Url.to_string request.url in
    if has_substring ~needle:"/sync" url then
      if !first then begin
        first := false;
        Fetch_mock.respond
          (Result.get_ok
             (Jsont_bytesrw.encode_string Matrix_proto.Sync.Response.jsont
                (no_receipt_sync_response ())))
          request
      end
      else Eio.Promise.await blocked
    else Alcotest.failf "unexpected request %s" url
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env)
      ~client:(runtime_client ~sw ~env handler)
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ~send_queue:(queue_for alice) ()
  in
  (* This event exists only in the physical cache. The base fold sees just the
     incremental sync event and reports one unread message, so reaching two
     proves that Runtime performed the no-receipt known-window recount. *)
  Ui.Event_cache.prepend
    (Ui.Runtime.event_cache runtime)
    the_room
    ~events:
      [
        message ~id:"$cached-before-sync" ~sender:"@bob:example.org"
          {|{"msgtype":"m.text","body":"cached"}|};
      ]
    ~prev_batch:None;
  Ui.Runtime.start
    ~on_error:(fun error ->
      Alcotest.failf "no-receipt sync failed: %a" Matrix_eio.Error.pp_err error)
    ~on_change:(fun _ _ ->
      Eio.Fiber.fork ~sw (fun () ->
          let rec wait attempts =
            let unread =
              match
                Ui.Room_list.find (Ui.Runtime.room_list runtime) the_room
              with
              | Some room -> room.unread_messages
              | None -> -1
            in
            if unread = 2 then ()
            else if attempts = 0 then
              Alcotest.failf
                "known window was not recounted without a receipt (got %d)"
                unread
            else begin
              Eio.Time.sleep (Eio.Stdenv.clock env) 0.001;
              wait (attempts - 1)
            end
          in
          wait 100;
          Eio.Promise.resolve resolver ();
          Ui.Runtime.stop runtime))
    runtime;
  Eio.Promise.await done_

let test_runtime_receipt_backfill () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let message_seen, message_seen_resolver = Eio.Promise.create () in
  let projection_done, projection_done_resolver = Eio.Promise.create () in
  let sync_seen = ref false in
  let handler (request : Fetch.Middleware.request) =
    let url = Fetch.Middleware.Url.to_string request.url in
    if has_substring ~needle:"/sync" url then
      begin if not !sync_seen then begin
        sync_seen := true;
        Fetch_mock.respond (receipt_backfill_sync_response ()) request
      end
      else Fetch_mock.respond "{}" request
      end
    else if has_substring ~needle:"/messages" url then begin
      Alcotest.(check bool)
        "backfill asks for the backward direction" true
        (has_substring ~needle:"dir=b" url);
      Alcotest.(check bool)
        "backfill uses the room token" true
        (has_substring ~needle:"from=b" url);
      Alcotest.(check bool)
        "backfill uses the bounded batch size" true
        (has_substring ~needle:"limit=30" url);
      let response =
        Fetch_mock.respond
          (messages_page ~id:"$receipt:example.org" ~body:"receipt" ~end_:None)
          request
      in
      Eio.Promise.resolve message_seen_resolver ();
      response
    end
    else Alcotest.failf "unexpected request %s" url
  in
  let client = runtime_client ~sw ~env handler in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ~send_queue:(queue_for alice) ()
  in
  (* A focused/pinned view may have the same event out of band; that copy must
     not satisfy the timeline receipt backfill. *)
  Ui.Event_cache.register_external_event
    (Ui.Runtime.event_cache runtime)
    the_room
    ~event:
      (message ~id:"$receipt:example.org"
         {|{"msgtype":"m.text","body":"external"}|});
  let timeline = Ui.Runtime.timeline runtime the_room in
  Ui.Runtime.start
    ~on_error:(fun error ->
      Alcotest.failf "receipt backfill sync failed: %a" Matrix_eio.Error.pp_err
        error)
    ~on_change:(fun _ _ ->
      Eio.Promise.await message_seen;
      let rec wait_for_projection attempts =
        let marker_visible () =
          Array.exists
            (function
              | Ui.Room_timeline.Virtual
                  { content = Ui.Room_timeline.Read_marker; _ } ->
                  true
              | Ui.Room_timeline.Event _ | Ui.Room_timeline.Virtual _ -> false)
            (Ui.Room_timeline.snapshot timeline)
        in
        if
          Option.is_some
            (Ui.Event_cache.position
               (Ui.Runtime.event_cache runtime)
               the_room
               (event_id "$receipt:example.org"))
          && marker_visible ()
        then ()
        else if attempts = 0 then
          Alcotest.failf "backfill did not complete (cache=%s)"
            (String.concat ","
               (Ui.Event_cache.snapshot
                  (Ui.Runtime.event_cache runtime)
                  the_room
               |> Array.to_list
               |> List.filter_map (fun (event : Ui.Event_cache.event) ->
                   Option.map Matrix_proto.Id.Event_id.to_string
                     event.event.event_id)))
        else begin
          Eio.Time.sleep (Eio.Stdenv.clock env) 0.001;
          wait_for_projection (attempts - 1)
        end
      in
      wait_for_projection 100;
      Eio.Promise.resolve projection_done_resolver ();
      Ui.Runtime.stop runtime)
    runtime;
  Eio.Promise.await projection_done;
  let cache = Ui.Runtime.event_cache runtime in
  Alcotest.(check bool)
    "receipt target is inserted into the timeline cache" true
    (Option.is_some
       (Ui.Event_cache.position cache the_room
          (event_id "$receipt:example.org")));
  Alcotest.(check bool)
    "the read marker is projected after backfill" true
    (Array.exists
       (function
         | Ui.Room_timeline.Virtual
             { content = Ui.Room_timeline.Read_marker; _ } ->
             true
         | Ui.Room_timeline.Event _ | Ui.Room_timeline.Virtual _ -> false)
       (Ui.Room_timeline.snapshot timeline));
  Alcotest.(check bool) "the initial sync was committed" true !sync_seen;
  Alcotest.(check int)
    "room unread count excludes the external copy" 1
    (match Ui.Room_list.find (Ui.Runtime.room_list runtime) the_room with
    | Some room -> room.unread_messages
    | None -> -1)

let test_runtime_receipt_backfill_terminal () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let message_seen, message_seen_resolver = Eio.Promise.create () in
  let finished, finished_resolver = Eio.Promise.create () in
  let message_requests = ref 0 in
  let sync_seen = ref false in
  let handler (request : Fetch.Middleware.request) =
    let url = Fetch.Middleware.Url.to_string request.url in
    if has_substring ~needle:"/sync" url then
      begin if not !sync_seen then begin
        sync_seen := true;
        Fetch_mock.respond
          (receipt_backfill_sync_response ~target:"$missing:example.org" ())
          request
      end
      else Fetch_mock.respond "{}" request
      end
    else if has_substring ~needle:"/messages" url then begin
      incr message_requests;
      Eio.Promise.resolve message_seen_resolver ();
      Fetch_mock.respond {|{"start":"b","chunk":[]}|} request
    end
    else Alcotest.failf "unexpected request %s" url
  in
  let client = runtime_client ~sw ~env handler in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ~send_queue:(queue_for alice) ()
  in
  Ui.Runtime.start
    ~on_error:(fun error ->
      Alcotest.failf "terminal receipt backfill sync failed: %a"
        Matrix_eio.Error.pp_err error)
    ~on_change:(fun _ _ ->
      Eio.Promise.await message_seen;
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
      Eio.Promise.resolve finished_resolver ();
      Ui.Runtime.stop runtime)
    runtime;
  Eio.Promise.await finished;
  Alcotest.(check int)
    "a terminal missing target is requested once" 1 !message_requests

let test_local_unread_counts_stale_expected_state () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    mock_client ~sw ~env (fun request -> Fetch_mock.respond "{}" request)
  in
  let service = Matrix_eio.Sync_service.of_user ~user_id:alice () in
  let response = sync_response ~batch:"s1" ~prev_batch:"p1" [ ("$a", "a") ] in
  ignore (Matrix_eio.Sync_service.apply client service response);
  let expected = Matrix_eio.Sync_service.state service in
  let response = sync_response ~batch:"s2" ~prev_batch:"p2" [ ("$b", "b") ] in
  ignore (Matrix_eio.Sync_service.apply client service response);
  let current = Matrix_eio.Sync_service.state service in
  let counts =
    { Matrix_client.Read_state.unread = 9; notifications = 8; highlights = 7 }
  in
  Alcotest.(check bool)
    "stale expected state is rejected" true
    (Option.is_none
       (Matrix_eio.Sync_service.set_local_unread_counts_if_current service
          ~room_id:the_room ~expected counts));
  Alcotest.(check bool)
    "stale update leaves active state unchanged" true
    (Matrix_eio.Sync_service.state service == current);
  Alcotest.(check bool)
    "the original expected state is no longer current" false
    (expected == current)

let test_local_unread_counts_persistence_and_rollback () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let tmp = Filename.temp_file "matrix-unread-room" ".d" in
  Unix.unlink tmp;
  Unix.mkdir tmp 0o700;
  let dir = Eio.Path.(Eio.Stdenv.fs env / tmp) in
  let store = Matrix_client.Store.on_disk ~dir in
  let client =
    mock_client ~sw ~env (fun request -> Fetch_mock.respond "{}" request)
  in
  let service =
    Matrix_eio.Sync_service.create ~store
      (Matrix_client.Base_client.create ~user_id:alice ())
  in
  ignore
    (Matrix_eio.Sync_service.apply client service
       (sync_response ~batch:"s1" ~prev_batch:"p1" [ ("$a", "a") ]));
  let expected = Matrix_eio.Sync_service.state service in
  let changed =
    { Matrix_client.Read_state.unread = 4; notifications = 3; highlights = 2 }
  in
  let committed =
    match
      Matrix_eio.Sync_service.set_local_unread_counts_if_current service
        ~room_id:the_room ~expected changed
    with
    | Some state -> state
    | None -> Alcotest.fail "room count update was unexpectedly stale"
  in
  Alcotest.(check int)
    "room-only update is active" 4
    (Option.get (Matrix_client.Base_client.find_room committed the_room))
      .local_unread_count;
  let restored_store = Matrix_client.Store.on_disk ~dir in
  let restored =
    Matrix_eio.Sync_service.of_store ~store:restored_store ~user_id:alice ()
  in
  Alcotest.(check int)
    "room-only update survives restart" 4
    (Option.get
       (Matrix_client.Base_client.find_room
          (Matrix_eio.Sync_service.state restored)
          the_room))
      .local_unread_count;
  (* Make the service's store handle stale, then force the room-only flush to
     fail. The active state and the in-memory room record must remain at the
     previously committed value. *)
  let competing = Matrix_client.Store.on_disk ~dir in
  Matrix_client.Store.set_next_batch competing "competing";
  (match Matrix_client.Store.flush competing with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "competing flush failed: %s"
        (Matrix_client.Error.to_string error));
  let before = Matrix_eio.Sync_service.state restored in
  let expected = before in
  let failed =
    try
      ignore
        (Matrix_eio.Sync_service.set_local_unread_counts_if_current restored
           ~room_id:the_room ~expected
           {
             Matrix_client.Read_state.unread = 6;
             notifications = 5;
             highlights = 4;
           });
      false
    with Eio.Io _ -> true
  in
  Alcotest.(check bool) "room-only flush conflict raises" true failed;
  Alcotest.(check int)
    "failed room update leaves service state unchanged" 4
    (Option.get
       (Matrix_client.Base_client.find_room
          (Matrix_eio.Sync_service.state restored)
          the_room))
      .local_unread_count;
  let after_failure = Matrix_client.Store.on_disk ~dir in
  Alcotest.(check int)
    "failed room update leaves disk value unchanged" 4
    (Option.get (Matrix_client.Store.find_room after_failure the_room))
      .local_unread_count

let test_runtime_stop_before_start () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    runtime_client ~sw ~env (fun request -> Fetch_mock.respond "{}" request)
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ~send_queue:(queue_for alice) ()
  in
  Ui.Runtime.stop runtime;
  Ui.Runtime.start runtime;
  Ui.Runtime.stop runtime;
  Alcotest.(check bool)
    "stopping the restarted runtime is observable" true
    (Ui.Observable.Value.get (Ui.Runtime.sync_state runtime)
    = Ui.Runtime.Stopped)

let raw_ids events =
  List.map
    (fun (event : Event.Raw_event.t) ->
      Option.value
        (Option.map Matrix_proto.Id.Event_id.to_string event.event_id)
        ~default:"<missing-id>")
    events

let event_json event =
  Result.get_ok (Jsont_bytesrw.encode_string Event.Raw_event.jsont event)

let focused_event_json ?(ts = 1L) id body =
  event_json
    (message ~id ~ts (Printf.sprintf {|{"msgtype":"m.text","body":"%s"}|} body))

let focused_context () =
  Printf.sprintf
    {|{"event":%s,"events_before":[%s,%s],"events_after":[%s,%s],"start":"older","end":"newer","state":[]}|}
    (focused_event_json ~ts:3L "$target" "target")
    (focused_event_json ~ts:2L "$before2" "before2")
    (focused_event_json ~ts:1L "$before1" "before1")
    (focused_event_json ~ts:4L "$after1" "after1")
    (focused_event_json ~ts:5L "$after2" "after2")

let focused_thread_context () =
  Printf.sprintf
    {|{"event":%s,"events_before":[%s],"events_after":[%s],"start":"older","end":"newer","state":[]}|}
    (event_json
       (message ~id:"$reply" ~ts:3L
          {|{"msgtype":"m.text","body":"reply","m.relates_to":{"rel_type":"m.thread","event_id":"$root"}}|}))
    (focused_event_json ~ts:2L "$other" "other")
    (focused_event_json ~ts:4L "$other2" "other2")

let test_event_focused_thread_pagination () =
  Eio_main.run @@ fun _env ->
  let requests = ref [] in
  let root_failures = ref 1 in
  let request_number = ref 0 in
  let view =
    Ui.Event_focused.create
      ~client:
        (client_over (fun request ->
             let url = Fetch.Middleware.Url.to_string request.url in
             requests := url :: !requests;
             incr request_number;
             match !request_number with
             | 1 -> Fetch_mock.respond (focused_thread_context ()) request
             | 2 | 4 ->
                 Fetch_mock.respond
                   {|{"chunk":[{"event_id":"$reply2","sender":"@alice:example.org","origin_server_ts":5,"type":"m.room.message","content":{"msgtype":"m.text","body":"reply2","m.relates_to":{"rel_type":"m.thread","event_id":"$root"}}},{"event_id":"$reply1","sender":"@alice:example.org","origin_server_ts":4,"type":"m.room.message","content":{"msgtype":"m.text","body":"reply1","m.relates_to":{"rel_type":"m.thread","event_id":"$root"}}}]}|}
                   request
             | 3 ->
                 if !root_failures > 0 then (
                   root_failures := 0;
                   Fetch_mock.respond ~status:503 "temporary" request)
                 else Alcotest.fail "root failure was not retried"
             | 5 ->
                 Fetch_mock.respond
                   {|{"event_id":"$root","sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.message","content":{"msgtype":"m.text","body":"root"}}|}
                   request
             | 6 ->
                 Fetch_mock.respond
                   {|{"chunk":[{"event_id":"$reply","sender":"@alice:example.org","origin_server_ts":3,"type":"m.room.message","content":{"msgtype":"m.text","body":"reply","m.relates_to":{"rel_type":"m.thread","event_id":"$root"}}},{"event_id":"$reply3","sender":"@alice:example.org","origin_server_ts":6,"type":"m.room.message","content":{"msgtype":"m.text","body":"reply3","m.relates_to":{"rel_type":"m.thread","event_id":"$root"}}}]}|}
                   request
             | _ -> Alcotest.failf "unexpected thread request: %s" url))
      ~room_id:the_room ~event_id:(event_id "$reply") ()
  in
  let started = Result.get_ok (Ui.Event_focused.start view ()) in
  Alcotest.(check (list string))
    "automatic context keeps only the thread" [ "$reply" ]
    (raw_ids started.events);
  Alcotest.(check bool)
    "thread context has a backward gap" true started.has_previous;
  let failed = Ui.Event_focused.paginate_backward view () in
  (match failed with
  | Error (Ui.Event_focused.Client_error _) -> ()
  | Error error ->
      Alcotest.failf "unexpected first failure: %a" Ui.Event_focused.pp_error
        error
  | Ok _ ->
      Alcotest.failf "expected terminal root fetch to fail (requests=%d)"
        !request_number);
  Alcotest.(check int) "relation and root were both attempted" 3 !request_number;
  Alcotest.(check (list string))
    "failed root fetch leaves content unchanged" [ "$reply" ]
    (raw_ids (Array.to_list (Ui.Event_focused.snapshot view)));
  let page =
    match Ui.Event_focused.paginate_backward view () with
    | Ok page -> page
    | Error error ->
        Alcotest.failf "retry failed (%d): %a" !request_number
          Ui.Event_focused.pp_error error
  in
  Alcotest.(check (list string))
    "thread backward page is chronological"
    [ "$root"; "$reply1"; "$reply2" ]
    (raw_ids page.events);
  Alcotest.(check (list string))
    "root is included once"
    [ "$root"; "$reply1"; "$reply2"; "$reply" ]
    (raw_ids (Array.to_list (Ui.Event_focused.snapshot view)));
  let forward = Result.get_ok (Ui.Event_focused.paginate_forward view ()) in
  Alcotest.(check (list string))
    "forward thread page is chronological and deduped" [ "$reply3" ]
    (raw_ids forward.events);
  let urls = List.rev !requests in
  let relation_urls =
    List.filter
      (fun url ->
        has_substring ~needle:"/relations/$root" url
        && has_substring ~needle:"dir=b" url)
      urls
  in
  let root_urls = List.filter (has_substring ~needle:"/event/$root") urls in
  Alcotest.(check (list string))
    "relations request is retried verbatim"
    [
      "https://hs.example/_matrix/client/v1/rooms/!room:example.org/relations/$root?limit=30&from=older&dir=b&recurse=true";
      "https://hs.example/_matrix/client/v1/rooms/!room:example.org/relations/$root?limit=30&from=older&dir=b&recurse=true";
    ]
    relation_urls;
  Alcotest.(check (list string))
    "terminal root request is exact"
    [
      "https://hs.example/_matrix/client/v3/rooms/!room:example.org/event/$root";
      "https://hs.example/_matrix/client/v3/rooms/!room:example.org/event/$root";
    ]
    root_urls;
  let forward_urls = List.filter (has_substring ~needle:"dir=f") urls in
  Alcotest.(check (list string))
    "forward relation request is exact"
    [
      "https://hs.example/_matrix/client/v1/rooms/!room:example.org/relations/$root?limit=30&from=newer&dir=f&recurse=true";
    ]
    forward_urls;
  Alcotest.(check bool)
    "relations endpoint and parameters are used" true
    (List.exists
       (fun url ->
         has_substring ~needle:"/relations/$root" url
         && has_substring ~needle:"dir=b" url
         && has_substring ~needle:"from=older" url
         && has_substring ~needle:"recurse=true" url
         && has_substring ~needle:"limit=30" url)
       urls)

let test_event_focused_force_thread_root () =
  Eio_main.run @@ fun _env ->
  let requests = ref [] in
  let view =
    let request_number = ref 0 in
    Ui.Event_focused.create ~thread_mode:Ui.Event_focused.Force
      ~client:
        (client_over (fun request ->
             let url = Fetch.Middleware.Url.to_string request.url in
             requests := url :: !requests;
             incr request_number;
             match !request_number with
             | 1 -> Fetch_mock.respond (focused_thread_context ()) request
             | 2 ->
                 Fetch_mock.respond {|{"chunk":[],"next_batch":"next"}|} request
             | _ -> Alcotest.failf "unexpected forced-thread request: %s" url))
      ~room_id:the_room ~event_id:(event_id "$reply") ()
  in
  let started = Result.get_ok (Ui.Event_focused.start view ()) in
  Alcotest.(check (list string))
    "force keeps thread context" [ "$reply" ] (raw_ids started.events);
  ignore (Result.get_ok (Ui.Event_focused.paginate_forward view ()));
  Alcotest.(check bool)
    "force uses the parsed thread root" true
    (List.exists (has_substring ~needle:"/relations/$root") !requests);
  Alcotest.(check bool)
    "force forwards with relations parameters" true
    (List.exists
       (fun url ->
         has_substring
           ~needle:"/relations/$root?limit=30&from=newer&dir=f&recurse=true" url)
       !requests)

let test_event_focused_target_validation_and_id_filter () =
  Eio_main.run @@ fun _env ->
  let requests = ref 0 in
  let view =
    Ui.Event_focused.create
      ~client:
        (client_over (fun request ->
             incr requests;
             Fetch_mock.respond
               {|{"event":{"event_id":"$wrong","sender":"@alice:example.org","origin_server_ts":1,"type":"m.room.message","content":{}},"events_before":[],"events_after":[],"start":"older","end":"newer","state":[]}|}
               request))
      ~room_id:the_room ~event_id:(event_id "$target") ()
  in
  (match Ui.Event_focused.start view () with
  | Error (Ui.Event_focused.Event_not_found id) ->
      Alcotest.(check string)
        "reported requested id" "$target"
        (Matrix_proto.Id.Event_id.to_string id)
  | _ -> Alcotest.fail "mismatched context target was accepted");
  Alcotest.(check int) "mismatched target is not retried implicitly" 1 !requests

let test_event_focused_automatic_room_filter () =
  Eio_main.run @@ fun _env ->
  let requests = ref 0 in
  let threaded id ts root =
    event_json
      (message ~id ~ts
         (Printf.sprintf
            {|{"msgtype":"m.text","body":"thread","m.relates_to":{"rel_type":"m.thread","event_id":"%s"}}|}
            root))
  in
  let context =
    Printf.sprintf
      {|{"event":%s,"events_before":[%s],"events_after":[%s],"start":"older","end":"newer","state":[]}|}
      (focused_event_json ~ts:3L "$target" "target")
      (threaded "$thread-before" 2L "$root")
      (focused_event_json ~ts:4L "$ordinary-after" "ordinary")
  in
  let view =
    Ui.Event_focused.create
      ~client:
        (client_over (fun request ->
             incr requests;
             match !requests with
             | 1 -> Fetch_mock.respond context request
             | 2 ->
                 Fetch_mock.respond
                   (Printf.sprintf
                      {|{"start":"older","chunk":[%s,%s],"state":[]}|}
                      (threaded "$thread-older" 1L "$root")
                      (focused_event_json ~ts:0L "$ordinary-older" "ordinary"))
                   request
             | _ -> Alcotest.fail "automatic room requested an unexpected page"))
      ~room_id:the_room ~event_id:(event_id "$target") ()
  in
  let started = Result.get_ok (Ui.Event_focused.start view ()) in
  Alcotest.(check (list string))
    "automatic room hides threaded context events"
    [ "$target"; "$ordinary-after" ]
    (raw_ids started.events);
  let page = Result.get_ok (Ui.Event_focused.paginate_backward view ()) in
  Alcotest.(check (list string))
    "automatic room hides threaded page events" [ "$ordinary-older" ]
    (raw_ids page.events)

let test_event_focused_ordering_and_edges () =
  Eio_main.run @@ fun _env ->
  let requests = ref 0 in
  let view =
    Ui.Event_focused.create
      ~client:
        (client_over (fun request ->
             incr requests;
             let body =
               match !requests with
               | 1 -> focused_context ()
               | 2 ->
                   Printf.sprintf
                     {|{"start":"older","chunk":[%s,%s,%s],"state":[]}|}
                     (focused_event_json ~ts:2L "$before2" "before2")
                     (focused_event_json ~ts:0L "$before0" "before0")
                     (focused_event_json ~ts:(-1L) "$before-1" "before-1")
               | 3 ->
                   Printf.sprintf
                     {|{"start":"newer","chunk":[%s,%s],"state":[]}|}
                     (focused_event_json ~ts:4L "$after1" "after1")
                     (focused_event_json ~ts:6L "$after3" "after3")
               | _ ->
                   Alcotest.fail "focused view paginated past a terminal edge"
             in
             Fetch_mock.respond body request))
      ~room_id:the_room ~event_id:(event_id "$target") ()
  in
  let started = Result.get_ok (Ui.Event_focused.start view ()) in
  Alcotest.(check (list string))
    "context is chronological"
    [ "$before1"; "$before2"; "$target"; "$after1"; "$after2" ]
    (raw_ids started.events);
  Alcotest.(check (list string))
    "observable starts in context order"
    [ "$before1"; "$before2"; "$target"; "$after1"; "$after2" ]
    (raw_ids (Array.to_list (Ui.Event_focused.snapshot view)));
  let older =
    match Ui.Event_focused.paginate_backward view () with
    | Ok page -> page
    | Error error ->
        Alcotest.failf "backward failed: %a" Ui.Event_focused.pp_error error
  in
  Alcotest.(check (list string))
    "backward page is chronological"
    [ "$before-1"; "$before0" ]
    (raw_ids older.events);
  Alcotest.(check (list string))
    "backward page prepends"
    [
      "$before-1";
      "$before0";
      "$before1";
      "$before2";
      "$target";
      "$after1";
      "$after2";
    ]
    (raw_ids (Array.to_list (Ui.Event_focused.snapshot view)));
  let newer = Result.get_ok (Ui.Event_focused.paginate_forward view ()) in
  Alcotest.(check (list string))
    "forward overlap is deduplicated" [ "$after3" ] (raw_ids newer.events);
  Alcotest.(check (list string))
    "forward page appends"
    [
      "$before-1";
      "$before0";
      "$before1";
      "$before2";
      "$target";
      "$after1";
      "$after2";
      "$after3";
    ]
    (raw_ids (Array.to_list (Ui.Event_focused.snapshot view)));
  ignore (Result.get_ok (Ui.Event_focused.paginate_backward view ()));
  ignore (Result.get_ok (Ui.Event_focused.paginate_forward view ()));
  Alcotest.(check int) "terminal edges do not request again" 3 !requests

let test_event_focused_shared_cache () =
  Eio_main.run @@ fun _env ->
  let store = Ui.Event_store.memory () in
  let cache = Ui.Event_cache.create ~store () in
  let requests = ref 0 in
  let view =
    Ui.Event_focused.create ~event_cache:cache
      ~client:
        (client_over (fun request ->
             incr requests;
             let body =
               match !requests with
               | 1 -> focused_context ()
               | 2 ->
                   Printf.sprintf
                     {|{"start":"older","end":"older-next","chunk":[%s]}|}
                     (focused_event_json ~ts:0L "$older" "older")
               | 3 ->
                   Printf.sprintf {|{"start":"newer","chunk":[%s]}|}
                     (focused_event_json ~ts:7L "$newer" "newer")
               | _ ->
                   Alcotest.fail
                     "shared-cache focused view made an extra request"
             in
             Fetch_mock.respond body request))
      ~room_id:the_room ~event_id:(event_id "$target") ()
  in
  ignore (Result.get_ok (Ui.Event_focused.start view ()));
  ignore (Result.get_ok (Ui.Event_focused.paginate_backward view ()));
  ignore (Result.get_ok (Ui.Event_focused.paginate_forward view ()));
  List.iter
    (fun id ->
      Alcotest.(check bool)
        ("focused event is in shared cache: " ^ id)
        true
        (Option.is_some
           (Ui.Event_cache.find_event cache the_room (event_id id))))
    [ "$before1"; "$target"; "$after2"; "$older"; "$newer" ];
  (* A second cache over the same store sees detached focused events without
     needing a timeline projection or another HTTP request. *)
  let cold = Ui.Event_cache.create ~store () in
  Alcotest.(check bool)
    "detached event survives cache reload" true
    (Option.is_some
       (Ui.Event_cache.find_event cold the_room (event_id "$older")));
  Ui.Event_focused.close view;
  (* A completed request racing with close must not repopulate the cache. *)
  let closed_cache = Ui.Event_cache.create () in
  let view_ref = ref None in
  let closed_view =
    Ui.Event_focused.create ~event_cache:closed_cache
      ~client:
        (client_over (fun request ->
             Option.iter Ui.Event_focused.close !view_ref;
             Fetch_mock.respond (focused_context ()) request))
      ~room_id:the_room ~event_id:(event_id "$target") ()
  in
  view_ref := Some closed_view;
  ignore (Result.get_ok (Ui.Event_focused.start closed_view ()));
  Alcotest.(check bool)
    "closed focused request does not register" false
    (Option.is_some
       (Ui.Event_cache.find_event closed_cache the_room (event_id "$target")))

let test_event_focused_forget_closes_view () =
  Eio_main.run @@ fun _env ->
  let cache = Ui.Event_cache.create () in
  let requests = ref 0 in
  let view =
    Ui.Event_focused.create ~event_cache:cache
      ~client:
        (client_over (fun request ->
             incr requests;
             Fetch_mock.respond (focused_context ()) request))
      ~room_id:the_room ~event_id:(event_id "$target") ()
  in
  let transitions = ref [] in
  let _unsubscribe =
    Ui.Event_focused.subscribe view (fun state ->
        transitions := state :: !transitions)
  in
  ignore (Result.get_ok (Ui.Event_focused.start view ()));
  Alcotest.(check int) "focused view fetched once" 1 !requests;
  Ui.Event_cache.forget_room cache the_room;
  Alcotest.(check bool)
    "forget closes focused view" true
    (Ui.Event_focused.state view = Ui.Event_focused.Closed);
  Alcotest.(check int)
    "forget clears focused events" 0
    (Array.length (Ui.Event_focused.snapshot view));
  Alcotest.(check bool)
    "forget publishes closed" true
    (List.mem Ui.Event_focused.Closed !transitions);
  ignore (Result.get_ok (Ui.Event_focused.start view ()));
  Alcotest.(check int) "closed focused view does not refetch" 1 !requests

let test_event_focused_failure_reset_and_close () =
  Eio_main.run @@ fun _env ->
  let attempts = ref 0 in
  let view =
    Ui.Event_focused.create
      ~client:
        (client_over (fun request ->
             incr attempts;
             match !attempts with
             | 1 -> Fetch_mock.respond (focused_context ()) request
             | 2 -> Fetch_mock.respond ~status:503 "offline" request
             | 3 -> Fetch_mock.respond (focused_context ()) request
             | _ -> Alcotest.fail "closed focused view made a request"))
      ~room_id:the_room ~event_id:(event_id "$target") ()
  in
  ignore (Result.get_ok (Ui.Event_focused.start view ()));
  let before = Ui.Event_focused.snapshot view in
  (match Ui.Event_focused.paginate_backward view () with
  | Error (Ui.Event_focused.Client_error _) -> ()
  | _ -> Alcotest.fail "expected pagination failure");
  Alcotest.(check int)
    "failed page keeps content" (Array.length before)
    (Array.length (Ui.Event_focused.snapshot view));
  ignore (Result.get_ok (Ui.Event_focused.reset view));
  Alcotest.(check int)
    "reset clears content" 0
    (Array.length (Ui.Event_focused.snapshot view));
  ignore (Result.get_ok (Ui.Event_focused.start view ()));
  Ui.Event_focused.close view;
  Alcotest.(check int)
    "close clears content" 0
    (Array.length (Ui.Event_focused.snapshot view));
  ignore (Result.get_ok (Ui.Event_focused.reset view));
  ignore (Result.get_ok (Ui.Event_focused.paginate_forward view ()));
  Alcotest.(check int) "closed operations do not request" 3 !attempts

let test_event_focused_close_during_request () =
  Eio_main.run @@ fun _env ->
  let view_ref = ref None in
  let rejected = ref false in
  let view =
    Ui.Event_focused.create
      ~client:
        (client_over (fun request ->
             (match !view_ref with
             | None -> Alcotest.fail "view was not installed before request"
             | Some view ->
                 (match Ui.Event_focused.paginate_forward view () with
                 | Error
                     (Ui.Event_focused.Invalid_state
                        {
                          expected = Ui.Event_focused.Idle;
                          actual = Ui.Event_focused.Fetching_target;
                        }) ->
                     rejected := true
                 | _ -> Alcotest.fail "reentrant pagination was not rejected");
                 Ui.Event_focused.close view);
             Fetch_mock.respond (focused_context ()) request))
      ~room_id:the_room ~event_id:(event_id "$target") ()
  in
  view_ref := Some view;
  let result = Result.get_ok (Ui.Event_focused.start view ()) in
  Alcotest.(check bool) "reentrant operation is rejected" true !rejected;
  Alcotest.(check int)
    "close prevents in-flight repopulation" 0
    (List.length result.events);
  Alcotest.(check bool)
    "closed state is published" true
    (Ui.Event_focused.state view = Ui.Event_focused.Closed);
  Alcotest.(check int)
    "closed view remains empty" 0
    (Array.length (Ui.Event_focused.snapshot view))

let test_event_focused_failed_start_retries () =
  Eio_main.run @@ fun _env ->
  let attempts = ref 0 in
  let view =
    Ui.Event_focused.create
      ~client:
        (client_over (fun request ->
             incr attempts;
             if !attempts = 1 then
               Fetch_mock.respond ~status:503 "offline" request
             else Fetch_mock.respond (focused_context ()) request))
      ~room_id:the_room ~event_id:(event_id "$target") ()
  in
  (match Ui.Event_focused.start view () with
  | Error (Ui.Event_focused.Client_error _) -> ()
  | _ -> Alcotest.fail "expected the initial context request to fail");
  Alcotest.(check bool)
    "failed start returns to initial" true
    (Ui.Event_focused.state view = Ui.Event_focused.Initial);
  let result = Result.get_ok (Ui.Event_focused.start view ()) in
  Alcotest.(check int) "retry loads context" 5 (List.length result.events);
  Alcotest.(check bool)
    "successful retry is idle" true
    (Ui.Event_focused.state view = Ui.Event_focused.Idle)

let test_event_focused_callback_isolation () =
  Eio_main.run @@ fun _env ->
  let view =
    Ui.Event_focused.create
      ~client:
        (client_over (fun request ->
             Fetch_mock.respond (focused_context ()) request))
      ~room_id:the_room ~event_id:(event_id "$target") ()
  in
  let other_calls = ref [] in
  let _unsubscribe_raising =
    Ui.Event_focused.subscribe view (fun state ->
        if state = Ui.Event_focused.Fetching_target then
          failwith "raising focused listener")
  in
  let unsubscribe_other =
    Ui.Event_focused.subscribe view (fun state ->
        other_calls := state :: !other_calls)
  in
  ignore (Result.get_ok (Ui.Event_focused.start view ()));
  unsubscribe_other ();
  Alcotest.(check bool)
    "raising listener does not wedge transition" true
    (Ui.Event_focused.state view = Ui.Event_focused.Idle);
  Alcotest.(check bool)
    "other listeners still run" true
    (List.mem Ui.Event_focused.Fetching_target !other_calls
    && List.mem Ui.Event_focused.Idle !other_calls);
  let initial_raises = ref false in
  let callback =
   fun _ ->
    initial_raises := true;
    failwith "initial listener"
  in
  (try
     ignore ((Ui.Event_focused.subscribe view callback) ());
     Alcotest.fail "initial callback exception was swallowed"
   with Failure _ -> ());
  Alcotest.(check bool) "initial callback ran" true !initial_raises;
  (* The failed immediate subscription must not be retained. *)
  ignore (Result.get_ok (Ui.Event_focused.reset view));
  ignore (Result.get_ok (Ui.Event_focused.start view ()))

(* Each page is answered by the mock in turn; the second carries no [end]
   token, which is how a homeserver says the room has no more history. *)
let test_pagination_outcome () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pages =
    ref [ ("$older", "older", Some "p2"); ("$oldest", "oldest", None) ]
  in
  let handler (request : Fetch.Middleware.request) =
    match !pages with
    | (id, body, end_) :: rest ->
        pages := rest;
        Fetch_mock.respond (messages_page ~id ~body ~end_) request
    | [] -> Alcotest.fail "the timeline paginated past the room's beginning"
  in
  let client = client_over handler in
  let cache = Ui.Event_cache.create () in
  let _ =
    fold_sync cache
      [
        sync_response ~limited:true ~batch:"s1" ~prev_batch:"p1"
          [ ("$new", "new") ];
      ]
  in
  let timeline =
    Ui.Room_timeline.create ~sw ~client ~send_queue:(queue_for alice) cache
      the_room
  in
  let bodies () =
    Ui.Room_timeline.snapshot timeline
    |> Array.to_list
    |> List.filter_map (function
      | Ui.Room_timeline.Event item -> Ui.Presentation.preview item.event
      | Ui.Room_timeline.Virtual _ -> None)
  in
  let at_start () =
    Array.exists
      (function
        | Ui.Room_timeline.Virtual
            { content = Ui.Room_timeline.Timeline_start; _ } ->
            true
        | _ -> false)
      (Ui.Room_timeline.snapshot timeline)
  in
  Alcotest.(check bool)
    "a page with a token behind it" true
    (Ui.Room_timeline.paginate_back timeline ~limit:10 () = Ok `More);
  Alcotest.(check (list string))
    "and the items show it before the call returns" [ "older"; "new" ]
    (bodies ());
  Alcotest.(check bool)
    "no start marker while a token is left" false (at_start ());
  Alcotest.(check bool)
    "the page that reaches the beginning says so" true
    (Ui.Room_timeline.paginate_back timeline ~limit:10 () = Ok `Reached_start);
  Alcotest.(check (list string))
    "with the whole history showing"
    [ "oldest"; "older"; "new" ]
    (bodies ());
  Alcotest.(check bool) "and the start marker in place" true (at_start ());
  Alcotest.(check bool)
    "a further page has nothing to fetch" true
    (Ui.Room_timeline.paginate_back timeline ~limit:10 () = Ok `Nothing_to_do)

(* [Runtime.join] and [Runtime.leave] are the endpoints under the runtime's
   own client, answering rather than raising. *)
let test_runtime_membership () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let urls = ref [] in
  let forbid = ref false in
  let handler (request : Fetch.Middleware.request) =
    let url = Fetch.Middleware.Url.to_string request.url in
    urls := url :: !urls;
    if !forbid then
      Fetch_mock.respond ~status:403
        {|{"errcode":"M_FORBIDDEN","error":"You are not invited"}|} request
    else if has_substring ~needle:"/join/" url then
      Fetch_mock.respond {|{"room_id":"!room:example.org"}|} request
    else Fetch_mock.respond "{}" request
  in
  let client = mock_client ~sw ~env handler in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ~send_queue:(queue_for alice) ()
  in
  Alcotest.(check bool)
    "the join succeeds" true
    (match Ui.Runtime.join runtime the_room with
    | Ok () -> true
    | Error _ -> false);
  Alcotest.(check bool)
    "against the join endpoint" true
    (List.exists (has_substring ~needle:"/_matrix/client/v3/join/") !urls);
  Alcotest.(check bool)
    "the leave succeeds" true
    (match Ui.Runtime.leave runtime the_room with
    | Ok () -> true
    | Error _ -> false);
  Alcotest.(check bool)
    "against that room's leave endpoint" true
    (List.exists (has_substring ~needle:"/leave") !urls);
  forbid := true;
  Alcotest.(check bool)
    "and a refused join comes back as a value, not an exception" true
    (match Ui.Runtime.join runtime the_room with
    | Ok () -> false
    | Error (Matrix_client.Error.Matrix_error { errcode; _ }) ->
        errcode = Matrix_client.Error.M_FORBIDDEN
    | Error _ -> false)

let test_runtime_forget_cleans_local_state () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let urls = ref [] in
  let handler (request : Fetch.Middleware.request) =
    urls := Fetch.Middleware.Url.to_string request.url :: !urls;
    Fetch_mock.respond "{}" request
  in
  let client = runtime_client ~sw ~env handler in
  let state_store = Matrix_client.Store.memory () in
  let initial, _ =
    Matrix_client.Base_client.apply
      (Matrix_client.Base_client.create ~user_id:alice ())
      (sync_response ~batch:"initial" ~prev_batch:"initial-prev"
         [ ("$initial", "initial") ])
  in
  Matrix_client.Base_client.persist state_store initial;
  Matrix_client.Store.set_receipts state_store the_room
    Matrix_client.Read_state.empty;
  let sync =
    Matrix_eio.Sync_service.of_store ~store:state_store ~user_id:alice ()
  in
  ignore
    (Matrix_client.Thread_subscriptions.upsert state_store ~room_id:the_room
       ~thread_root:(event_id "$root")
       { status = Matrix_client.Thread_subscriptions.Manual; bump_stamp = None });
  let event_store = Ui.Event_store.memory () in
  let queue = queue_for alice in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client ~sync
      ~event_store ~send_queue:queue ()
  in
  Alcotest.(check int)
    "room list starts populated" 1
    (Array.length
       (Ui.Observable.List.snapshot
          (Ui.Room_list.all_rooms (Ui.Runtime.room_list runtime))));
  Ui.Event_cache.prepend
    (Ui.Runtime.event_cache runtime)
    the_room
    ~events:[ message ~id:"$cached" {|{"body":"cached"}|} ]
    ~prev_batch:None;
  ignore
    (Matrix_client.Send_queue.send_text queue ~room_id:the_room ~body:"queued");
  ignore (Ui.Runtime.timeline runtime the_room);
  (* The synthetic state has no room summary; the persisted thread subscription
     and cache still exercise all local cleanup paths. *)
  match Ui.Runtime.forget runtime the_room with
  | Error error -> Alcotest.fail (Matrix_client.Error.to_string error)
  | Ok () ->
      Alcotest.(check bool)
        "forget endpoint was used" true
        (List.exists (has_substring ~needle:"/forget") !urls);
      Alcotest.(check int)
        "cache is empty" 0
        (Array.length
           (Ui.Event_cache.snapshot (Ui.Runtime.event_cache runtime) the_room));
      Alcotest.(check int)
        "queued room sends are cancelled" 0
        (List.length (Matrix_client.Send_queue.room_requests queue the_room));
      Alcotest.(check int)
        "thread subscriptions are removed" 0
        (List.length
           (Result.get_ok
              (Matrix_client.Thread_subscriptions.subscriptions state_store)));
      Alcotest.(check bool)
        "base state has no forgotten room" true
        (Option.is_none
           (Matrix_client.Base_client.find_room
              (Matrix_eio.Sync_service.state sync)
              the_room));
      Alcotest.(check int)
        "room-list projection is refreshed" 0
        (Array.length
           (Ui.Observable.List.snapshot
              (Ui.Room_list.all_rooms (Ui.Runtime.room_list runtime))));
      Alcotest.(check bool)
        "base store has no forgotten room" true
        (Option.is_none (Matrix_client.Store.find_room state_store the_room));
      Alcotest.(check bool)
        "base store has no forgotten receipts" true
        (Option.is_none (Matrix_client.Store.receipts state_store the_room));
      Alcotest.(check bool)
        "event store has no forgotten room" true
        (Option.is_none
           (Result.get_ok (Ui.Event_store.load_room event_store the_room)))

let test_runtime_forget_clears_open_timeline () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    runtime_client ~sw ~env (fun request -> Fetch_mock.respond "{}" request)
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ~send_queue:(queue_for alice) ()
  in
  Ui.Event_cache.prepend
    (Ui.Runtime.event_cache runtime)
    the_room
    ~events:[ message ~id:"$open-forget" {|{"body":"open"}|} ]
    ~prev_batch:None;
  let timeline = Ui.Runtime.timeline runtime the_room in
  Alcotest.(check bool)
    "open timeline starts populated" true
    (Array.length (Ui.Room_timeline.snapshot timeline) > 0);
  (match Ui.Runtime.forget runtime the_room with
  | Error error -> Alcotest.fail (Matrix_client.Error.to_string error)
  | Ok () -> ());
  Alcotest.(check int)
    "forget synchronously clears the open timeline" 0
    (Array.length (Ui.Room_timeline.snapshot timeline));
  let replacement = Ui.Runtime.timeline runtime the_room in
  Alcotest.(check bool)
    "forget causes the next timeline call to make a fresh handle" false
    (timeline == replacement);
  Alcotest.(check bool)
    "the replacement timeline contains no forgotten room data" true
    (match Array.to_list (Ui.Room_timeline.snapshot replacement) with
    | [ Ui.Room_timeline.Virtual { content = Timeline_start; _ } ] -> true
    | _ -> false)

let test_runtime_forget_server_failure_preserves_local_state () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client =
    runtime_client ~sw ~env (fun request ->
        Fetch_mock.respond ~status:403
          {|{"errcode":"M_FORBIDDEN","error":"not forgotten"}|} request)
  in
  let event_store = Ui.Event_store.memory () in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client
      ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
      ~event_store ~send_queue:(queue_for alice) ()
  in
  Ui.Event_cache.prepend
    (Ui.Runtime.event_cache runtime)
    the_room
    ~events:[ message ~id:"$retained" {|{"body":"retain"}|} ]
    ~prev_batch:None;
  let result = Ui.Runtime.forget runtime the_room in
  Alcotest.(check bool)
    "server failure is returned" true
    (match result with
    | Error
        (Matrix_client.Error.Matrix_error
           { errcode = Matrix_client.Error.M_FORBIDDEN; _ }) ->
        true
    | Error _ | Ok () -> false);
  Alcotest.(check int)
    "cache survives server failure" 1
    (Array.length
       (Ui.Event_cache.snapshot (Ui.Runtime.event_cache runtime) the_room));
  Alcotest.(check bool)
    "store survives server failure" true
    (Option.is_some
       (Result.get_ok (Ui.Event_store.load_room event_store the_room)))

let test_runtime_forget_flushes_queue_store () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let path = Filename.temp_file "matrix-runtime-queue-store" ".d" in
  Unix.unlink path;
  Unix.mkdir path 0o700;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun name ->
          let file = Filename.concat path name in
          if Sys.file_exists file then Unix.unlink file)
        [ "base_state.json"; "base_state.json.tmp"; ".profile.lock" ];
      Unix.rmdir path)
    (fun () ->
      let dir = Eio.Path.(Eio.Stdenv.fs env / path) in
      let queue_store = Matrix_client.Store.on_disk ~dir in
      let queue =
        Matrix_client.Send_queue.create
          ~random:
            (Matrix_client.Random.of_source
               (Eio.Flow.string_source (String.make 4096 'd')))
          ~user_id:alice ~store:queue_store ()
      in
      ignore
        (Matrix_client.Send_queue.send_text queue ~room_id:the_room
           ~body:"must be deleted on disk");
      (match Matrix_client.Store.flush queue_store with
      | Ok () -> ()
      | Error error ->
          Alcotest.failf "initial queue flush failed: %s"
            (Matrix_client.Error.to_string error));
      let before = Matrix_client.Store.on_disk ~dir in
      let before_queue =
        Matrix_client.Send_queue.create
          ~random:
            (Matrix_client.Random.of_source
               (Eio.Flow.string_source (String.make 4096 'b')))
          ~user_id:alice ~store:before ()
      in
      Alcotest.(check bool)
        "queue request reaches disk before forget" false
        (Matrix_client.Send_queue.is_empty before_queue);
      let client =
        runtime_client ~sw ~env (fun request -> Fetch_mock.respond "{}" request)
      in
      let runtime =
        Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client
          ~sync:(Matrix_eio.Sync_service.of_user ~user_id:alice ())
          ~send_queue:queue ()
      in
      (match Ui.Runtime.forget runtime the_room with
      | Ok () -> ()
      | Error error -> Alcotest.fail (Matrix_client.Error.to_string error));
      let after = Matrix_client.Store.on_disk ~dir in
      let after_queue =
        Matrix_client.Send_queue.create
          ~random:
            (Matrix_client.Random.of_source
               (Eio.Flow.string_source (String.make 4096 'a')))
          ~user_id:alice ~store:after ()
      in
      Alcotest.(check bool)
        "queue deletion reaches disk" true
        (Matrix_client.Send_queue.is_empty after_queue))

let invited_sync () =
  let response =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        {|{"next_batch":"invite","rooms":{"invite":{"!room:example.org":{"invite_state":{"events":[{"type":"m.room.member","state_key":"@alice:example.org","sender":"@bob:example.org","content":{"membership":"invite"}}]}}}}}|}
    with
    | Ok response -> response
    | Error error -> Alcotest.failf "bad invite response: %s" error
  in
  let state = Matrix_client.Base_client.create ~user_id:alice () in
  let state, _ = Matrix_client.Base_client.apply state response in
  Matrix_eio.Sync_service.create state

let invite_encryption () =
  Matrix_eio.Encryption.create
    ~random:
      (Matrix_client.Random.of_source
         (Eio.Flow.string_source (String.make 400_000 'e')))
    ~user_id:alice
    ~device_id:(Matrix_proto.Id.Device_id.of_string_exn "ALICEDEV")
    ()

let test_runtime_join_invite_acceptance () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fail_join = ref false in
  let handler (request : Fetch.Middleware.request) =
    let url = Fetch.Middleware.Url.to_string request.url in
    if has_substring ~needle:"/join/" url then
      if !fail_join then
        Fetch_mock.respond ~status:403
          {|{"errcode":"M_FORBIDDEN","error":"You are not invited"}|} request
      else Fetch_mock.respond {|{"room_id":"!room:example.org"}|} request
    else Fetch_mock.respond "{}" request
  in
  let client = mock_client ~sw ~env handler in
  let sync = invited_sync () in
  let encryption = invite_encryption () in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client ~sync
      ~encryption ~send_queue:(queue_for alice) ()
  in
  let machine = Matrix_eio.Encryption.machine encryption in
  Alcotest.(check bool)
    "an invite starts without a pending acceptance" true
    (Matrix_client.Encryption.pending_key_bundle machine ~room_id:the_room
    = None);
  Alcotest.(check bool)
    "a successful invite join succeeds" true
    (match Ui.Runtime.join runtime the_room with
    | Ok () -> true
    | Error _ -> false);
  Alcotest.(check (option string))
    "the successful join records its inviter" (Some "@bob:example.org")
    (match
       Matrix_client.Encryption.pending_key_bundle machine ~room_id:the_room
     with
    | Some pending -> Some (Matrix_proto.Id.User_id.to_string pending.inviter)
    | None -> None);
  let sync_response json =
    match Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont json with
    | Ok response -> response
    | Error error -> Alcotest.failf "bad membership response: %s" error
  in
  ignore
    (Matrix_eio.Sync_service.apply ~encryption client sync
       (sync_response
          {|{"next_batch":"joined","rooms":{"join":{"!room:example.org":{}}}}|}));
  Alcotest.(check bool)
    "a joined sync retains the pending acceptance" true
    (Matrix_client.Encryption.pending_key_bundle machine ~room_id:the_room
    <> None);
  ignore
    (Matrix_eio.Sync_service.apply ~encryption client sync
       (sync_response
          {|{"next_batch":"left","rooms":{"leave":{"!room:example.org":{}}}}|}));
  Alcotest.(check bool)
    "a left sync clears the pending acceptance" true
    (Matrix_client.Encryption.pending_key_bundle machine ~room_id:the_room
    = None);
  Matrix_client.Encryption.record_invite_acceptance machine ~room_id:the_room
    ~inviter:(Matrix_proto.Id.User_id.of_string_exn "@bob:example.org");
  ignore
    (Matrix_eio.Sync_service.apply ~encryption client sync
       (sync_response {|{"next_batch":"empty"}|}));
  Alcotest.(check bool)
    "an empty incremental sync clears stale state already recorded as left" true
    (Matrix_client.Encryption.pending_key_bundle machine ~room_id:the_room
    = None);
  let failed_encryption = invite_encryption () in
  let failed_runtime =
    Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client
      ~sync:(invited_sync ()) ~encryption:failed_encryption
      ~send_queue:(queue_for alice) ()
  in
  fail_join := true;
  Alcotest.(check bool)
    "a refused invite join remains an error" true
    (match Ui.Runtime.join failed_runtime the_room with
    | Error (Matrix_client.Error.Matrix_error { errcode; _ }) ->
        errcode = Matrix_client.Error.M_FORBIDDEN
    | Error _ -> true
    | Ok () -> false);
  Alcotest.(check bool)
    "a refused join does not record acceptance" true
    (Matrix_client.Encryption.pending_key_bundle
       (Matrix_eio.Encryption.machine failed_encryption)
       ~room_id:the_room
    = None)

let test_runtime_join_alias () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let alias = Matrix_proto.Id.Room_alias.of_string_exn "#invite:example.org" in
  let joined_room =
    Matrix_proto.Id.Room_id.of_string_exn "!joined:example.org"
  in
  let make_runtime handler =
    let client = mock_client ~sw ~env handler in
    let encryption = invite_encryption () in
    let runtime =
      Ui.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) ~client
        ~sync:(invited_sync ()) ~encryption ~send_queue:(queue_for alice) ()
    in
    (runtime, encryption)
  in
  let success_requests = ref [] in
  let success_handler (request : Fetch.Middleware.request) =
    let url = Fetch.Middleware.Url.to_string request.url in
    success_requests := !success_requests @ [ url ];
    if has_substring ~needle:"/directory/room/" url then
      Fetch_mock.respond
        {|{"room_id":"!room:example.org","servers":["remote.example.org"]}|}
        request
    else if has_substring ~needle:"/join/" url then
      Fetch_mock.respond {|{"room_id":"!joined:example.org"}|} request
    else Fetch_mock.respond "{}" request
  in
  let runtime, encryption = make_runtime success_handler in
  let via = [ "first.example.org"; "second.example.org" ] in
  Alcotest.(check bool)
    "an alias join succeeds" true
    (match
       Ui.Runtime.join_room runtime ~room_id_or_alias:(`Room_alias alias) ~via
         ()
     with
    | Ok () -> true
    | Error _ -> false);
  (match !success_requests with
  | [ resolve; join ] ->
      Alcotest.(check string)
        "alias is resolved before join"
        "https://hs.example/_matrix/client/v3/directory/room/%23invite:example.org"
        resolve;
      Alcotest.(check string)
        "original alias and via are passed to join"
        "https://hs.example/_matrix/client/v3/join/%23invite:example.org?server_name=first.example.org&server_name=second.example.org"
        join
  | _ ->
      Alcotest.failf "expected resolve then join, got %d requests"
        (List.length !success_requests));
  let machine = Matrix_eio.Encryption.machine encryption in
  Alcotest.(check (option string))
    "successful alias join records the inviter" (Some "@bob:example.org")
    (match
       Matrix_client.Encryption.pending_key_bundle machine ~room_id:joined_room
     with
    | Some pending -> Some (Matrix_proto.Id.User_id.to_string pending.inviter)
    | None -> None);
  Alcotest.(check bool)
    "the returned room id, not the resolved id, owns the record" true
    (Matrix_client.Encryption.pending_key_bundle machine ~room_id:the_room
    = None);
  let failed_requests = ref [] in
  let failed_handler (request : Fetch.Middleware.request) =
    let url = Fetch.Middleware.Url.to_string request.url in
    failed_requests := !failed_requests @ [ url ];
    if has_substring ~needle:"/directory/room/" url then
      Fetch_mock.respond {|{"room_id":"!room:example.org","servers":[]}|}
        request
    else if has_substring ~needle:"/join/" url then
      Fetch_mock.respond ~status:403
        {|{"errcode":"M_FORBIDDEN","error":"You are not invited"}|} request
    else Fetch_mock.respond "{}" request
  in
  let failed_runtime, failed_encryption = make_runtime failed_handler in
  Alcotest.(check bool)
    "a failed alias join remains an error" true
    (match
       Ui.Runtime.join_room failed_runtime ~room_id_or_alias:(`Room_alias alias)
         ()
     with
    | Error _ -> true
    | Ok () -> false);
  Alcotest.(check int)
    "failed join still resolved the alias" 2
    (List.length !failed_requests);
  Alcotest.(check bool)
    "a failed alias join does not record acceptance" true
    (Matrix_client.Encryption.pending_key_bundle
       (Matrix_eio.Encryption.machine failed_encryption)
       ~room_id:the_room
    = None);
  let resolve_requests = ref [] in
  let resolve_handler (request : Fetch.Middleware.request) =
    let url = Fetch.Middleware.Url.to_string request.url in
    resolve_requests := !resolve_requests @ [ url ];
    if has_substring ~needle:"/directory/room/" url then
      Fetch_mock.respond ~status:404
        {|{"errcode":"M_NOT_FOUND","error":"Unknown alias"}|} request
    else Alcotest.fail "join was attempted after alias resolution failed"
  in
  let resolve_runtime, resolve_encryption = make_runtime resolve_handler in
  Alcotest.(check bool)
    "an alias resolution failure remains an error" true
    (match
       Ui.Runtime.join_room resolve_runtime
         ~room_id_or_alias:(`Room_alias alias) ()
     with
    | Error _ -> true
    | Ok () -> false);
  Alcotest.(check int)
    "resolution failure makes one request" 1
    (List.length !resolve_requests);
  Alcotest.(check bool)
    "resolution failure does not record acceptance" true
    (Matrix_client.Encryption.pending_key_bundle
       (Matrix_eio.Encryption.machine resolve_encryption)
       ~room_id:the_room
    = None)

let test_utd_hook_lifecycle () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let event = message ~id:"$utd" ~ts:1_700_000_000_000L {|{}|} in
  let store = Matrix_client.Store.memory () in
  let reports = ref [] in
  let device_created_at = Option.get (Ptime.of_float_s 1_699_999_999.) in
  let hook =
    Ui.Utd_hook.create ~sw ~clock ~store ~max_delay:0.02 ~device_created_at
      ~on_utd:(fun report -> reports := report :: !reports)
      ()
  in
  Ui.Utd_hook.on_utd hook ~event ~cause:Matrix_client.Encryption.Unknown
    ~user_trusts_own_identity:false ();
  Ui.Utd_hook.on_utd hook ~event ~cause:Matrix_client.Encryption.Unknown
    ~user_trusts_own_identity:false ();
  Alcotest.(check int) "grace period holds report" 0 (List.length !reports);
  Ui.Utd_hook.on_late_decrypt hook (event_id "$utd");
  Alcotest.(check int) "late report emitted once" 1 (List.length !reports);
  Alcotest.(check bool)
    "late duration recorded" true
    (Option.exists
       (fun duration -> duration >= 0.)
       (List.hd !reports).time_to_decrypt);
  Alcotest.(check (option int64))
    "event local age recorded" (Some 1000L)
    (List.hd !reports).event_local_age_millis;
  Ui.Utd_hook.on_late_decrypt hook (event_id "$utd");
  Alcotest.(check int) "late duplicate ignored" 1 (List.length !reports);
  let reloaded =
    Ui.Utd_hook.create ~sw ~clock ~store
      ~on_utd:(fun report -> reports := report :: !reports)
      ()
  in
  Ui.Utd_hook.on_utd reloaded ~event ~cause:Matrix_client.Encryption.Unknown
    ~user_trusts_own_identity:false ();
  Alcotest.(check int) "durable duplicate ignored" 1 (List.length !reports);
  let reentrant_calls = ref 0 in
  let reentrant_hook = ref None in
  let reentrant_event = message ~id:"$utd-reentrant" {|{}|} in
  let hook =
    Ui.Utd_hook.create ~sw ~clock
      ~on_utd:(fun _ ->
        incr reentrant_calls;
        Ui.Utd_hook.on_utd
          (Option.get !reentrant_hook)
          ~event:reentrant_event ~cause:Matrix_client.Encryption.Unknown
          ~user_trusts_own_identity:false ())
      ()
  in
  reentrant_hook := Some hook;
  Ui.Utd_hook.on_utd hook ~event:reentrant_event
    ~cause:Matrix_client.Encryption.Unknown ~user_trusts_own_identity:false ();
  Alcotest.(check int)
    "the callback runs outside the deduplication lock" 1 !reentrant_calls;
  let bounded_store = Matrix_client.Store.memory () in
  let bounded_reports = ref 0 in
  let bounded =
    Ui.Utd_hook.create ~sw ~clock ~store:bounded_store
      ~on_utd:(fun _ -> incr bounded_reports)
      ()
  in
  let ring_event i = message ~id:(Printf.sprintf "$utd-ring-%d" i) {|{}|} in
  for i = 0 to 4096 do
    Ui.Utd_hook.on_utd bounded ~event:(ring_event i)
      ~cause:Matrix_client.Encryption.Unknown ~user_trusts_own_identity:false ()
  done;
  Alcotest.(check int) "every distinct event reports" 4097 !bounded_reports;
  let bounded_reloaded =
    Ui.Utd_hook.create ~sw ~clock ~store:bounded_store
      ~on_utd:(fun _ -> incr bounded_reports)
      ()
  in
  Ui.Utd_hook.on_utd bounded_reloaded ~event:(ring_event 0)
    ~cause:Matrix_client.Encryption.Unknown ~user_trusts_own_identity:false ();
  Ui.Utd_hook.on_utd bounded_reloaded ~event:(ring_event 4096)
    ~cause:Matrix_client.Encryption.Unknown ~user_trusts_own_identity:false ();
  Alcotest.(check int)
    "the exact FIFO evicts the oldest and retains the newest" 4098
    !bounded_reports;
  let expired = ref [] in
  let expiry_hook =
    Ui.Utd_hook.create ~sw ~clock ~max_delay:0.01
      ~on_utd:(fun report -> expired := report :: !expired)
      ()
  in
  Ui.Utd_hook.on_utd expiry_hook ~event ~cause:Matrix_client.Encryption.Unknown
    ~user_trusts_own_identity:false ();
  Eio.Time.sleep clock 0.03;
  Alcotest.(check int) "expiry emits definite report" 1 (List.length !expired);
  Alcotest.(check bool)
    "expiry has no late duration" true
    (Option.is_none (List.hd !expired).time_to_decrypt)

let thread_root () =
  raw
    {|{"event_id":"$thread","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{"msgtype":"m.text","body":"root"},"unsigned":{"m.relations":{"m.thread":{"count":2,"latest_event":{"event_id":"$latest","sender":"@bob:example.org","origin_server_ts":1700000002000,"type":"m.room.message","content":{"msgtype":"m.text","body":"latest"}}}}}}|}

let thread_reply ?(root = "$thread") ?ts id body =
  message ~id ~sender:"@bob:example.org" ?ts
    (Printf.sprintf
       {|{"msgtype":"m.text","body":"%s","m.relates_to":{"rel_type":"m.thread","event_id":"%s"}}|}
       body root)

let test_thread_cache_durable_projection () =
  Eio_main.run @@ fun _env ->
  let store = Matrix_client.Store.memory () in
  let event_store = Ui.Event_store.memory () in
  let events = Ui.Event_cache.create ~store:event_store () in
  let threads = Ui.Thread_cache.create ~event_cache:events ~store () in
  let root = thread_root () in
  let reply = thread_reply ~ts:1_700_000_003_000L "$reply" "reply" in
  let earlier = thread_reply ~ts:1_700_000_001_000L "$earlier" "earlier" in
  Ui.Thread_cache.ingest threads ~room_id:the_room
    ~events:[ reply; root; earlier ];
  (match Ui.Thread_info.summary_of_root root with
  | Ui.Thread_info.Known _, Some latest ->
      Ui.Thread_cache.ingest_thread threads ~room_id:the_room
        ~root_id:(event_id "$thread") ~events:[ latest ]
  | _ -> Alcotest.fail "thread root did not expose its bundled latest reply");
  let view =
    Ui.Thread_cache.snapshot threads ~room_id:the_room
      ~root_id:(event_id "$thread")
  in
  (match view with
  | Some view ->
      Alcotest.(check int) "ordered reply count" 3 (List.length view.replies);
      Alcotest.(check bool)
        "root is cache-addressable" true (Option.is_some view.root);
      Alcotest.(check (list string))
        "root and replies are chronological"
        [ "$thread"; "$earlier"; "$latest"; "$reply" ]
        (List.filter_map
           (fun (event : Event.Raw_event.t) ->
             Option.map Matrix_proto.Id.Event_id.to_string event.event_id)
           view.events)
  | None -> Alcotest.fail "thread cache did not retain root");
  let changed = ref 0 in
  let unsubscribe =
    Ui.Thread_cache.subscribe_events ~room_id:the_room
      ~root_id:(event_id "$thread") threads (fun _ -> incr changed)
  in
  Alcotest.(check int)
    "scoped event subscription starts with snapshot" 1 !changed;
  Ui.Thread_cache.ingest_thread threads ~room_id:the_room
    ~root_id:(event_id "$other")
    ~events:[ message ~id:"$other" {|{"msgtype":"m.text","body":"other"}|} ];
  Alcotest.(check int)
    "scoped event subscription ignores another thread" 1 !changed;
  let late_reply = thread_reply ~root:"$late-root" "$late-reply" "late" in
  Ui.Thread_cache.ingest threads ~room_id:the_room ~events:[ late_reply ];
  let late_changed = ref 0 in
  let unsubscribe_late =
    Ui.Thread_cache.subscribe_events ~room_id:the_room
      ~root_id:(event_id "$late-root") threads (fun _ -> incr late_changed)
  in
  Alcotest.(check int) "late thread starts with reply snapshot" 1 !late_changed;
  let late_root =
    message ~id:"$late-root" {|{"msgtype":"m.text","body":"root"}|}
  in
  Ui.Thread_cache.ingest threads ~room_id:the_room ~events:[ late_root ];
  Alcotest.(check int) "late root notifies its thread" 2 !late_changed;
  unsubscribe_late ();
  let wrong_room = Matrix_proto.Id.Room_id.of_string_exn "!other:example.org" in
  let mismatched =
    { late_reply with Event.Raw_event.room_id = Some wrong_room }
  in
  Ui.Thread_cache.ingest_thread threads ~room_id:the_room
    ~root_id:(event_id "$thread") ~events:[ mismatched ];
  Ui.Thread_cache.ingest_thread threads ~room_id:the_room
    ~root_id:(event_id "$thread")
    ~events:[ thread_reply ~root:"$different" "$bad-relation" "bad" ];
  Alcotest.(check int) "mismatched thread input is ignored" 1 !changed;
  let receipts =
    Matrix_client.Read_state.v ~fully_read:(event_id "$reply") ()
  in
  let receipt_changed = ref 0 in
  let unsubscribe_receipts =
    Ui.Thread_cache.subscribe_receipts ~room_id:the_room
      ~root_id:(event_id "$thread") threads (fun _ -> incr receipt_changed)
  in
  Ui.Thread_cache.set_receipts threads ~room_id:the_room
    ~root_id:(event_id "$thread") receipts;
  Alcotest.(check int) "scoped receipt callback" 2 !receipt_changed;
  let unread_changed = ref 0 in
  let unsubscribe_unread =
    Ui.Thread_cache.subscribe_unread ~room_id:the_room
      ~root_id:(event_id "$thread") threads (fun _ -> incr unread_changed)
  in
  let unread =
    { Matrix_client.Read_state.unread = 1; notifications = 1; highlights = 0 }
  in
  Ui.Thread_cache.set_unread threads ~room_id:the_room
    ~root_id:(event_id "$thread") unread;
  Alcotest.(check int) "scoped unread callback" 2 !unread_changed;
  Ui.Thread_cache.set_pagination threads ~room_id:the_room
    ~root_id:(event_id "$thread") ~backward:(Ui.Thread_cache.Has_more "b")
    ~forward:(Ui.Thread_cache.Has_more "f");
  Ui.Thread_cache.set_pagination threads ~room_id:the_room
    ~root_id:(event_id "$other") ~backward:Ui.Thread_cache.Hit_end
    ~forward:Ui.Thread_cache.Not_started;
  unsubscribe ();
  unsubscribe ();
  unsubscribe_receipts ();
  unsubscribe_receipts ();
  unsubscribe_unread ();
  Ui.Thread_cache.ingest_thread threads ~room_id:the_room
    ~root_id:(event_id "$thread") ~events:[ reply ];
  Alcotest.(check int) "cancelled scoped event callback" 2 !changed;
  let persisted_receipts =
    Matrix_client.Read_state.v ~fully_read:(event_id "$earlier") ()
  in
  Ui.Thread_cache.set_receipts threads ~room_id:the_room
    ~root_id:(event_id "$thread") persisted_receipts;
  let persisted_unread =
    { Matrix_client.Read_state.unread = 2; notifications = 1; highlights = 1 }
  in
  Ui.Thread_cache.set_unread threads ~room_id:the_room
    ~root_id:(event_id "$thread") persisted_unread;
  Alcotest.(check int) "cancelled scoped receipt callback" 2 !receipt_changed;
  Alcotest.(check int) "cancelled scoped unread callback" 2 !unread_changed;
  (* A physical timeline copy must not be duplicated in the detached registry. *)
  Ui.Event_cache.prepend events the_room ~events:[ root ] ~prev_batch:None;
  Ui.Event_cache.register_external_event events the_room ~event:root;
  (match Ui.Event_store.load_room event_store the_room with
  | Ok (Some room) ->
      Alcotest.(check bool)
        "physical event is not detached" false
        (List.exists
           (fun (event : Ui.Event_store.Internal.event) ->
             Option.equal Matrix_proto.Id.Event_id.equal event.event.event_id
               (Some (event_id "$thread")))
           room.external_events)
  | _ -> Alcotest.fail "event store room disappeared");
  let restarted_events = Ui.Event_cache.create ~store:event_store () in
  let restarted_threads =
    Ui.Thread_cache.create ~event_cache:restarted_events ~store ()
  in
  let restarted =
    Ui.Thread_cache.snapshot restarted_threads ~room_id:the_room
      ~root_id:(event_id "$thread")
  in
  (match restarted with
  | Some view ->
      Alcotest.(check int)
        "ordered restart event count" 4 (List.length view.events);
      Alcotest.(check (list string))
        "event order survives restart"
        [ "$thread"; "$earlier"; "$latest"; "$reply" ]
        (List.filter_map
           (fun (event : Event.Raw_event.t) ->
             Option.map Matrix_proto.Id.Event_id.to_string event.event_id)
           view.events);
      Alcotest.(check bool)
        "backward token survives restart" true
        (view.pagination.backward = Ui.Thread_cache.Has_more "b");
      Alcotest.(check (option string))
        "receipt state survives restart" (Some "$earlier")
        (Option.map Matrix_proto.Id.Event_id.to_string
           (Matrix_client.Read_state.fully_read view.receipts));
      Alcotest.(check int) "unread state survives restart" 2 view.unread.unread
  | None -> Alcotest.fail "thread metadata did not restart");
  (match
     Ui.Thread_cache.snapshot restarted_threads ~room_id:the_room
       ~root_id:(event_id "$other")
   with
  | Some view ->
      Alcotest.(check bool)
        "Hit_end token survives restart" true
        (view.pagination.backward = Ui.Thread_cache.Hit_end);
      Alcotest.(check bool)
        "Not_started token survives restart" true
        (view.pagination.forward = Ui.Thread_cache.Not_started)
  | None -> Alcotest.fail "second thread metadata did not restart");
  Ui.Event_cache.forget_room restarted_events the_room;
  Alcotest.(check bool)
    "forget atomically clears thread" true
    (Option.is_none
       (Ui.Thread_cache.snapshot restarted_threads ~room_id:the_room
          ~root_id:(event_id "$thread")));
  let forgotten_threads =
    Ui.Thread_cache.create
      ~event_cache:(Ui.Event_cache.create ~store:event_store ())
      ~store ()
  in
  Alcotest.(check bool)
    "forget survives thread restart" true
    (Option.is_none
       (Ui.Thread_cache.snapshot forgotten_threads ~room_id:the_room
          ~root_id:(event_id "$thread")));
  Ui.Thread_cache.close threads;
  Ui.Thread_cache.close restarted_threads;
  Ui.Thread_cache.close forgotten_threads

let test_thread_cache_bounds_ordered_metadata () =
  Eio_main.run @@ fun _env ->
  let events = Ui.Event_cache.create () in
  let threads = Ui.Thread_cache.create ~event_cache:events () in
  let root = thread_root () in
  Ui.Event_cache.prepend events the_room ~events:[ root ] ~prev_batch:None;
  let replies =
    List.init 300 (fun index ->
        thread_reply
          ~ts:Int64.(add 1_700_000_001_000L (of_int index))
          (Printf.sprintf "$bounded-%03d" index)
          (Printf.sprintf "reply %d" index))
  in
  Ui.Thread_cache.ingest_thread threads ~room_id:the_room
    ~root_id:(event_id "$thread") ~events:(root :: replies);
  (match
     Ui.Thread_cache.snapshot threads ~room_id:the_room
       ~root_id:(event_id "$thread")
   with
  | None -> Alcotest.fail "bounded thread cache disappeared"
  | Some view ->
      Alcotest.(check int)
        "root plus 255 newest replies" 256 (List.length view.events);
      Alcotest.(check (list string))
        "the root stays first and the oldest excess replies are dropped"
        [ "$thread"; "$bounded-045"; "$bounded-299" ]
        (match view.events with
        | [] -> []
        | root :: replies ->
            List.filter_map
              (fun (event : Event.Raw_event.t) ->
                Option.map Matrix_proto.Id.Event_id.to_string event.event_id)
              [ root; List.hd replies; List.hd (List.rev replies) ]));
  Ui.Thread_cache.close threads

let test_thread_info_summary_and_restart () =
  Eio_main.run @@ fun _env ->
  let store = Matrix_client.Store.memory () in
  let summaries = Ui.Thread_info.create ~store ~user_id:alice () in
  let root = thread_root () in
  (match Ui.Thread_info.summary_of_root root with
  | ( Ui.Thread_info.Known { reply_count = 2; latest_reply_id = Some latest },
      Some latest_event ) ->
      Alcotest.(check string)
        "bundled latest id" "$latest"
        (Matrix_proto.Id.Event_id.to_string latest);
      Alcotest.(check string)
        "bundled latest body" "latest"
        (Matrix_proto.Json.find_string "body" latest_event.content
        |> Option.value ~default:"")
  | _ -> Alcotest.fail "bundled summary was not decoded");
  (match
     Ui.Thread_info.summary_of_root
       (raw
          {|{"event_id":"$bad-thread","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{},"unsigned":{"m.relations":{"m.thread":{"count":-1}}}}|})
   with
  | Ui.Thread_info.Unknown, None -> ()
  | _ -> Alcotest.fail "malformed bundled summary was not Unknown");
  (match
     Ui.Thread_info.summary_of_root
       (raw
          {|{"event_id":"$missing-latest","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{},"unsigned":{"m.relations":{"m.thread":{"count":1}}}}|})
   with
  | Ui.Thread_info.Unknown, None -> ()
  | _ -> Alcotest.fail "a summary without latest_event was not Unknown");
  (match
     Ui.Thread_info.summary_of_root
       (raw
          {|{"event_id":"$bad-shape","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{},"unsigned":{"m.relations":{"m.thread":[]}}}|})
   with
  | Ui.Thread_info.Unknown, None -> ()
  | _ -> Alcotest.fail "malformed thread shape was not Unknown");
  (match Ui.Thread_info.summary_of_root (message ~id:"$plain-root" "{}") with
  | Ui.Thread_info.Known_none, None -> ()
  | _ -> Alcotest.fail "missing bundled summary was not known-none");
  Ui.Thread_info.ingest_root summaries ~room_id:the_room root;
  let state = Matrix_client.Base_client.create ~user_id:alice () in
  Ui.Thread_info.refresh_room summaries ~state ~room_id:the_room
    ~events:[ root; thread_reply "$reply" "reply" ];
  let first = Ui.Thread_info.snapshot summaries the_room in
  Alcotest.(check int) "one root is retained" 1 (Array.length first);
  Alcotest.(check int)
    "server count is retained over partial cache" 2 first.(0).reply_count;
  Alcotest.(check string)
    "latest reply is bundled event" "$latest"
    (Matrix_proto.Id.Event_id.to_string
       (Option.get (Option.get first.(0).latest_reply).event_id));
  (* Store.Slot persistence is independent of the optional SQLite event store. *)
  let restored = Ui.Thread_info.create ~store ~user_id:alice () in
  let restored_items = Ui.Thread_info.snapshot restored the_room in
  Alcotest.(check int)
    "summary survives restart" 1
    (Array.length restored_items);
  let restored_item =
    match restored_items with
    | [| item |] -> item
    | _ -> Alcotest.fail "summary survives restart"
  in
  Alcotest.(check bool)
    "the persisted root is normalized" true
    (match restored_item.Ui.Thread_info.root.unsigned with
    | None -> true
    | Some unsigned -> Option.is_none (Event.Unsigned.relations unsigned));
  Ui.Thread_info.ingest_root summaries ~room_id:the_room root;
  Alcotest.(check int)
    "duplicate root stays one item" 1
    (Array.length (Ui.Thread_info.snapshot summaries the_room));
  let local = Ui.Thread_info.create ~user_id:alice () in
  let local_root = message ~id:"$local-thread" {|{"body":"root"}|} in
  let duplicate_reply =
    thread_reply ~root:"$local-thread" "$local-reply" "reply"
  in
  Ui.Thread_info.ingest_root local ~room_id:the_room local_root;
  Ui.Thread_info.refresh_room local ~state ~room_id:the_room
    ~events:[ local_root; duplicate_reply; duplicate_reply ];
  let local_info = (Ui.Thread_info.snapshot local the_room).(0) in
  Alcotest.(check int) "duplicate reply counts once" 1 local_info.reply_count;
  (match local_info.summary_status with
  | Ui.Thread_info.Known { reply_count = 1; _ } -> ()
  | _ -> Alcotest.fail "an observed reply did not establish a known summary");
  let event_store =
    Ui.Event_store.memory ~plaintext_policy:Store_plaintext ()
  in
  let stored =
    {
      Model.next_chunk_id = 1;
      chunks =
        [ events_chunk [ stored_event ~clear_event:root "event:$thread" root ] ];
      external_events = [];
    }
  in
  ignore (Ui.Event_store.save_room event_store the_room stored);
  let loaded = Ui.Event_store.load_room event_store the_room |> Result.get_ok in
  let persisted_root =
    match loaded with
    | Some room -> (List.hd (Model.room_events room)).event
    | None -> Alcotest.fail "thread root was not persisted"
  in
  Alcotest.(check bool)
    "bundled relations are not stored in event chunks" true
    (Option.for_all
       (fun unsigned -> Option.is_none (Event.Unsigned.relations unsigned))
       persisted_root.unsigned);
  let persisted_clear =
    match loaded with
    | Some room -> (List.hd (Model.room_events room)).clear_event
    | None -> None
  in
  Alcotest.(check bool)
    "bundled relations are not stored in decrypted chunks" true
    (Option.for_all
       (fun event ->
         Option.for_all
           (fun unsigned -> Option.is_none (Event.Unsigned.relations unsigned))
           event.Event.Raw_event.unsigned)
       persisted_clear);
  let stale = Ui.Thread_info.infos summaries the_room in
  Ui.Thread_info.remove_room summaries the_room;
  Alcotest.(check int)
    "forget clears observable summary" 0
    (Array.length (Ui.Observable.List.snapshot stale));
  let fresh = Ui.Thread_info.infos summaries the_room in
  Alcotest.(check bool) "forget detaches the old handle" false (fresh == stale);
  Ui.Thread_info.ingest_root summaries ~room_id:the_room root;
  Alcotest.(check int)
    "a post-forget handle can be repopulated" 1
    (Array.length (Ui.Observable.List.snapshot fresh));
  Alcotest.(check int)
    "the stale handle stays empty" 0
    (Array.length (Ui.Observable.List.snapshot stale))

let test_thread_info_disk_restart_and_forget () =
  Eio_main.run @@ fun env ->
  let path = Filename.temp_file "matrix-thread-info" ".d" in
  Unix.unlink path;
  Unix.mkdir path 0o700;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun name ->
          let file = Filename.concat path name in
          if Sys.file_exists file then Unix.unlink file)
        [ "base_state.json"; "base_state.json.tmp"; ".profile.lock" ];
      Unix.rmdir path)
    (fun () ->
      let dir = Eio.Path.(Eio.Stdenv.fs env / path) in
      let first_store = Matrix_client.Store.on_disk ~dir in
      let first = Ui.Thread_info.create ~store:first_store ~user_id:alice () in
      Ui.Thread_info.ingest_root first ~room_id:the_room (thread_root ());
      Alcotest.(check bool)
        "thread slot is flushed" true
        (Sys.file_exists (Filename.concat path "base_state.json"));
      let restored_store = Matrix_client.Store.on_disk ~dir in
      let restored =
        Ui.Thread_info.create ~store:restored_store ~user_id:alice ()
      in
      Alcotest.(check int)
        "on-disk summary survives restart" 1
        (Array.length (Ui.Thread_info.snapshot restored the_room));
      let stale = Ui.Thread_info.infos restored the_room in
      Ui.Thread_info.remove_room restored the_room;
      Alcotest.(check int)
        "on-disk forget clears the existing handle" 0
        (Array.length (Ui.Observable.List.snapshot stale));
      let final_store = Matrix_client.Store.on_disk ~dir in
      let final = Ui.Thread_info.create ~store:final_store ~user_id:alice () in
      Alcotest.(check int)
        "on-disk forget survives restart" 0
        (Array.length (Ui.Thread_info.snapshot final the_room)))

let () =
  Alcotest.run "matrix.ui"
    [
      ( "html",
        [
          Alcotest.test_case "HTML sanitizer" `Quick test_sanitize_html;
          Alcotest.test_case "HTML malformed URIs" `Quick
            test_sanitize_malformed_uris;
          Alcotest.test_case "plain and Unicode" `Quick
            test_plain_reply_and_unicode;
        ] );
      ( "presentation",
        [
          Alcotest.test_case "message" `Quick test_presentation;
          Alcotest.test_case "membership changes" `Quick test_membership_changes;
          Alcotest.test_case "profile change" `Quick test_profile_change;
          Alcotest.test_case "other state" `Quick test_other_state;
          Alcotest.test_case "beacon presentation" `Quick
            test_beacon_info_presentation;
          Alcotest.test_case "preview eligibility" `Quick test_preview_worthy;
        ] );
      ( "observable",
        Alcotest.test_case "bounded reset and granular diffs" `Quick
          test_reconcile_reset_and_granular_diffs
        :: List.map QCheck_alcotest.to_alcotest [ reconcile_property ] );
      ( "chunks",
        Alcotest.test_case "a disjoint limited sync keeps its history" `Quick
          test_limited_sync
        :: Alcotest.test_case "a limited sync that overlaps merges" `Quick
             test_limited_sync_overlap
        :: Alcotest.test_case "filling a gap" `Quick test_gap_fill
        :: Alcotest.test_case "receipt reconciliation gap safety" `Quick
             test_receipt_reconcile_gap_safety
        :: Alcotest.test_case "room recount ignores local echo and thread reply"
             `Quick test_room_reconcile_ignores_local_echo_and_thread_reply
        :: Alcotest.test_case "cache snapshot validation is atomic" `Quick
             test_event_cache_snapshot_validation_is_atomic
        :: Alcotest.test_case "physical decryption updates room count" `Quick
             test_physical_decryption_notifies_and_changes_room_count
        :: Alcotest.test_case "trimming leaves a fillable gap" `Quick
             test_trim_leaves_a_gap
        :: List.map QCheck_alcotest.to_alcotest [ splice_property ] );
      ( "timeline",
        [
          Alcotest.test_case "aggregation" `Quick test_timeline_aggregation;
          Alcotest.test_case "relation index refresh" `Quick
            test_timeline_relation_index_refresh;
          Alcotest.test_case "edit revision order" `Quick
            test_edit_revisions_order_and_duplicates;
          Alcotest.test_case "invalid and redacted revisions" `Quick
            test_edit_revisions_skip_invalid_and_redacted;
          Alcotest.test_case "encrypted revision provenance" `Quick
            test_edit_revisions_encrypted_provenance;
          Alcotest.test_case "custom state filter" `Quick
            test_timeline_custom_state_filter;
          Alcotest.test_case "exclusion filter" `Quick
            test_timeline_exclusion_filter;
          Alcotest.test_case "unable to decrypt stays visible" `Quick
            test_timeline_filter_keeps_unable_to_decrypt;
          Alcotest.test_case "local echo" `Quick test_local_echo;
          Alcotest.test_case "attachment caption updates local echo" `Quick
            test_attachment_caption_echo_updates;
          Alcotest.test_case "attachment caption migrates local echo" `Quick
            test_attachment_caption_echo_migration;
          Alcotest.test_case "in-flight redaction replaces echo" `Quick
            test_in_flight_redaction_replaces_echo;
          Alcotest.test_case "forget persists queue deletion" `Quick
            test_send_queue_forget_persists_deletion;
          Alcotest.test_case "forget detaches in-flight event" `Quick
            test_send_queue_forget_in_flight_event;
          Alcotest.test_case "forget detaches in-flight upload" `Quick
            test_send_queue_forget_in_flight_upload;
          Alcotest.test_case "gap becomes the timeline start" `Quick
            test_timeline_start;
          Alcotest.test_case "gaps are items in place" `Quick test_gap_items;
          Alcotest.test_case "read marker" `Quick test_read_marker;
          Alcotest.test_case "timeline receipts" `Quick test_timeline_receipts;
          Alcotest.test_case "own-event receipt targets" `Quick
            test_timeline_receipt_own_event_targets;
          Alcotest.test_case "sending a formatted reply" `Quick
            test_send_message;
          Alcotest.test_case "sending a location" `Quick test_send_location;
          Alcotest.test_case "sending an edit" `Quick test_send_edit;
          Alcotest.test_case "resolves media consistently" `Quick
            test_timeline_resolves_media_consistently;
          Alcotest.test_case "finding the item a send produced" `Quick
            test_request_lookup;
          Alcotest.test_case "what a pagination did" `Quick
            test_pagination_outcome;
        ] );
      ( "event-focused",
        [
          Alcotest.test_case "ordering, overlap and terminal edges" `Quick
            test_event_focused_ordering_and_edges;
          Alcotest.test_case "shared cache registration and reload" `Quick
            test_event_focused_shared_cache;
          Alcotest.test_case "forget closes focused view" `Quick
            test_event_focused_forget_closes_view;
          Alcotest.test_case "thread pagination and root retry" `Quick
            test_event_focused_thread_pagination;
          Alcotest.test_case "force thread root" `Quick
            test_event_focused_force_thread_root;
          Alcotest.test_case "target validation" `Quick
            test_event_focused_target_validation_and_id_filter;
          Alcotest.test_case "automatic room filtering" `Quick
            test_event_focused_automatic_room_filter;
          Alcotest.test_case "failure, reset and close" `Quick
            test_event_focused_failure_reset_and_close;
          Alcotest.test_case "close during request and reentrancy" `Quick
            test_event_focused_close_during_request;
          Alcotest.test_case "failed start can retry" `Quick
            test_event_focused_failed_start_retries;
          Alcotest.test_case "callback exceptions are isolated" `Quick
            test_event_focused_callback_isolation;
        ] );
      ( "rooms",
        [
          Alcotest.test_case "sync retry goes offline then live" `Quick
            test_runtime_sync_offline;
          Alcotest.test_case "terminal sync failure stops" `Quick
            test_runtime_sync_terminal_failure;
          Alcotest.test_case "stopping while offline" `Quick
            test_runtime_stop_while_offline;
          Alcotest.test_case "typing users" `Quick test_runtime_typing_users;
          Alcotest.test_case "receipt target backfill" `Quick
            test_runtime_receipt_backfill;
          Alcotest.test_case "recount without a receipt" `Quick
            test_runtime_recounts_without_receipt;
          Alcotest.test_case "terminal receipt target backfill" `Quick
            test_runtime_receipt_backfill_terminal;
          Alcotest.test_case "stale unread projection" `Quick
            test_local_unread_counts_stale_expected_state;
          Alcotest.test_case "persisted unread projection and rollback" `Quick
            test_local_unread_counts_persistence_and_rollback;
          Alcotest.test_case "stop before start" `Quick
            test_runtime_stop_before_start;
          Alcotest.test_case "forget drops an in-flight sync" `Quick
            test_runtime_forget_drops_in_flight_sync;
          Alcotest.test_case "forget drops sync after crypto prelude" `Quick
            test_runtime_forget_drops_sync_after_crypto_prelude;
          Alcotest.test_case "forget drops sync after crypto post-fold" `Quick
            test_runtime_forget_drops_sync_after_crypto_postfold;
          Alcotest.test_case "implicit queue persistence dependencies" `Quick
            test_runtime_send_queue_dependencies;
          Alcotest.test_case "unknown sessions request and recover" `Quick
            test_runtime_unknown_session_requests_once;
          Alcotest.test_case "looking a room up by id" `Quick
            test_room_list_find;
          Alcotest.test_case "the position of an event" `Quick test_position;
          Alcotest.test_case "joining and leaving" `Quick
            test_runtime_membership;
          Alcotest.test_case "forget cleans local room state" `Quick
            test_runtime_forget_cleans_local_state;
          Alcotest.test_case "forget clears an open timeline" `Quick
            test_runtime_forget_clears_open_timeline;
          Alcotest.test_case "forget removes direct account data" `Quick
            test_runtime_forget_direct_updates_account_data;
          Alcotest.test_case "failed direct cleanup still forgets" `Quick
            test_runtime_forget_direct_cleanup_failure_still_forgets;
          Alcotest.test_case "failed forget preserves local state" `Quick
            test_runtime_forget_server_failure_preserves_local_state;
          Alcotest.test_case "forget flushes its queue store" `Quick
            test_runtime_forget_flushes_queue_store;
          Alcotest.test_case "recording an accepted invite" `Quick
            test_runtime_join_invite_acceptance;
          Alcotest.test_case "joining an invite by alias" `Quick
            test_runtime_join_alias;
          Alcotest.test_case "stopping closes timelines" `Quick
            test_runtime_stop_closes_timelines;
          Alcotest.test_case "thread-list lifecycle" `Quick
            test_runtime_thread_list_lifecycle;
          Alcotest.test_case "timeline resolver isolation" `Quick
            test_runtime_timeline_resolver_isolation;
          Alcotest.test_case "recovery manager projection and release" `Quick
            test_runtime_recovery_manager_seed_and_release;
        ] );
      ( "utd",
        [
          Alcotest.test_case "deduplication and late timing" `Quick
            test_utd_hook_lifecycle;
        ] );
      ( "encryption errors",
        [
          Alcotest.test_case "callback isolation" `Quick
            test_sync_encryption_error_callback_isolated;
        ] );
      ( "store",
        [
          Alcotest.test_case "durable thread event projection" `Quick
            test_thread_cache_durable_projection;
          Alcotest.test_case "thread projection is bounded" `Quick
            test_thread_cache_bounds_ordered_metadata;
          Alcotest.test_case "thread summary disk restart and forget" `Quick
            test_thread_info_disk_restart_and_forget;
          Alcotest.test_case "thread summary persistence and deduplication"
            `Quick test_thread_info_summary_and_restart;
          Alcotest.test_case "SQLite roundtrip" `Quick test_sqlite_roundtrip;
          Alcotest.test_case "a stale schema is dropped" `Quick
            test_sqlite_schema_bump;
          Alcotest.test_case "incremental writes" `Quick
            test_incremental_persistence;
          Alcotest.test_case "forget removes room data" `Quick
            test_event_cache_forget_room;
          Alcotest.test_case "forgotten in-flight request stays hidden" `Quick
            test_event_cache_forget_in_flight_request;
        ] );
    ]
