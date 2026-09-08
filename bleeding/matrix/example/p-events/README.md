# `p-events`

<br>

`events` builds an event content, encodes it to JSON and decodes it back,
then decodes a small state event from a string literal and reads its typed
fields. It takes no arguments and touches no network. Every step prints what
it found.

<br>

`matrix-chat.proto` gives every wire shape a `jsont` codec and every identifier a
type of its own, so a room id and a user id cannot be swapped by mistake and
a decode of malformed JSON fails where it happens rather than later, deep in
a client. This example exercises both away from the network calls that
surround them elsewhere in the tutorial.

```ocaml
module E = Matrix_proto.Event
module Id = Matrix_proto.Id

let member_event =
  {|{"content": {"membership": "join", "displayname": "Alice"},
     "event_id": "$abc123:localhost",
     "origin_server_ts": 1700000000000,
     "sender": "@alice:localhost",
     "state_key": "@alice:localhost",
     "type": "m.room.member"}|}

let () =
  let content = E.Text_message_content.make ~body:"Hello, Matrix" () in
  let json =
    match Jsont_bytesrw.encode_string E.Text_message_content.jsont content with
    | Ok json -> json
    | Error e -> failwith e
  in
  Printf.printf "%s\n" json;
  (match Jsont_bytesrw.decode_string E.Text_message_content.jsont json with
  | Error e -> failwith e
  | Ok content ->
      Printf.printf "body: %s\n" (E.Text_message_content.body content));

  match Jsont_bytesrw.decode_string E.Raw_event.jsont member_event with
  | Error e -> failwith e
  | Ok event ->
      (Printf.printf "sender: %s\n" (Id.User_id.to_string event.sender);
       (match event.event_id with
       | None -> ()
       | Some event_id ->
           Printf.printf "event_id: %s\n" (Id.Event_id.to_string event_id));
       match Jsont.Json.decode E.Room_member_content.jsont event.content with
       | Error e -> failwith e
       | Ok member ->
           Printf.printf "membership: %s\n"
             (E.Membership.to_string (E.Room_member_content.membership member)));

      (match Id.User_id.of_string "not-a-user-id" with
      | Ok _ -> assert false
      | Error (`Msg msg) -> Printf.printf "rejected: %s\n" msg);
      (match Id.User_id.of_string "@Alice_B:example.org" with
      | Error _ -> assert false
      | Ok uid ->
          Printf.printf "historical, spec conformant: %b\n"
            (Id.User_id.is_spec_conformant uid));

      let alice = Id.User_id.of_string_exn "@alice:localhost" in
      let alice' = Id.User_id.of_string_exn "@alice:localhost" in
      let bob = Id.User_id.of_string_exn "@bob:localhost" in
      Printf.printf "alice equal alice: %b\n" (Id.User_id.equal alice alice');
      Printf.printf "alice before bob: %b\n" (Id.User_id.compare alice bob < 0)
```

<br>

[`Matrix_proto.Event.Text_message_content.make`](../../lib/matrix_proto/matrix_event_message.mli)
builds the content of an `m.text` message from its body. `Jsont_bytesrw`'s
`encode_string` and `decode_string` write and read it with the content's own
`jsont` codec, so the body printed after the round trip matches the one
`make` was given.

<br>

[`Matrix_proto.Event.Raw_event.jsont`](../../lib/matrix_proto/matrix_event.mli)
decodes an event of any type with its content left as JSON, which is what
`/sync` hands a client before it knows the type of each event.
`event.sender` and `event.event_id` come out already typed, as
`Matrix_proto.Id.User_id.t` and `Matrix_proto.Id.Event_id.t`. Once
`event.type_` says the event is `m.room.member`, its content decodes with
`Jsont.Json.decode` and
[`Matrix_proto.Event.Room_member_content.jsont`](../../lib/matrix_proto/matrix_event_state.mli),
giving the typed `Membership.t` the join declared.

<br>

[`Matrix_proto.Id.User_id.of_string`](../../lib/matrix_proto/matrix_id.mli)
rejects `not-a-user-id` because it carries no `@` sigil, and the error names
the fault. It accepts `@Alice_B:example.org` because the specification
requires historical identifiers to keep working, even where their localpart
uses characters no longer allowed in a new one, and `is_spec_conformant`
tells the two kinds apart. `equal` and `compare` order identifiers by their
canonical rendering.

<pre><code><b>$ dune exec -- example/p-events/events.exe</b>
{"body":"Hello, Matrix","msgtype":"m.text"}
body: Hello, Matrix
sender: @alice:localhost
event_id: $abc123:localhost
membership: join
rejected: must start with @
historical, spec conformant: false
alice equal alice: true
alice before bob: true
</code></pre>

<br>

**Next:** [`p-sync`](../p-sync#folders-and-files) decodes a saved `/sync`
response and walks its rooms.

**See also:** [`3-sync`](../3-sync#folders-and-files) decodes the same kind
of event over a live connection. [`r-mock`](../r-mock#folders-and-files)
moves up to `matrix-chat.client`, the layer that turns these types into requests
and results.

<br>

[Up to the example index](../#readme)
