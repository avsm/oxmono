# `p-sync`

<br>

`sync` decodes a saved `/sync` response and walks it, printing the batch
token, then for each joined room its id, its state events, its timeline
events and whether the timeline is limited. It reads the response from a
file named on the command line, or from `sync.json` in this directory when
none is given, and touches no network.

<br>

`GET /sync` is how a client learns what changed since the last time it
asked. The first response, the one with no `since` token, carries the full
state and timeline of every joined room. Every later response carries only
the difference, and a client folds each one into the room state it already
holds rather than replacing it. `sync.json` here is one joined room from
such a response, its state and timeline both present because the server
capped how many timeline events it would return in one answer and moved the
rest into state.

```ocaml
module S = Matrix_proto.Sync
module E = Matrix_proto.Event

let read_file path = In_channel.with_open_bin path In_channel.input_all

let print_state_event (event : E.Raw_event.t) =
  Printf.printf "  state %s %S\n"
    (E.Event_type.to_string event.type_)
    (Option.value event.state_key ~default:"")

let print_timeline_event (event : E.Raw_event.t) =
  let sender = Matrix_proto.Id.User_id.to_string event.sender in
  let type_ = E.Event_type.to_string event.type_ in
  match
    if E.Event_type.equal event.type_ E.Event_type.Room_message then
      Jsont.Json.decode E.Text_message_content.jsont event.content
      |> Result.to_option
    else None
  with
  | Some content ->
      Printf.printf "  %s %s: %s\n" sender type_
        (E.Text_message_content.body content)
  | None -> Printf.printf "  %s %s\n" sender type_

let print_room (room_id, (room : S.Joined_room.t)) =
  Printf.printf "%s\n" room_id;
  (match room.state with
  | None -> ()
  | Some state -> List.iter print_state_event state.events);
  match room.timeline with
  | None -> ()
  | Some timeline ->
      List.iter print_timeline_event timeline.events;
      Printf.printf "  limited: %b\n"
        (Option.value timeline.limited ~default:false)

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "sync.json" in
  match Jsont_bytesrw.decode_string S.Response.jsont (read_file path) with
  | Error e ->
      Printf.eprintf "%s\n" e;
      exit 1
  | Ok response -> (
      Printf.printf "next_batch %s\n" response.next_batch;
      match response.rooms with
      | None -> ()
      | Some rooms -> List.iter print_room rooms.join)
```

<br>

`Jsont_bytesrw.decode_string` reads the whole response into a
[`Matrix_proto.Sync.Response.t`](../../lib/matrix_proto/matrix_sync.mli)
with its `jsont` codec, and `response.next_batch` is the token a client
passes as `since` to continue from here.
[`Matrix_proto.Sync.Rooms.join`](../../lib/matrix_proto/matrix_sync.mli)
pairs each room id with a
[`Matrix_proto.Sync.Joined_room.t`](../../lib/matrix_proto/matrix_sync.mli).

<br>

A joined room's `state` holds
[`Matrix_proto.Event.Raw_event.t`](../../lib/matrix_proto/matrix_event.mli)
values that changed outside the visible timeline, printed here by their
`type_` and `state_key`. `timeline` holds the room's new events in order,
oldest first, printed by `sender` and `type_`, with the body added where
`type_` is `Room_message` and the content decodes with
[`Matrix_proto.Event.Text_message_content.jsont`](../../lib/matrix_proto/matrix_event_message.mli).
`timeline.limited` is `true` exactly when events were left out of this
batch, which is what tells a client it must paginate from `prev_batch` to
see them.

<pre><code><b>$ dune exec -- example/p-sync/sync.exe</b>
next_batch s27831_8449_0_72_431_1_1132_4937_0_1_1_1_1_1
!OaZVTorgdovXfpDRiH:localhost
  state m.room.name ""
  @pexample-173172483:localhost m.room.message: Hello from the p-sync example
  @pexample-173172483:localhost m.room.message: A second message
  limited: true
</code></pre>

<br>

**Next:** [`r-mock`](../r-mock#folders-and-files) moves up to
`matrix-chat.client`, the layer that turns requests into results instead of
raising.

**See also:** [`p-events`](../p-events#folders-and-files) decodes one event
at a time, the type `Raw_event.t` this example decodes many of at once.
[`3-sync`](../3-sync#folders-and-files) runs the same `/sync` loop against a
live homeserver.

<br>

[Up to the example index](../#readme)
