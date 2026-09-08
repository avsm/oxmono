let event client ~room_id ~event_id ?reason ?score () =
  Error.unwrap ~context:"reporting event"
    (Matrix_client.Report.event (Client.base client) ~room_id ~event_id ?reason
       ?score ())

let room client ~room_id ?reason () =
  Error.unwrap ~context:"reporting room"
    (Matrix_client.Report.room (Client.base client) ~room_id ?reason ())

let user client ~user_id ?reason () =
  Error.unwrap ~context:"reporting user"
    (Matrix_client.Report.user (Client.base client) ~user_id ?reason ())
