open Matrix_bot
module Id = Matrix_proto.Id

let default_words = [ "badger"; "spoiler" ]

(* [Matching.contains] folds case and strips diacritics, so a forbidden word
   cannot be evaded by spelling it fullwidth or with an accent. *)
let offending ~words body =
  List.find_opt
    (fun word -> Matrix_ui.Matching.contains ~haystack:body ~needle:word)
    words

(* Keyed by room and by user, so somebody warned in one room starts clean in
   the next, and the count outlives the process. *)
let strike bot room sender =
  Plugin_store.update (Bot.plugin_store bot) ~room:(Room.id room)
    ~plugin:"moderator" ~key:(Id.User_id.to_string sender)
    Matrix_proto.Json.Codec.int (function
    | None -> 1
    | Some count -> count + 1)

let remove room ~limit ~who ~count sender =
  match Room.kick room ~reason:"Repeatedly used a forbidden word" sender with
  | Ok () -> Printf.sprintf "%s has been removed after %d strikes." who count
  | Error error ->
      Format.asprintf "%s is on strike %d of %d, but I cannot remove them: %a"
        who count limit Matrix_client.Error.pp error

let verdict bot room ~limit ~word sender =
  let who = Id.User_id.to_string sender in
  match strike bot room sender with
  | Error error ->
      Printf.sprintf
        "%s: that message has been removed (%S is not allowed here). I cannot \
         keep count: %s"
        who word
        (Plugin_store.error_to_string error)
  | Ok count when count < limit ->
      Printf.sprintf
        "%s: that message has been removed (%S is not allowed here). Strike %d \
         of %d."
        who word count limit
  | Ok count -> remove room ~limit ~who ~count sender

let moderate ~words ~limit bot (m : Event.message) =
  match offending ~words m.content.body with
  | None -> ()
  | Some word ->
      let room = m.envelope.room in
      (* Redact first: the point is that the message goes, whatever happens
         to the warning. *)
      ignore
        (Room.redact room
           ~reason:("Forbidden word: " ^ word)
           m.envelope.event_id);
      ignore
        (Room.send_notice room
           (verdict bot room ~limit ~word m.envelope.sender))

let plugin ?(words = default_words) ?(strikes = 3) =
  Bot.on_message (moderate ~words ~limit:strikes)
