module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
open Matrix_proto.Json

type item = {
  event : Event.Raw_event.t;
  local_echo : bool;
  redacted : bool;
  replacement : Jsont.json option;
}

let empty_object = Jsont.Json.object' []
let event i = i.event
let local_echo i = i.local_echo
let redacted i = i.redacted
let replacement i = i.replacement

let content i =
  if i.redacted then empty_object
  else match i.replacement with Some c -> c | None -> i.event.content

type t = {
  t_room_id : Id.Room_id.t;
  t_limit : int;
  mutable t_items : item list; (* Oldest first. *)
  mutable t_prev_batch : string option;
}

let create ~room_id ?(limit = 1000) () =
  { t_room_id = room_id; t_limit = limit; t_items = []; t_prev_batch = None }

let room_id t = t.t_room_id
let items t = t.t_items
let length t = List.length t.t_items

let event_id_string (e : Event.Raw_event.t) =
  Option.map Id.Event_id.to_string e.event_id

let find t id =
  let target = Id.Event_id.to_string id in
  List.find_opt (fun i -> event_id_string i.event = Some target) t.t_items

let rec last_of = function
  | [] -> None
  | [ i ] -> Some i
  | _ :: tl -> last_of tl

let last t = last_of t.t_items

let clear t =
  t.t_items <- [];
  t.t_prev_batch <- None

let prev_batch t = t.t_prev_batch
let set_prev_batch t b = t.t_prev_batch <- b
let relates_to (e : Event.Raw_event.t) = find_mem "m.relates_to" e.content

let replacement_target e =
  match relates_to e with
  | None -> None
  | Some r ->
      if find_string "rel_type" r = Some "m.replace" then
        find_string "event_id" r
      else None

let redaction_target (e : Event.Raw_event.t) =
  if not (String.equal (Event.Event_type.to_string e.type_) "m.room.redaction")
  then None
  else
    match find_string "redacts" e.content with
    | Some _ as target -> target
    | None -> Option.map Id.Event_id.to_string e.redacts

let txn_of (e : Event.Raw_event.t) =
  match e.unsigned with
  | None -> None
  | Some u ->
      Option.map Id.Transaction_id.to_string (Event.Unsigned.transaction_id u)

let map_item t target f =
  t.t_items <-
    List.map
      (fun i -> if event_id_string i.event = Some target then f i else i)
      t.t_items

let has_target t target =
  List.exists (fun i -> event_id_string i.event = Some target) t.t_items

let trim t =
  let n = List.length t.t_items in
  if n > t.t_limit then
    t.t_items <- List.filteri (fun i _ -> i >= n - t.t_limit) t.t_items

let insert t ~local_echo ~at_front (e : Event.Raw_event.t) =
  let i = { event = e; local_echo; redacted = false; replacement = None } in
  if at_front then t.t_items <- i :: t.t_items
  else (
    t.t_items <- t.t_items @ [ i ];
    trim t)

let apply_relation t (e : Event.Raw_event.t) =
  match replacement_target e with
  | Some target when has_target t target ->
      let new_content = find_mem "m.new_content" e.content in
      map_item t target (fun i -> { i with replacement = new_content });
      true
  | _ -> (
      match redaction_target e with
      | Some target when has_target t target ->
          map_item t target (fun i ->
              { i with redacted = true; replacement = None });
          true
      | _ -> false)

let add t ?(local_echo = false) (e : Event.Raw_event.t) =
  let already =
    match event_id_string e with Some id -> has_target t id | None -> false
  in
  if already then ()
  else
    (* A remote event that carries our transaction id replaces the local
       echo we made for it, keeping the echo's position in the list. *)
    let echo_replaced =
      match txn_of e with
      | None -> false
      | Some txn ->
          let found = ref false in
          t.t_items <-
            List.map
              (fun i ->
                if (not !found) && i.local_echo && txn_of i.event = Some txn
                then (
                  found := true;
                  { i with event = e; local_echo = false })
                else i)
              t.t_items;
          !found
    in
    if echo_replaced then ()
    else if apply_relation t e then ()
    else insert t ~local_echo ~at_front:false e

let add_many t l = List.iter (fun e -> add t e) l

let prepend t l =
  (* Oldest first: insert at the front in reverse so order is preserved.
     Every event in [l] is inserted as its own raw item first, edits and
     redactions included, so that a relation's target is always already
     present for the second pass below regardless of where in the batch it
     falls: a fetched page can hold both an edit and the message it edits,
     and reversing the batch to get insertion order right would otherwise
     visit the edit before its target every time. *)
  List.iter
    (fun (e : Event.Raw_event.t) ->
      let already =
        match event_id_string e with
        | Some id -> has_target t id
        | None -> false
      in
      if not already then insert t ~local_echo:false ~at_front:true e)
    (List.rev l);
  (* Oldest first, so that of several edits to the same target in this batch,
     the chronologically last is the one whose replacement sticks. *)
  List.iter
    (fun (e : Event.Raw_event.t) ->
      if apply_relation t e then
        match event_id_string e with
        | Some id ->
            t.t_items <-
              List.filter
                (fun i -> event_id_string i.event <> Some id)
                t.t_items
        | None -> ())
    l

let paginate_back client t ?(limit = 20) () =
  match t.t_prev_batch with
  | None -> Ok []
  | Some from -> (
      match
        Messages.get_messages client ~room_id:t.t_room_id ~from
          ~dir:Matrix_proto.Common.Direction.Backward ~limit ()
      with
      | Error e -> Error e
      | Ok reply ->
          (* [/messages] going backwards returns newest first. *)
          let page = reply.Messages.page in
          let chunk = List.rev page.Matrix_proto.Common.Page.chunk in
          prepend t chunk;
          t.t_prev_batch <-
            (if chunk = [] then None
             else page.Matrix_proto.Common.Page.next_batch);
          Ok chunk)
