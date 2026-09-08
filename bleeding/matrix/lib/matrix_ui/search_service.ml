module Search = Matrix_client.Search

type state = Idle of { end_reached : bool } | Loading

type t = {
  client : Matrix_client.Client.t;
  mutable criteria : Search.criteria option;
  mutable next_batch : string option;
  mutable pages : int;
  results : Search.hit Observable.List.t;
  state : state Observable.Value.t;
  last_error : Matrix_client.Error.t option Observable.Value.t;
}

let create ~client () =
  {
    client;
    criteria = None;
    next_batch = None;
    pages = 0;
    results = Observable.List.create [];
    state = Observable.Value.create (Idle { end_reached = false });
    last_error = Observable.Value.create None;
  }

let results t = t.results
let state t = t.state
let last_error t = t.last_error
let loaded_pages t = t.pages

let busy_error () =
  Matrix_client.Error.Json_error "search request already in progress"

let not_started_error () =
  Matrix_client.Error.Json_error "search has not been started"

let clear_results t =
  Observable.List.reconcile_by ~key:(fun _ -> ()) ~equal:( == ) t.results []

let request t criteria =
  Observable.Value.set t.last_error None;
  Observable.Value.set t.state Loading;
  let response =
    try Search.room_events t.client ~criteria ?next_batch:t.next_batch ()
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Observable.Value.set t.state (Idle { end_reached = false });
      Printexc.raise_with_backtrace exn bt
  in
  match response with
  | Error error ->
      Observable.Value.set t.last_error (Some error);
      Observable.Value.set t.state (Idle { end_reached = false });
      Error error
  | Ok page ->
      List.iter (Observable.List.append t.results) page.results;
      t.pages <- t.pages + 1;
      t.next_batch <- page.next_batch;
      Observable.Value.set t.state
        (Idle { end_reached = Option.is_none page.next_batch });
      Ok ()

let search t ~criteria =
  match Observable.Value.get t.state with
  | Loading -> Error (busy_error ())
  | Idle _ ->
      t.criteria <- Some criteria;
      t.next_batch <- None;
      t.pages <- 0;
      clear_results t;
      request t criteria

let next_page t =
  match Observable.Value.get t.state with
  | Loading -> Error (busy_error ())
  | Idle { end_reached = true } -> Ok ()
  | Idle { end_reached = false } -> (
      match t.criteria with
      | None -> Error (not_started_error ())
      | Some criteria -> request t criteria)
