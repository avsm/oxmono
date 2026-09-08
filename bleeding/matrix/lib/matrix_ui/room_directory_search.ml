module Directory = Matrix_client.Directory

type state =
  | Start
  | Loading
  | Next of string
  | End
  | Failed of Matrix_client.Error.t

type t = {
  client : Matrix_client.Client.t;
  mutex : Eio.Mutex.t;
  mutable filter : string option;
  mutable batch_size : int;
  mutable via_server : string option;
  mutable next_token : string option;
  mutable terminal : bool;
  results : Directory.room_summary Observable.List.t;
  state : state Observable.Value.t;
}

let create ~client () =
  {
    client;
    mutex = Eio.Mutex.create ();
    filter = None;
    batch_size = 0;
    via_server = None;
    next_token = None;
    terminal = false;
    results = Observable.List.create [];
    state = Observable.Value.create Start;
  }

let results t = t.results
let state t = t.state

let loaded_pages t =
  if t.batch_size = 0 then 0
  else
    let count = Observable.List.length t.results in
    (count + t.batch_size - 1) / t.batch_size

let is_at_last_page t = t.terminal

let clear_results t =
  Observable.List.reconcile_by
    ~key:(fun (room : Directory.room_summary) -> room.room_id)
    ~equal:( = ) t.results []

let request t ~rollback =
  Observable.Value.set t.state Loading;
  let filter =
    Option.map
      (fun generic_search_term ->
        Directory.
          { generic_search_term = Some generic_search_term; room_types = None })
      t.filter
  in
  let response =
    try
      Directory.search_public_rooms t.client ?server:t.via_server
        ~limit:t.batch_size ?from:t.next_token ?filter ()
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Observable.Value.set t.state rollback;
      Printexc.raise_with_backtrace exn bt
  in
  match response with
  | Error error ->
      Observable.Value.set t.state (Failed error);
      Error error
  | Ok response ->
      List.iter (Observable.List.append t.results) response.page.chunk;
      t.next_token <- response.page.next_batch;
      t.terminal <- Option.is_none t.next_token;
      Observable.Value.set t.state
        (match t.next_token with Some token -> Next token | None -> End);
      Ok ()

let search t ?filter ~batch_size ?via_server () =
  if batch_size < 1 then
    invalid_arg "Matrix_ui.Room_directory_search.search: batch_size";
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.filter <- filter;
      t.batch_size <- batch_size;
      t.via_server <- via_server;
      t.next_token <- None;
      t.terminal <- false;
      clear_results t;
      request t ~rollback:Start)

let next_page t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      if t.terminal then Ok ()
      else if t.batch_size = 0 then
        Error
          (Matrix_client.Error.Json_error
             "room-directory search has not been started")
      else
        let rollback = Observable.Value.get t.state in
        request t ~rollback)
