open Persistence

type context = {
  actor : string;
  room : string;
  event : string;
  source_event : string;
  source : string;
}

let key : context Eio.Fiber.key = Eio.Fiber.create_key ()
let with_context context f = Eio.Fiber.with_binding key context f
let current () = Eio.Fiber.get key

type t = { db : Sqlite3_eio.t; mutex : Eio.Mutex.t; now : unit -> string }

let create ~db ~mutex ~now = { db; mutex; now }

let init db ~now =
  sql db
    {|CREATE TABLE IF NOT EXISTS model_traces (
id INTEGER PRIMARY KEY AUTOINCREMENT,
started_at TEXT NOT NULL, finished_at TEXT,
actor TEXT NOT NULL, room TEXT NOT NULL, event TEXT NOT NULL,
source_event TEXT NOT NULL, source TEXT NOT NULL,
request TEXT NOT NULL, response TEXT NOT NULL DEFAULT '',
http_status INTEGER, status TEXT NOT NULL, error TEXT NOT NULL DEFAULT '');
CREATE INDEX IF NOT EXISTS model_traces_event ON model_traces(event,id);|};
  execute db
    "UPDATE model_traces SET status='interrupted',finished_at=? WHERE \
     status='running'"
    [ text now ]

let start t request =
  let context =
    Option.value
      ~default:
        {
          actor = "";
          room = "local";
          event = "";
          source_event = "";
          source = "local";
        }
      (Eio.Fiber.get key)
  in
  locked t.mutex @@ fun () ->
  execute t.db
    "INSERT INTO \
     model_traces(started_at,actor,room,event,source_event,source,request,status) \
     VALUES(?,?,?,?,?,?,?,'running')"
    [
      text (t.now ());
      text context.actor;
      text context.room;
      text context.event;
      text context.source_event;
      text context.source;
      text request;
    ];
  last_id t.db

let finish t id ~response ~http_status ~status ~error =
  Eio.Cancel.protect (fun () ->
      locked t.mutex @@ fun () ->
      execute t.db
        "UPDATE model_traces SET \
         finished_at=?,response=?,http_status=?,status=?,error=? WHERE id=?"
        [
          text (t.now ());
          text response;
          optional integer http_status;
          text status;
          text error;
          integer id;
        ])

let wrap t client =
  let module M = Fetch.Middleware in
  M.middleware
    (fun next ~sw (request : M.request) ->
      let body =
        match request.body with
        | Fetch.String body -> body
        | Empty -> ""
        | Stream _ ->
            invalid_arg "Model tracing requires a replayable request body"
      in
      let id = start t body in
      Diagnostics.Log.info (fun m -> m "OpenRouter trace started id=%d" id);
      let received = Buffer.create 4096 in
      let status = ref None in
      try
        let response = next ~sw request in
        Fun.protect ~finally:(fun () -> M.close response) @@ fun () ->
        status := Some (M.status response);
        let chunk = Cstruct.create 4096 in
        let limit = 1024 * 1024 in
        let rec read () =
          match Eio.Flow.single_read (M.body response) chunk with
          | n ->
              let remaining = limit - Buffer.length received in
              Buffer.add_string received
                (Cstruct.to_string ~len:(min n remaining) chunk);
              if n > remaining then raise Eio.Buf_read.Buffer_limit_exceeded;
              read ()
          | exception End_of_file -> ()
        in
        read ();
        let body = Buffer.contents received in
        finish t id ~response:body ~http_status:!status
          ~status:
            (if M.status response >= 200 && M.status response < 300 then "ok"
             else "http-error")
          ~error:"";
        Diagnostics.Log.info (fun m ->
            m "OpenRouter trace completed id=%d http_status=%d" id
              (M.status response));
        M.Pi.response ~status:(M.status response) ~headers:(M.headers response)
          ~version:(M.version response)
          ~body:(Eio.Flow.string_source body)
          ~close:(fun () -> ())
          ~url:(M.effective_url response) ~scope:(M.scope response)
          ~sensitive:(M.sensitive response) ()
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        let category =
          match exn with
          | Eio.Buf_read.Buffer_limit_exceeded -> "response-too-large"
          | Eio.Cancel.Cancelled _ -> "cancelled"
          | _ -> "error"
        in
        finish t id ~response:(Buffer.contents received) ~http_status:!status
          ~status:category ~error:(Diagnostics.error exn);
        Printexc.raise_with_backtrace exn bt)
    client
