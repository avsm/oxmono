type t = {
  dispatch :
    check:(unit -> unit) ->
    actor:string ->
    room:string ->
    event:string ->
    source_event:string ->
    call_id:string ->
    string ->
    string ->
    (string, string) result;
  allowed : actor:string -> room:string -> bool;
}

let system_prompt =
  Dooit_agent.system_prompt
  ^ " Memory holds background context and task references, not a second task \
     list. Cron schedules actions and reminders, not task state. Do not copy \
     existing memory into Dooit unless explicitly asked. With the Local \
     backend, write results say sync=local_only and must not be described as \
     remotely synchronized. Crow supports WebDAV task reads, but WebDAV task \
     writes are unavailable. Report that limitation instead of using memory."

let unavailable_prompt =
  "\n\
   Dooit is the primary task store, but its tools are unavailable. For task \
   requests, explain that an existing Dooit store must be configured. Do not \
   use memory as a substitute task list or claim a task was recorded. Cron \
   remains available for explicitly scheduled actions and reminders."

let operation ~store_id ~profile ~actor ~room ~event ~source_event ~call_id name
    =
  let fields =
    [
      "crowthebot.dooit/v1";
      profile;
      actor;
      room;
      event;
      source_event;
      call_id;
      name;
    ]
  in
  let key =
    String.concat ""
      (List.map (fun s -> string_of_int (String.length s) ^ ":" ^ s) fields)
  in
  Dooit.Common.uuid5 store_id key

let dispatch ~tools ~store_id ~profile ~check ~actor ~room ~event ~source_event
    ~call_id name arguments =
  let operation =
    operation ~store_id ~profile ~actor ~room ~event ~source_event ~call_id name
  in
  Dooit_agent.invoke tools ~check ~operation name arguments

let create ~tools ~store_id ~profile ~allowed =
  { dispatch = dispatch ~tools ~store_id ~profile; allowed }

let available t ~actor ~room = t.allowed ~actor ~room

let invoke t ~actor ~room ~event ~source_event ~call_id ~check name arguments =
  let check () =
    check ();
    if not (available t ~actor ~room) then
      failwith "Task access denied for this actor and room"
  in
  try
    check ();
    let result =
      t.dispatch ~check ~actor ~room ~event ~source_event ~call_id name
        arguments
    in
    check ();
    result
  with
  | Dooit.Common.Error message | Invalid_argument message | Failure message ->
    Error message

let existing ~fs =
  Eio_unix.run_in_systhread ~label:"crow.tasks.config" (fun () ->
      let config = Dooit.Config.load ~fs () in
      let path = Filename.concat config.root "store.json" in
      let exists =
        try
          ignore (Unix.lstat path);
          true
        with Unix.Unix_error (Unix.ENOENT, _, _) -> false
      in
      if not exists then None
      else Some (Dooit.Store.open_ config.root, config.backend, config.git))

let load ~fs ~process_mgr ~store ~profile ~max_bytes () =
  if max_bytes < 1024 then invalid_arg "Dooit tool budget must be at least 1024";
  try
    match existing ~fs with
    | None -> None
    | Some _ ->
        let dispatch ~check ~actor ~room ~event ~source_event ~call_id name
            arguments =
          check ();
          let current = existing ~fs in
          check ();
          match current with
          | None -> Error "Dooit task store is unavailable"
          | Some (current, backend, git) ->
              let write = name = "dooit_create" || name = "dooit_update" in
              if backend = Dooit.Config.Webdav && write then
                Error
                  "Crow does not support task writes with the WebDAV backend. \
                   Use the Dooit CLI with the configured backend."
              else
                let tools =
                  match backend with
                  | Dooit.Config.Git ->
                      Dooit_agent.create ~process_mgr ~git ~store:current
                        ~max_bytes
                  | Dooit.Config.Local | Dooit.Config.Webdav ->
                      Dooit_agent.create_local ~store:current ~max_bytes
                in
                let result =
                  dispatch ~tools ~store_id:current.id ~profile ~check ~actor
                    ~room ~event ~source_event ~call_id name arguments
                in
                if backend = Dooit.Config.Local && write then
                  Result.map
                    (fun result ->
                      let open Dooit.Common in
                      json result
                      |> set "sync" (str "local_only")
                      |> json_string)
                    result
                else result
        in
        Some { dispatch; allowed = Knowledge.authorized store }
  with Dooit.Common.Error message -> failwith message
