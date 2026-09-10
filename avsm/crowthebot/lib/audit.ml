type summary = {
  log_id : int;
  tool : string;
  argument_bytes : int;
  result_bytes : int;
  status : string;
}

let summary_line s =
  Printf.sprintf "%s(log_id=%d status=%s argument_bytes=%d result_bytes=%d)"
    s.tool s.log_id s.status s.argument_bytes s.result_bytes

let tool_name name =
  if
    String.length name <= 64
    && String.for_all
         (function
           | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false)
         name
  then name
  else "[invalid-tool-name]"

let run ?(on_finish = fun _ -> ()) store ~actor ~room ~event ~source ~call_id
    ~tool ~arguments f =
  let memory =
    tool = "memory"
    || String.starts_with ~prefix:"memory_" tool
    || tool = "cron"
    || String.starts_with ~prefix:"cron_" tool
  in
  let payload text =
    if memory then "[memory content omitted]"
    else if tool = "location" || String.starts_with ~prefix:"location_" tool
    then "[location content omitted]"
    else if
      String.starts_with ~prefix:"calendar_" tool
      || String.starts_with ~prefix:"caldav_" tool
    then "[calendar content omitted]"
    else if String.starts_with ~prefix:"email_" tool then
      "[email content omitted]"
    else Plugin.clip ~bytes:4096 text
  in
  let id =
    Store.start_tool store ~actor ~room ~event ~source
      ~call_id:(Plugin.clip ~bytes:256 call_id)
      ~tool:(Plugin.clip ~bytes:64 tool)
      ~arguments:(payload arguments)
  in
  Diagnostics.Tools.info (fun m ->
      m
        "Tool started log_id=%d tool=%S source=%S actor=%S room=%S event=%S \
         argument_bytes=%d"
        id (tool_name tool) source actor room event (String.length arguments));
  let finish ~status ~result_bytes result =
    Eio.Cancel.protect (fun () ->
        Store.finish_tool store id ~status ~result:(payload result));
    Diagnostics.Tools.info (fun m ->
        m
          "Tool finished log_id=%d tool=%S event=%S status=%s \
           argument_bytes=%d result_bytes=%d"
          id (tool_name tool) event status (String.length arguments)
          result_bytes);
    on_finish
      {
        log_id = id;
        tool = tool_name tool;
        argument_bytes = String.length arguments;
        result_bytes;
        status;
      }
  in
  match f () with
  | Ok result ->
      let result_bytes = String.length result in
      let result = Plugin.clip ~bytes:4096 result in
      finish ~status:"ok" ~result_bytes result;
      result
  | Error result ->
      finish ~status:"rejected" ~result_bytes:(String.length result) result;
      Plugin.clip ~bytes:4096 result
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      Diagnostics.Tools.err (fun m ->
          m "Tool failed log_id=%d error=%s" id (Diagnostics.error exn));
      let status =
        match exn with
        | Eio.Cancel.Cancelled _ | Eio.Time.Timeout -> "cancelled"
        | _ -> "error"
      in
      finish ~status ~result_bytes:0 "Tool did not complete successfully.";
      Printexc.raise_with_backtrace exn bt

let line (use : Store.tool_use) =
  Printf.sprintf "#%d %s %s %s (%s, %s)\nArguments: %s\nResult: %s" use.log_id
    use.started_at use.tool use.status use.actor use.source use.arguments
    use.result
