let run store ~actor ~room ~event ~source ~call_id ~tool ~arguments f =
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
    else Plugin.clip ~bytes:4096 text
  in
  let id =
    Store.start_tool store ~actor ~room ~event ~source
      ~call_id:(Plugin.clip ~bytes:256 call_id)
      ~tool:(Plugin.clip ~bytes:64 tool)
      ~arguments:(payload arguments)
  in
  let finish ~status result =
    Eio.Cancel.protect (fun () ->
        Store.finish_tool store id ~status ~result:(payload result))
  in
  match f () with
  | Ok result ->
      let result = Plugin.clip ~bytes:4096 result in
      finish ~status:"ok" result;
      result
  | Error result ->
      finish ~status:"rejected" result;
      Plugin.clip ~bytes:4096 result
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      let status =
        match exn with
        | Eio.Cancel.Cancelled _ | Eio.Time.Timeout -> "cancelled"
        | _ -> "error"
      in
      finish ~status "Tool did not complete successfully.";
      Printexc.raise_with_backtrace exn bt

let line (use : Store.tool_use) =
  Printf.sprintf "#%d %s %s %s (%s, %s)\nArguments: %s\nResult: %s" use.log_id
    use.started_at use.tool use.status use.actor use.source use.arguments
    use.result
