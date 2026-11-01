module M = Siftbot_model

type preview = {
  source : M.source;
  source_revision : string;
  title : string;
  deadline : string option;
  area : string option;
  thread : string;
}

let preview (m : M.message) ~title ?deadline ?area () =
  if String.trim title = "" || String.length title > 1024 then
    invalid_arg "Invalid task title";
  Option.iter
    (fun value ->
      match Ptime.of_rfc3339 value with
      | Ok _ -> ()
      | Error _ -> invalid_arg "Use an RFC3339 task deadline")
    deadline;
  {
    source = m.source;
    source_revision = m.revision;
    title;
    deadline;
    area;
    thread = m.thread;
  }

let capture db ~revision ~dooit_root p =
  let message =
    match Store.get db p.source with
    | Some m when m.revision = p.source_revision && m.thread = p.thread -> m
    | _ -> raise Store.Conflict
  in
  ignore (preview message ~title:p.title ?deadline:p.deadline ?area:p.area ());
  Option.iter
    (fun id ->
      if
        not
          (List.exists
             (fun (a : M.area) -> a.id = id && a.status = "approved")
             (Store.snapshot db ~query:"").areas)
      then invalid_arg "Task area must be approved")
    p.area;
  let root = Unix.realpath dooit_root in
  let key =
    M.key { p.source with id = p.thread }
    ^ ":" ^ root ^ ":"
    ^ Option.value ~default:"" p.area
  in
  let source, title =
    Store.reserve_task db ~key ~revision ~source:p.source
      ~source_revision:p.source_revision ~title:p.title
  in
  let work () =
    let store = Dooit.Store.open_ root in
    let target =
      Dooit.Link.email ~service:source.service ~account:source.account
        ~email:source.id
    in
    let body =
      "Captured after Siftbot review.\nSource: " ^ M.key p.source
      ^ Option.fold ~none:""
          ~some:(fun date -> "\nRequested deadline: " ^ date)
          p.deadline
    in
    let tags = Option.to_list p.area in
    let due =
      Option.map
        (fun date ->
          let time =
            match Ptime.of_rfc3339 date with
            | Ok (t, _, _) -> t
            | Error _ -> invalid_arg "Invalid task deadline"
          in
          let y, m, d = Ptime.to_date time in
          Printf.sprintf "%04d-%02d-%02d" y m d)
        p.deadline
    in
    let notes = Dooit.Link.capture ~title ~tags ~body ?due store target in
    let ids = List.map Dooit.Doc.id notes in
    Store.task_receipt db ~key ~status:"captured"
      ~receipt:(Jmap_eio.Codec.encode_exn (Jsont.list Jsont.string) ids);
    ids
  in
  try work ()
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    Store.task_receipt db ~key ~status:"failed"
      ~receipt:"Capture failed; retry the reviewed source.";
    Printexc.raise_with_backtrace e bt
