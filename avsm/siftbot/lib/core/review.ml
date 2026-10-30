module M = Siftbot_model

let prefix limit s =
  if String.length s <= limit then s
  else
    let rec boundary i =
      if i > 0 && Char.code s.[i] land 0xc0 = 0x80 then boundary (i - 1) else i
    in
    String.sub s 0 (boundary limit)

let snapshot db ~query ?selected () =
  let snapshot = Store.snapshot ~visible_messages_only:true db ~query in
  let selected =
    match selected with
    | Some source
      when List.exists
             (fun (m : M.message) -> m.source = source)
             snapshot.messages ->
        Some source
    | _ ->
        Option.map
          (fun (m : M.message) -> m.source)
          (List.nth_opt snapshot.messages 0)
  in
  let messages =
    List.map
      (fun (m : M.message) ->
        let original =
          if selected = Some m.source then prefix (256 * 1024) m.original
          else ""
        in
        {
          m with
          original;
          text = "";
          headers =
            (try [ ("list-id", M.rule_key M.Mailing_list m) ]
             with Invalid_argument _ -> []);
          recipients = [];
          message_ids = [];
          reply_ids = [];
          subject = prefix 2048 m.subject;
          sender = prefix 2048 m.sender;
          truncated =
            m.truncated
            || selected = Some m.source
               && String.length original < String.length m.original;
        })
      snapshot.messages
  in
  let assignments =
    List.filter
      (fun (a : M.assignment) ->
        List.exists
          (fun (m : M.message) ->
            a.target = m.source
            || (a.conversation && a.target = { m.source with id = m.thread }))
          messages)
      snapshot.assignments
  in
  {
    snapshot with
    messages;
    assignments;
    discussion = prefix (64 * 1024) snapshot.discussion;
  }
