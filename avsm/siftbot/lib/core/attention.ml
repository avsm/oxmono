module M = Siftbot_model

let contains text pattern =
  let n = String.length pattern in
  let rec loop i =
    i + n <= String.length text
    && (String.sub text i n = pattern || loop (i + 1))
  in
  loop 0

let infer ~self_addresses ms =
  if self_addresses = [] then []
  else
    let groups = Hashtbl.create 32 in
    List.iter
      (fun (m : M.message) ->
        let k = M.key { m.source with id = m.thread } in
        Hashtbl.replace groups k
          (m :: Option.value ~default:[] (Hashtbl.find_opt groups k)))
      ms;
    Hashtbl.to_seq_values groups
    |> List.of_seq
    |> List.concat_map (fun messages ->
           let ordered =
             List.sort
               (fun (a : M.message) b ->
                 let c = String.compare a.date b.date in
                 if c = 0 then String.compare (M.key a.source) (M.key b.source)
                 else c)
               messages
           in
           let self m =
             List.mem
               (String.lowercase_ascii m.M.sender)
               (List.map String.lowercase_ascii self_addresses)
           in
           let requests =
             List.filter
               (fun (m : M.message) ->
                 (not (self m))
                 && List.exists
                      (contains (String.lowercase_ascii m.text))
                      [
                        "please review";
                        "please reply";
                        "could you";
                        "can you";
                        "please submit";
                      ])
               ordered
           in
           List.map
             (fun (m : M.message) ->
               let reply =
                 List.find_opt
                   (fun (r : M.message) ->
                     r.date > m.date && self r
                     && List.exists
                          (fun id -> List.mem id r.reply_ids)
                          m.message_ids)
                   ordered
               in
               let value, explanation, evidence =
                 match reply with
                 | Some r ->
                     ( "waiting",
                       "An admin reply references the request. Verify whether \
                        it resolved it.",
                       [ m.source; r.source ] )
                 | None ->
                     ( "needs-reply",
                       "Explicit request without a later referencing admin \
                        reply: "
                       ^ Agentkit.Memo.clip 512 m.text,
                       [ m.source ] )
               in
               M.
                 {
                   target = m.source;
                   conversation = false;
                   dimension = Attention;
                   value;
                   origin = Rule;
                   version = "attention/v1";
                   source_revision = m.revision;
                   explanation;
                   evidence;
                   score = None;
                 })
             requests)

let queue db ~now =
  let now =
    match Ptime.of_rfc3339 now with
    | Ok (t, _, _) -> Ptime.to_rfc3339 ~tz_offset_s:0 t
    | Error _ -> invalid_arg "Use an RFC3339 time"
  in
  (Store.snapshot db ~query:"").assignments
  |> List.filter (fun (a : M.assignment) ->
         a.dimension = Attention
         && (not (List.mem a.value [ "fyi"; "resolved" ]))
         && not (Store.snoozed db ~now a.target))
