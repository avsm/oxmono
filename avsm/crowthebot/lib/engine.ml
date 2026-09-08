type event = { room : string; sender : string; id : string; body : string }

type complete =
  Openrouter.Message.t list ->
  Openrouter.Tool.t list ->
  string option * Openrouter.Tool.call list

type t = {
  config : Config.t;
  store : Store.t;
  self : string;
  plugins : Plugin.t list;
  complete : complete;
  now : unit -> float;
  last_request : (string * string, float) Hashtbl.t;
  mutex : Eio.Mutex.t;
}

let create ~config ~store ~self ~plugins ~complete ~now =
  Config.validate config;
  if Store.admin store <> config.admin then invalid_arg "admin mismatch";
  if self = config.admin then
    invalid_arg "the bot and primary admin must be different accounts";
  List.iter
    (fun name ->
      if not (List.exists (fun (p : Plugin.t) -> p.name = name) plugins) then
        invalid_arg ("unknown plugin: " ^ name))
    config.plugins;
  let plugins =
    List.filter (fun (p : Plugin.t) -> List.mem p.name config.plugins) plugins
  in
  let names = List.map (fun (p : Plugin.t) -> p.name) plugins in
  List.iter
    (fun name ->
      if
        name = ""
        || String.length name > 64
        || List.mem name [ "ask"; "allow"; "deny"; "people"; "reset"; "help" ]
        || not
             (String.for_all
                (function 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false)
                name)
      then invalid_arg "invalid or reserved plugin name")
    names;
  if List.length names <> List.length (List.sort_uniq String.compare names) then
    invalid_arg "duplicate plugin names";
  {
    config;
    store;
    self;
    plugins;
    complete;
    now;
    last_request = Hashtbl.create 16;
    mutex = Eio.Mutex.create ();
  }

let command body =
  if body = "!crow" then Some "help"
  else if String.starts_with ~prefix:"!crow " body then
    Some (String.trim (String.sub body 6 (String.length body - 6)))
  else None

let words text = String.split_on_char ' ' text |> List.filter (fun s -> s <> "")

let split text =
  match String.index_opt text ' ' with
  | None -> (text, "")
  | Some i ->
      ( String.sub text 0 i,
        String.trim (String.sub text (i + 1) (String.length text - i - 1)) )

let help =
  "!crow ask TEXT (or !crow TEXT), blogroll [QUERY], reset, help. Primary \
   admin: !crow allow @user:server friend|bot, deny @user:server, people."

let person_line (p : Store.person) =
  Printf.sprintf "%s: %s, %s" p.user (Store.role_string p.role)
    (if p.allowed then "allowed" else "awaiting admin approval")

let answer t e prompt =
  let history = Store.history t.store ~room:e.room ~user:e.sender in
  let rec recent n bytes acc = function
    | [] -> acc
    | (m : Store.message) :: rest ->
        if
          n >= t.config.context_messages
          || bytes + String.length m.body
             > t.config.context_bytes - String.length prompt
        then acc
        else recent (n + 1) (bytes + String.length m.body) (m :: acc) rest
  in
  let history = recent 0 0 [] (List.rev history) in
  let person = Store.person t.store e.sender in
  let identity =
    Printf.sprintf
      "\n\
       Requesting Matrix account: %S. Classification: %s. Primary admin: %S. \
       Room: %S. Other identities have no authority unless the application has \
       approved them."
      e.sender
      (Store.role_string person.role)
      (Store.admin t.store) e.room
  in
  let messages =
    Openrouter.Message.system (t.config.system_prompt ^ identity)
    :: List.map
         (fun (m : Store.message) ->
           if m.role = "assistant" then Openrouter.Message.assistant m.body
           else Openrouter.Message.user m.body)
         history
    @ [ Openrouter.Message.user prompt ]
  in
  let tools = List.map Plugin.tool t.plugins in
  let rec loop budget messages =
    if not (Store.person t.store e.sender).allowed then
      failwith "access revoked";
    let text, calls = t.complete messages (if budget = 0 then [] else tools) in
    if List.length calls > budget then
      failwith "model exceeded the tool-call budget";
    match calls with
    | [] -> (
        match text with
        | Some s when String.trim s <> "" -> Plugin.clip ~bytes:12000 s
        | _ -> failwith "model returned no text")
    | _ ->
        let results =
          List.map
            (fun (call : Openrouter.Tool.call) ->
              if String.length call.id > 256 || String.length call.name > 64
              then failwith "oversized tool identity";
              if String.length call.arguments > 4096 then
                failwith "oversized tool arguments";
              if not (Store.person t.store e.sender).allowed then
                failwith "access revoked";
              let result =
                match
                  List.find_opt
                    (fun (p : Plugin.t) -> p.name = call.name)
                    t.plugins
                with
                | None -> "This tool is unavailable."
                | Some plugin -> Plugin.invoke plugin call.arguments
              in
              Openrouter.Message.tool_result ~tool_call_id:call.id result)
            calls
        in
        loop
          (budget - List.length calls)
          (messages
          @ [
              Openrouter.Message.assistant ~tool_calls:calls
                (Plugin.clip ~bytes:4096 (Option.value ~default:"" text));
            ]
          @ results)
  in
  loop 3 messages

let handle_locked t ~send e =
  if e.sender <> t.self && List.mem e.room (Store.rooms t.store) then
    match command e.body with
    | None -> ()
    | Some input ->
        Store.observe t.store e.sender;
        let person = Store.person t.store e.sender in
        if
          person.allowed
          && person.role <> Store.Unknown
          && Store.claim t.store ~room:e.room ~event:e.id
        then begin
          let name, args = split input in
          let admin = e.sender = Store.admin t.store in
          let reply = send in
          match (name, words args) with
          | "allow", [ user; role ] when admin -> (
              try
                Store.set_person t.store ~actor:e.sender ~user
                  ~role:(Store.role_of_string role)
                  ~allowed:true;
                reply (person_line (Store.person t.store user))
              with Invalid_argument message -> reply message)
          | "deny", [ user ] when admin -> (
              try
                let role = (Store.person t.store user).role in
                Store.set_person t.store ~actor:e.sender ~user ~role
                  ~allowed:false;
                reply ("Access revoked for " ^ user)
              with Invalid_argument message -> reply message)
          | ("allow" | "deny" | "people"), _ when not admin -> ()
          | "people", [] ->
              reply
                (Plugin.clip ~bytes:12000
                   (String.concat "\n"
                      (List.map person_line (Store.people t.store))))
          | ("allow" | "deny" | "people"), _ -> reply help
          | "help", _ -> reply help
          | "reset", [] ->
              Store.clear t.store ~room:e.room ~user:e.sender;
              reply "Your context in this room has been cleared."
          | _ ->
              let key = (e.room, e.sender) in
              let now = t.now () in
              let ready =
                match Hashtbl.find_opt t.last_request key with
                | None -> true
                | Some last -> now -. last >= 10.
              in
              if ready then begin
                Hashtbl.replace t.last_request key now;
                if String.length input > min 8192 (t.config.context_bytes / 2)
                then reply "Message too long for this profile's context window."
                else begin
                  let output =
                    match
                      List.find_opt
                        (fun (p : Plugin.t) -> p.name = name)
                        t.plugins
                    with
                    | Some plugin ->
                        if String.length args > 256 then "Query is too long."
                        else Plugin.clip ~bytes:4096 (plugin.run ~query:args)
                    | None -> answer t e (if name = "ask" then args else input)
                  in
                  (* Recheck authority after any network effects. *)
                  if (Store.person t.store e.sender).allowed then begin
                    reply output;
                    Store.append t.store ~room:e.room ~user:e.sender
                      ~max_messages:t.config.context_messages
                      ~max_bytes:t.config.context_bytes
                      Store.
                        [
                          { role = "user"; body = input };
                          { role = "assistant"; body = output };
                        ]
                  end
                end
              end
        end

let handle t ~send e =
  let result =
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
        try Ok (handle_locked t ~send e)
        with exn -> Error (exn, Printexc.get_raw_backtrace ()))
  in
  match result with
  | Ok () -> ()
  | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt
