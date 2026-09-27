type t = {
  resources : Client.t Eio.Pool.t;
  max_connections : int;
  closed : bool ref;
}

exception Connect_failed of Error.t

let create ~sw ~max_connections ~connect =
  if max_connections < 1 then invalid_arg "IMAP pool size must be positive";
  let closed=ref false in
  Eio.Switch.on_release sw (fun () -> closed:=true);
  let alloc () =
    match connect ~sw with
    | Ok client when Client.is_open client -> client
    | Ok client -> Client.close client; raise (Connect_failed Error.Closed)
    | Error error -> raise (Connect_failed error) in
  let resources=Eio.Pool.create
    ~validate:Client.is_open ~dispose:Client.close max_connections alloc in
  {resources;max_connections;closed}

let max_connections t = t.max_connections

let use t callback =
  if !(t.closed) then Error Error.Closed
  else
    try Eio.Pool.use t.resources (fun client ->
      try
        let result=callback client in
        (match result with
         | Error (Error.Uncertain _ | Error.Protocol _ | Error.Transport _) ->
             Client.close client
         | _ -> ());
        result
      with ex ->
        Client.close client;
        raise ex)
    with Connect_failed error -> Error error
