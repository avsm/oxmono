type result = [ `Accepted | `Stopped | `Invalid_token | `Malformed of string ]

let max_payload = 1024 * 1024
let max_depth = 64

let equal_token a b =
  let different = ref (String.length a lxor String.length b) in
  let longest = max (String.length a) (String.length b) in
  for i = 0 to longest - 1 do
    let char string i =
      if i < String.length string then Char.code string.[i] else 0
    in
    different := !different lor (char a i lxor char b i)
  done;
  !different = 0

let member name = function
  | Jsont.Object (members, _) ->
      Option.map snd (Jsont.Json.find_mem name members)
  | _ -> None

let rec bounded depth = function
  | Jsont.Array (values, _) ->
      depth < max_depth
      && List.length values <= 4096
      && List.for_all (bounded (depth + 1)) values
  | Jsont.Object (members, _) ->
      depth < max_depth
      && List.length members <= 4096
      && List.for_all (fun (_, value) -> bounded (depth + 1) value) members
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _ -> true

let handle ~token bot ~payload =
  if not (Bot.is_running bot) then `Stopped
  else if token = "" then `Invalid_token
  else if String.length payload > max_payload then
    `Malformed "outgoing webhook exceeds 1 MiB"
  else
    match Fetch.Json.decode_string' ~max_depth Jsont.json payload with
    | Error error -> `Malformed (Jsont.Error.to_string error)
    | Ok json when not (bounded 0 json) ->
        `Malformed "outgoing webhook exceeds structural limits"
    | Ok json -> (
        match
          (member "token" json, member "trigger" json, member "message" json)
        with
        | ( Some (Jsont.String (received, _)),
            Some (Jsont.String (trigger, _)),
            Some message )
          when equal_token token received -> (
            match Jsont.Json.decode Zulip.Message.jsont message with
            | Error error -> `Malformed error
            | Ok message -> (
                let flags =
                  match trigger with
                  | "mention" -> Some [ `Mentioned ]
                  | "direct_message" | "private_message" -> Some []
                  | _ -> None
                in
                match flags with
                | None -> `Malformed "unsupported outgoing webhook trigger"
                | Some flags -> (
                    try
                      Bot.dispatch bot
                        (Event.Message
                           (Event.of_message (Bot.context bot) ~flags message));
                      `Accepted
                    with Invalid_argument _ when not (Bot.is_running bot) ->
                      `Stopped)))
        | Some (Jsont.String _), _, _ -> `Invalid_token
        | _ ->
            `Malformed
              "outgoing webhook needs string token, trigger, and message")
