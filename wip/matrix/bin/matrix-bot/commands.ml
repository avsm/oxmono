open Matrix_bot

let random bot =
  Matrix_client.Client.random
    (Matrix_eio.Client.base (Context.client (Bot.context bot)))

(* [NdM] is N dice of M sides; both default, so [!roll] alone is [1d6]. *)
let dice argument =
  let argument = if argument = "" then "1d6" else argument in
  let count, sides =
    match String.index_opt argument 'd' with
    | None -> (argument, "6")
    | Some index ->
        ( (if index = 0 then "1" else String.sub argument 0 index),
          String.sub argument (index + 1) (String.length argument - index - 1)
        )
  in
  match (int_of_string_opt count, int_of_string_opt sides) with
  | Some count, Some sides
    when count >= 1 && count <= 20 && sides >= 2 && sides <= 1000 ->
      Some (count, sides)
  | _ -> None

(* The dice come from the same secure source the transaction ids do, so the
   bot needs no other randomness. Two bytes a die folded modulo the number
   of sides: the bias is under one part in 32768 and this is a toy. *)
let roll bot (c : Event.command) =
  match dice c.args with
  | None ->
      ignore
        (Event.reply c.message.envelope
           (c.args
          ^ " is not a roll I understand; try 2d6 (up to 20 dice of up to 1000 \
             sides)"))
  | Some (count, sides) ->
      let bytes = Matrix_client.Random.generate (random bot) (2 * count) in
      let rolls =
        List.init count (fun index ->
            ((Char.code bytes.[2 * index] * 256)
            + Char.code bytes.[(2 * index) + 1])
            mod sides
            + 1)
      in
      let total = List.fold_left ( + ) 0 rolls in
      ignore
        (Event.reply c.message.envelope
           (if count = 1 then Printf.sprintf "%dd%d: %d" count sides total
            else
              Printf.sprintf "%dd%d: %s = %d" count sides
                (String.concat " + " (List.map string_of_int rolls))
                total))

let topic _ (c : Event.command) =
  if String.equal c.args "" then
    ignore (Event.reply c.message.envelope "!topic needs some text")
  else
    match Room.set_topic c.message.envelope.room c.args with
    | Ok () -> ignore (Event.react c.message.envelope "\xe2\x9c\x85" (* ✅ *))
    | Error error ->
        ignore
          (Event.reply c.message.envelope
             (Format.asprintf "I cannot set the topic: %a"
                Matrix_client.Error.pp error))

let plugin spec =
  spec
  |> Bot.command ~name:"ping" ~doc:"answer, as a reply to the command"
       (fun _ c -> ignore (Event.reply c.message.envelope "pong"))
  |> Bot.command ~name:"roll" ~args:"NdM" ~doc:"roll N dice of M sides" roll
  |> Bot.command ~name:"react" ~doc:"react to the command with a thumbs up"
       (fun _ c ->
         ignore (Event.react c.message.envelope "\xf0\x9f\x91\x8d" (* 👍 *)))
  |> Bot.command ~name:"topic" ~args:"<text>" ~doc:"set the room topic"
       ~admin:true topic
  |> Bot.help
