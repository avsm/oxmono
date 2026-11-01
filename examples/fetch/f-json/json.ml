type todo = { id : int; title : string; done_ : bool }

let todo_jsont =
  Jsont.Object.map ~kind:"Todo" (fun id title done_ -> { id; title; done_ })
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun t -> t.id)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun t -> t.title)
  |> Jsont.Object.mem "done" Jsont.bool ~enc:(fun t -> t.done_) ~dec_absent:(fun () -> false)
  |> Jsont.Object.finish

let todo = Fetch.Json.v todo_jsont
let todo_lines = Fetch.Json.lines todo_jsont

let show t =
  Printf.printf "Todo %d: %s%s\n" t.id t.title (if t.done_ then " (done)" else "")

let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in

  (match Fetch.read_as client todo (base ^ "/todo") with
   | Ok t -> show t
   | Error r -> Printf.printf "The server said %d\n" (Fetch.status r));

  (match Fetch.read_as client todo (base ^ "/todo/2") with
   | Ok t -> show t
   | Error r ->
     Printf.printf "The server said %d: %s" (Fetch.status r)
       (Fetch.decode Fetch.Media.text r));

  Eio.Switch.run @@ fun sw ->
  let headers, body = Fetch.encode todo { id = 2; title = "ship it"; done_ = false } in
  let response = Fetch.post ~sw ~headers ~body client (base ^ "/echo") in
  print_string "Echoed back: ";
  show (Fetch.decode todo response);

  let response = Fetch.get ~sw client (base ^ "/todos.jsonl") in
  Fetch.decode_seq todo_lines response
  |> Seq.filter (fun t -> not t.done_)
  |> Seq.iter (fun t -> Printf.printf "Still to do: %s\n" t.title)
