open Proffer
open Proffer.Route

type todo = { id : int; title : string; done_ : bool }

let todo_jsont =
  Jsont.Object.map ~kind:"Todo" (fun id title done_ -> { id; title; done_ })
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun t -> t.id)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun t -> t.title)
  |> Jsont.Object.mem "done" Jsont.bool ~enc:(fun t -> t.done_) ~dec_absent:(fun () -> false)
  |> Jsont.Object.finish

type env = {
  store : (int, todo) Hashtbl.t;
  todo : todo Media.t;
  todos : todo list Media.t;
  todo_lines : todo Media.seq;
}

let all env =
  Hashtbl.to_seq_values env.store
  |> List.of_seq
  |> List.sort (fun a b -> compare a.id b.id)

let site =
  Site.of_routes
    [ get (s "todos") (fun env _request respond ->
        Resp.encode respond env.todos (all env));

      get (s "todos" / s "export") (fun env _request respond ->
        Resp.encode_seq respond env.todo_lines (List.to_seq (all env)));

      get (s "todos" / int) (fun id env _request respond ->
        match Hashtbl.find_opt env.store id with
        | Some t -> Resp.encode respond env.todo t
        | None -> Resp.not_found respond ());

      post (s "todos") (with_body (fun env -> env.todo) (fun t env _request respond ->
        Hashtbl.replace env.store t.id t;
        Resp.encode respond ~status:Created env.todo t)) ]

let () =
  let store = Hashtbl.create 8 in
  Hashtbl.replace store 1 { id = 1; title = "write the tutorial"; done_ = true };
  Eio_main.run @@ fun stdenv ->
  let env =
    { store;
      todo = Json.v todo_jsont;
      todos = Json.v (Jsont.list todo_jsont);
      todo_lines = Json.lines todo_jsont }
  in
  Proffer_httpz.run stdenv ~env site
