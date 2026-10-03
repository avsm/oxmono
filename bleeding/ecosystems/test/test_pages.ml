let fake ~cap ~total ~calls ~page ~per_page =
  incr calls;
  let page = int_of_string page and n = min cap (int_of_string per_page) in
  let first = ((page - 1) * n) + 1 in
  List.init (max 0 (min n (total - first + 1))) (fun i -> first + i)

let run ?cap total per_page =
  let calls = ref 0 in
  let items =
    let cap = Option.value cap ~default:max_int in
    Ecosystems_client.pages ~per_page (fake ~cap ~total ~calls) |> List.of_seq
  in
  (items, !calls)

let () =
  (* Iteration ends on the first empty page. *)
  assert (run 0 10 = ([], 1));
  assert (run 7 10 = (List.init 7 succ, 2));
  assert (run 20 10 = (List.init 20 succ, 3));
  assert (run 25 10 = (List.init 25 succ, 4));
  (* A server that returns fewer items than asked for loses nothing. *)
  assert (run ~cap:4 8 10 = (List.init 8 succ, 3));
  let calls = ref 0 in
  let seq =
    Ecosystems_client.pages ~per_page:10 (fake ~cap:max_int ~total:100 ~calls)
  in
  ignore (Seq.take 5 seq |> List.of_seq);
  assert (!calls = 1);
  assert (
    match
      Seq.is_empty
        (Ecosystems_client.pages ~per_page:0
           (fake ~cap:max_int ~total:1 ~calls))
    with
    | exception Invalid_argument _ -> true
    | _ -> false)
