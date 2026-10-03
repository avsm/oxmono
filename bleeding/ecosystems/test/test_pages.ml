let fake ~total ~calls ~page ~per_page =
  incr calls;
  let page = int_of_string page and n = int_of_string per_page in
  let first = ((page - 1) * n) + 1 in
  List.init (max 0 (min n (total - first + 1))) (fun i -> first + i)

let run total per_page =
  let calls = ref 0 in
  let items =
    Ecosystems_client.pages ~per_page (fake ~total ~calls) |> List.of_seq
  in
  (items, !calls)

let () =
  assert (run 0 10 = ([], 1));
  assert (run 7 10 = (List.init 7 succ, 1));
  assert (run 20 10 = (List.init 20 succ, 3));
  assert (run 25 10 = (List.init 25 succ, 3));
  let calls = ref 0 in
  let seq = Ecosystems_client.pages ~per_page:10 (fake ~total:100 ~calls) in
  ignore (Seq.take 5 seq |> List.of_seq);
  assert (!calls = 1)
