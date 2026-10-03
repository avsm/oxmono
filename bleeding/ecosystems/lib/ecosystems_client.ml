let pages ?(per_page = 100) f =
  let rec from page () =
    match f ~page:(string_of_int page) ~per_page:(string_of_int per_page) with
    | [] -> Seq.Nil
    | items ->
        let rest =
          if List.length items < per_page then Seq.empty else from (page + 1)
        in
        Seq.append (List.to_seq items) rest ()
  in
  from 1
