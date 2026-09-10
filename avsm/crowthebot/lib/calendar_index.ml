let text json =
  let buffer = Buffer.create 1024 in
  let rec collect = function
    | Jsont.String (text, _) ->
        Buffer.add_string buffer text;
        Buffer.add_char buffer '\n'
    | Jsont.Array (values, _) -> List.iter collect values
    | Jsont.Object (fields, _) ->
        List.iter (fun (_, value) -> collect value) fields
    | _ -> ()
  in
  collect json;
  Buffer.contents buffer

let blobs json =
  let rec collect found = function
    | Jsont.Object (values, _) ->
        let found =
          match
            List.assoc_opt "blobId"
              (List.map (fun ((k, _), v) -> (k, v)) values)
          with
          | Some (Jsont.String (blob, _)) -> blob :: found
          | _ -> found
        in
        List.fold_left
          (fun found (_, value) -> collect found value)
          found values
    | Jsont.Array (values, _) -> List.fold_left collect found values
    | _ -> found
  in
  collect [] json |> List.sort_uniq String.compare
