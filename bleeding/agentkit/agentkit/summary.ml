type failure = Json | Empty | Oversized | Length | Tools

exception Failed of failure

let failure_name = function
  | Json -> "invalid summary JSON"
  | Empty -> "empty summary"
  | Oversized -> "summary exceeds byte limit"
  | Length -> "model output token limit reached"
  | Tools -> "summary requested unavailable tools"

let json_object text =
  match (String.index_opt text '{', String.rindex_opt text '}') with
  | Some i, Some j when i < j -> String.sub text i (j - i + 1)
  | _ -> text

let codec =
  Jsont.Object.map Fun.id
  |> Jsont.Object.mem "summary" Jsont.string ~enc:Fun.id
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let run ~complete ~instructions ~limit ?(max_tokens = 4096)
    ?(reasoning = Some "none") ?(on_retry = fun _ ~words:_ -> ()) input =
  if limit < 1 then invalid_arg "Agentkit.Summary.run: nonpositive limit";
  let attempt words =
    let (r : Chat.response) =
      complete
        (Chat.request ~max_tokens ?reasoning
           [ Chat.System (instructions ~words); Chat.User input ])
    in
    if r.calls <> [] then raise (Failed Tools);
    if r.finish = Some Chat.Length then raise (Failed Length);
    let body =
      match
        Jsont_bytesrw.decode_string codec
          (json_object (Option.value ~default:"" r.text))
      with
      | Ok body -> body
      | Error _ -> raise (Failed Json)
    in
    if String.trim body = "" then raise (Failed Empty);
    if String.length body > limit then raise (Failed Oversized);
    body
  in
  let words = max 64 (limit / 10) in
  try attempt words
  with Failed ((Json | Empty | Oversized | Length) as failure) ->
    let words = words / 2 in
    on_retry failure ~words;
    attempt words
