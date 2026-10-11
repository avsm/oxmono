module M = Siftbot_model

type provider = {
  model : string;
  dimensions : int;
  max_batch : int;
  embed : check:(unit -> unit) -> string list -> float list list;
}

let normalization = "passage-8192-l2/v1"

let checked provider values =
  if
    List.length values <> provider.dimensions
    || List.exists
         (fun f -> (not (Float.is_finite f)) || abs_float f > 1e30)
         values
  then invalid_arg "Embedding provider returned invalid dimensions or values";
  let norm = sqrt (List.fold_left (fun sum x -> sum +. (x *. x)) 0. values) in
  if norm = 0. then invalid_arg "Embedding provider returned a zero vector";
  List.map (fun x -> x /. norm) values

let index_name db provider =
  if provider.max_batch < 1 || provider.max_batch > 128 then
    invalid_arg "Embedding batch size must be 1 to 128";
  Store.vector_index db ~model:provider.model ~dimensions:provider.dimensions
    ~normalization

let passage (m : M.message) = Agentkit.Memo.clip 8192 (m.subject ^ "\n" ^ m.text)
let hash s = Digestif.SHA256.(to_hex (digest_string s))

let index db provider ~check ~max_batches =
  if max_batches < 0 || max_batches > 10000 then
    invalid_arg "Invalid embedding budget";
  let index = index_name db provider in
  let pending =
    Store.messages db
    |> List.filter (fun (m : M.message) ->
           not
             (Store.vector_current db ~index ~source:m.source
                ~revision:m.revision
                ~input_hash:(hash (passage m))))
  in
  let rec loop remaining count = function
    | [] -> count
    | _ when remaining = 0 -> count
    | messages ->
        check ();
        let batch = List.filteri (fun i _ -> i < provider.max_batch) messages in
        let rest = List.filteri (fun i _ -> i >= provider.max_batch) messages in
        let inputs = List.map passage batch in
        let vectors = provider.embed ~check inputs in
        check ();
        if List.length vectors <> List.length batch then
          invalid_arg "Embedding provider returned an incomplete batch";
        let vectors = List.map (checked provider) vectors in
        List.iter2
          (fun (m : M.message) vector ->
            Store.vector_put db ~index ~source:m.source ~revision:m.revision
              ~input_hash:(hash (passage m))
              vector)
          batch vectors;
        loop (remaining - 1) (count + List.length batch) rest
  in
  loop max_batches 0 pending

let search db provider ~check query =
  let index = index_name db provider in
  check ();
  let vectors = provider.embed ~check [ Agentkit.Memo.clip 8192 query ] in
  check ();
  match vectors with
  | [ v ] -> Store.vector_search db ~index ~limit:20 (checked provider v)
  | _ -> invalid_arg "Embedding query returned the wrong vector count"

let hybrid db provider ~check ~lexical ~semantic =
  let lexical = Store.search db lexical in
  let semantic = search db provider ~check semantic |> List.map fst in
  let ranks = Hashtbl.create 32 in
  List.iter
    (fun ms ->
      List.iteri
        (fun i (m : M.message) ->
          let k = M.key m.source in
          let previous =
            Option.fold ~none:0. ~some:snd (Hashtbl.find_opt ranks k)
          in
          Hashtbl.replace ranks k (m, previous +. (1. /. float (60 + i))))
        ms)
    [ lexical; semantic ];
  Hashtbl.to_seq_values ranks
  |> List.of_seq
  |> List.sort (fun ((a : M.message), sa) ((b : M.message), sb) ->
         let n = Float.compare sb sa in
         if n = 0 then String.compare (M.key a.source) (M.key b.source) else n)
  |> List.filteri (fun i _ -> i < 100)
  |> List.map fst

let http ~sw ~fetch ~url ?api_key ~model ~dimensions () =
  let module U = Fetch.Middleware.Url in
  let endpoint =
    match U.of_string url with
    | Ok x -> x
    | Error _ -> invalid_arg "Invalid embedding endpoint"
  in
  if U.has_query endpoint || U.has_fragment endpoint then
    invalid_arg "Embedding endpoint must not contain query or fragment";
  if api_key <> None && U.scheme endpoint <> `Https then
    invalid_arg "Embedding credentials require HTTPS";
  let url = U.to_string endpoint in
  let fetch =
    Fetch.restrict ~under:[ url ] ~methods:[ `POST ]
      ~filter:(fun (r : Fetch.Middleware.request) ->
        if U.to_string r.url = url then `Allow
        else `Reject "Embedding endpoint changed")
      fetch
  in
  let row_codec =
    Jsont.Object.map (fun index embedding -> (index, embedding))
    |> Jsont.Object.mem "index" Jsont.int ~enc:fst
    |> Jsont.Object.mem "embedding" (Jsont.list Jsont.number) ~enc:snd
    |> Jsont.Object.finish
  in
  let response_codec =
    Jsont.Object.map Fun.id
    |> Jsont.Object.mem "data" (Jsont.list row_codec) ~enc:Fun.id
    |> Jsont.Object.finish
  in
  let embed ~check inputs =
    if
      inputs = []
      || List.length inputs > 16
      || List.exists (fun s -> String.length s > 8192) inputs
    then invalid_arg "Embedding HTTP batch exceeds limits";
    check ();
    let payload =
      Jsont.Json.object'
        (List.map
           (fun (k, v) -> ((k, Jsont.Meta.none), v))
           [
             ("model", Jsont.Json.string model);
             ("input", Jsont.Json.list (List.map Jsont.Json.string inputs));
             ("encoding_format", Jsont.Json.string "float");
           ])
      |> Jmap_eio.Codec.encode_exn Jsont.json
    in
    let headers = Fetch.Header.[ raw "content-type" "application/json" ] in
    let fetch =
      match api_key with
      | None -> fetch
      | Some key ->
          Fetch.with_credentials ~scope:[ url ]
            [ Fetch.Credential.bearer key ]
            fetch
    in
    let response =
      Fetch.post ~sw ~headers ~redirects:0 ~body:(String payload) fetch url
    in
    let raw =
      Fun.protect
        ~finally:(fun () -> Fetch.close response)
        (fun () ->
          if Fetch.status response <> 200 then
            failwith
              (Printf.sprintf "Embedding HTTP %d" (Fetch.status response));
          Eio.Buf_read.(parse_exn take_all)
            ~max_size:4194304 (Fetch.body response))
    in
    check ();
    let rows =
      Jmap_eio.Codec.decode_exn response_codec raw
      |> List.sort (fun (a, _) (b, _) -> compare a b)
    in
    if List.map fst rows <> List.init (List.length inputs) Fun.id then
      invalid_arg "Embedding response indices differ from input batch";
    List.map snd rows
  in
  { model = url ^ "#" ^ model; dimensions; max_batch = 16; embed }
