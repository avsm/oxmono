module F = Stdlib_upstream_compatible.Float_u

type task = [ `Query | `Document ]

type provider = {
  model : string;
  dimensions : int;
  max_batch : int;
  embed : task:task -> check:(unit -> unit) -> string list -> float list list;
}

let normalization = "passage-8192-l2/v2"

let[@zero_alloc] squared_norm dimensions values =
  let rec sum count acc = function
    | [] ->
        if count <> dimensions then
          invalid_arg "Embedding provider returned invalid dimensions or values";
        acc
    | x :: rest ->
        if (not (Float.is_finite x)) || abs_float x > 1e30 then
          invalid_arg "Embedding provider returned invalid dimensions or values";
        let x = F.of_float x in
        sum (count + 1) (F.add acc (F.mul x x)) rest
  in
  sum 0 #0. values

let[@zero_alloc] max_magnitude values =
  let rec loop largest = function
    | [] -> largest
    | x :: rest ->
        let x = F.abs (F.of_float x) in
        loop (if F.compare x largest > 0 then x else largest) rest
  in
  loop #0. values

let[@zero_alloc] scaled_squared_norm scale values =
  let rec loop acc = function
    | [] -> acc
    | x :: rest ->
        let x = F.div (F.of_float x) scale in
        loop (F.add acc (F.mul x x)) rest
  in
  loop #0. values

let checked provider values =
  let squared = squared_norm provider.dimensions values in
  (* Subnormal squared sums lose precision or round to zero. Scale only this
     range so that ordinary vectors retain their existing rounding. *)
  if F.compare squared #0x1p-1022 < 0 then begin
    let scale = max_magnitude values in
    if F.equal scale #0. then
      invalid_arg "Embedding provider returned a zero vector";
    let norm = F.sqrt (scaled_squared_norm scale values) in
    List.map
      (fun x -> F.to_float (F.div (F.div (F.of_float x) scale) norm))
      values
  end
  else begin
    let norm = F.sqrt squared in
    List.map (fun x -> F.to_float (F.div (F.of_float x) norm)) values
  end

type 'a backend = {
  documents : unit -> 'a list;
  key : 'a -> string;
  revision : 'a -> string;
  passage : 'a -> string;
  index : model:string -> dimensions:int -> normalization:string -> string;
  current : index:string -> document:'a -> input_hash:string -> bool;
  put : index:string -> document:'a -> input_hash:string -> float list -> unit;
  nearest : index:string -> limit:int -> float list -> ('a * float) list;
  lexical : string -> 'a list;
}

let hash s = Digestif.SHA256.(to_hex (digest_string s))

let clip limit s =
  if String.length s <= limit then s
  else
    let mutable stop = limit in
    while stop > 0 && Char.code s.[stop] land 0xc0 = 0x80 do
      stop <- stop - 1
    done;
    String.sub s 0 stop

let index_name db provider =
  if provider.max_batch < 1 || provider.max_batch > 128 then
    invalid_arg "Embedding batch size must be 1 to 128";
  db.index ~model:provider.model ~dimensions:provider.dimensions ~normalization

type 'a pending = { document : 'a; input : string; input_hash : string }

(* At most 128 selected documents remain on the call stack. Already-current
   documents are skipped by a tail call, and the unconsumed tail is shared. *)
let rec pending_batch db ~index remaining documents =
  if remaining = 0 then #([], documents)
  else
    match documents with
    | [] -> #([], [])
    | document :: rest ->
        let input = db.passage document in
        let input_hash = hash input in
        if db.current ~index ~document ~input_hash then
          pending_batch db ~index remaining rest
        else
          let #(batch, rest) =
            pending_batch db ~index (remaining - 1) rest
          in
          #({ document; input; input_hash } :: batch, rest)

let index ?on_progress db provider ~check ~max_batches =
  if max_batches < 0 || max_batches > 10000 then
    invalid_arg "Invalid embedding budget";
  let index = index_name db provider in
  let rec loop remaining count documents =
    if remaining = 0 then count
    else
      let #(batch, rest) =
        pending_batch db ~index provider.max_batch documents
      in
      match batch with
      | [] -> count
      | _ ->
          check ();
          let inputs = List.map (fun pending -> pending.input) batch in
          let vectors = provider.embed ~task:`Document ~check inputs in
          check ();
          if List.length vectors <> List.length batch then
            invalid_arg "Embedding provider returned an incomplete batch";
          let vectors = List.map (checked provider) vectors in
          List.iter2
            (fun pending vector ->
              db.put ~index ~document:pending.document
                ~input_hash:pending.input_hash vector)
            batch vectors;
          let count = count + List.length batch in
          Option.iter (fun progress -> progress count) on_progress;
          loop (remaining - 1) count rest
  in
  if max_batches = 0 then 0 else loop max_batches 0 (db.documents ())

let search db provider ~check query =
  let index = index_name db provider in
  check ();
  let vectors = provider.embed ~task:`Query ~check [ clip 8192 query ] in
  check ();
  match vectors with
  | [ v ] -> db.nearest ~index ~limit:20 (checked provider v)
  | _ -> invalid_arg "Embedding query returned the wrong vector count"

let fuse ~key ~limit lists =
  let ranks = Hashtbl.create 32 in
  List.iter
    (fun items ->
      let seen = Hashtbl.create 32 in
      List.iteri
        (fun i item ->
          let k = key item in
          if not (Hashtbl.mem seen k) then (
            Hashtbl.add seen k ();
            let previous =
              Option.fold ~none:0. ~some:snd (Hashtbl.find_opt ranks k)
            in
            Hashtbl.replace ranks k (item, previous +. (1. /. float (60 + i)))))
        items)
    lists;
  Hashtbl.to_seq_values ranks
  |> List.of_seq
  |> List.sort (fun (a, sa) (b, sb) ->
         let n = Float.compare sb sa in
         if n = 0 then String.compare (key a) (key b) else n)
  |> List.filteri (fun i _ -> i < limit)
  |> List.map fst

let hybrid db provider ~check ~lexical ~semantic =
  let lexical = db.lexical lexical in
  let semantic = search db provider ~check semantic |> List.map fst in
  fuse ~key:db.key ~limit:100 [ lexical; semantic ]

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
  let embed ~task:_ ~check inputs =
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
      |> Json.encode Jsont.json
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
      Json.decode response_codec raw
      |> List.sort (fun (a, _) (b, _) -> compare a b)
    in
    if List.map fst rows <> List.init (List.length inputs) Fun.id then
      invalid_arg "Embedding response indices differ from input batch";
    List.map snd rows
  in
  { model = url ^ "#" ^ model; dimensions; max_batch = 16; embed }
