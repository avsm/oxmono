type log = {
  step : string;
  status : int option;
  elapsed : float option;
  detail : string option;
}
type error = { message : string; source : string option }

type payload =
  | Log of log
  | Error of error
  | Outcome of Jsont.json
  | Summary of Jsont.json

type t = {
  v : int;
  seq : int;
  at : string;
  cmd : string;
  subject : string option;
  payload : payload;
}

let object_json = Jsont.map ~kind:"Event payload" ~enc:Fun.id
  ~dec:(function
    | Jsont.Object _ as j -> j
    | _ -> Jsont.Error.msg Jsont.Meta.none "Expected object payload")
  Jsont.json

let log_json =
  Jsont.Object.map (fun step status elapsed detail ->
    { step; status; elapsed; detail })
  |> Jsont.Object.mem "step" Jsont.string ~enc:(fun l -> l.step)
  |> Jsont.Object.opt_mem "status" Jsont.int ~enc:(fun l -> l.status)
  |> Jsont.Object.opt_mem "elapsed" Jsont.number ~enc:(fun l -> l.elapsed)
  |> Jsont.Object.opt_mem "detail" Jsont.string ~enc:(fun l -> l.detail)
  |> Jsont.Object.finish

let error_json =
  Jsont.Object.map (fun message source -> { message; source })
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun e -> e.message)
  |> Jsont.Object.opt_mem "source" Jsont.string ~enc:(fun e -> e.source)
  |> Jsont.Object.finish

let jsont =
  Jsont.Object.map ~kind:"Event"
    (fun v seq at cmd subject log error outcome summary ->
      if seq <= 0 then
        Jsont.Error.msg Jsont.Meta.none "Expected positive seq";
      let payload = match log, error, outcome, summary with
        | Some j, None, None, None -> Log j
        | None, Some j, None, None -> Error j
        | None, None, Some j, None -> Outcome j
        | None, None, None, Some j -> Summary j
        | _ -> Jsont.Error.msg Jsont.Meta.none
            "Expected exactly one event payload"
      in
      { v; seq; at; cmd; subject; payload })
  |> Jsont.Object.mem "v" Json.version ~enc:(fun e -> e.v)
  |> Jsont.Object.mem "seq" Jsont.int ~enc:(fun e -> e.seq)
  |> Jsont.Object.mem "at" Jsont.string ~enc:(fun e -> e.at)
  |> Jsont.Object.mem "cmd" Jsont.string ~enc:(fun e -> e.cmd)
  |> Jsont.Object.opt_mem "subject" Jsont.string ~enc:(fun e -> e.subject)
  |> Jsont.Object.opt_mem "log" log_json
      ~enc:(fun e -> match e.payload with Log j -> Some j | _ -> None)
  |> Jsont.Object.opt_mem "error" error_json
      ~enc:(fun e -> match e.payload with Error j -> Some j | _ -> None)
  |> Jsont.Object.opt_mem "outcome" object_json
      ~enc:(fun e -> match e.payload with Outcome j -> Some j | _ -> None)
  |> Jsont.Object.opt_mem "summary" object_json
      ~enc:(fun e -> match e.payload with Summary j -> Some j | _ -> None)
  |> Jsont.Object.finish

let of_string = Jsont_bytesrw.decode_string jsont
let to_string = Jsont_bytesrw.encode_string jsont
let kind e = match e.payload with
  | Log _ -> "log" | Error _ -> "error"
  | Outcome _ -> "outcome" | Summary _ -> "summary"
