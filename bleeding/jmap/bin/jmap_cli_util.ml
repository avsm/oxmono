(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Proto = Jmap.Proto
module Chain = Jmap.Chain
module Client = Jmap_eio.Client
module Cli = Jmap_eio.Cli
module Sync = Jmap_eio.Sync

let mail_capabilities = [ Proto.Capability.core; Proto.Capability.mail ]

let die fmt =
  Format.kfprintf
    (fun _ -> exit 1)
    Fmt.stderr
    ("@[<v>%a " ^^ fmt ^^ "@]@.")
    Fmt.(styled `Red string)
    "Error:"

let warn fmt =
  Format.fprintf Fmt.stderr
    ("@[<v>%a " ^^ fmt ^^ "@]@.")
    Fmt.(styled `Yellow string)
    "Warning:"

let fail_client e = die "%s" (Client.error_to_string e)
let fail_sync error = die "%a" Sync.pp_error error

let debug_json cfg name jsont v =
  (* The encoded JSON carries its own newlines, so it goes to [Cli.debug] as
     one string rather than as a box Format would reindent. *)
  if cfg.Cli.debug then
    match Proto.Json.encode ~format:Jsont.Indent jsont v with
    | Ok json -> Cli.debug cfg "%s" (name ^ ":\n" ^ json)
    | Error e ->
        Cli.debug cfg "%s: (not encodable: %s)" name (Jsont.Error.to_string e)

let positive_int =
  let parse value =
    match int_of_string_opt value with
    | Some value
      when value > 0 && Int64.of_int value <= Proto.Int53.Unsigned.max_value ->
        Ok value
    | Some _ | None -> Error (`Msg "expected an integer between 1 and 2^53-1")
  in
  Cmdliner.Arg.conv (parse, Fmt.int)

let id =
  let parse value =
    Result.map_error (fun message -> `Msg message) (Proto.Id.of_string value)
  in
  Cmdliner.Arg.conv (parse, Proto.Id.pp)

let unique_ids ids =
  let seen = Hashtbl.create (List.length ids) in
  let rec check = function
    | [] -> Ok ids
    | id :: rest ->
        let key = Proto.Id.to_string id in
        if Hashtbl.mem seen key then
          Error
            (Fmt.str "email id %a was supplied more than once" Proto.Id.pp id)
        else begin
          Hashtbl.add seen key ();
          check rest
        end
  in
  check ids

let terminal_text = Cli.terminal_text

let ptime_to_string t =
  let (y, m, d), ((hh, mm, ss), _tz) = Ptime.to_date_time t in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" y m d hh mm ss

let truncate_string budget value =
  let value = terminal_text value in
  let length = String.length value in
  if length <= budget then value
  else
    (* Back up to the start of the UTF-8 sequence the cut falls inside, so that
       a multi-byte character is never left half printed. *)
    let rec boundary at =
      if at <= 0 then 0
      else if Char.code value.[at] land 0xc0 = 0x80 then boundary (at - 1)
      else at
    in
    if budget <= 3 then String.sub value 0 (boundary (Int.max 0 budget))
    else String.sub value 0 (boundary (budget - 3)) ^ "..."

let limit_note ~shown ~limit = if shown >= limit then ", limit reached" else ""

let listing_header title summary =
  Fmt.pr "@[<v>%a (%s)@,@," Fmt.(styled `Bold string) title summary

let resolve_account_id ?capability cfg client =
  match Cli.account_id ?capability cfg client with
  | Ok id -> id
  | Error message -> die "%s" message

let call ?(capabilities = mail_capabilities) client chain =
  match Client.call client ~capabilities chain with
  | Ok result -> result
  | Error e -> fail_client e

let query_ids client ?(capabilities = mail_capabilities) ?max query =
  match Sync.all_ids client ~capabilities ?max query with
  | Ok ids -> ids
  | Error error -> fail_sync error

let get_objects client ?(capabilities = mail_capabilities) ~kind ~id ids get =
  match Sync.get_all client ~capabilities ids get with
  | Error error -> fail_sync error
  | Ok (objects, not_found) ->
      if not_found <> [] then
        warn "the server did not return %d of the %s asked for: %a"
          (List.length not_found) kind
          Fmt.(list ~sep:(any ", ") Proto.Id.pp)
          not_found;
      Proto.Method.in_ids_order ~id ids objects

let get_emails client ?capabilities ~account_id ~properties ids =
  get_objects client ?capabilities ~kind:"emails" ~id:Proto.Email.id ids
    (fun ~ids ->
      Chain.email_get ~account_id ~ids:(Chain.ids ids) ~properties ())

let chunks size values =
  if size <= 0 then
    invalid_arg "Jmap_cli_util.chunks: the chunk size must be positive";
  let rec take left taken rest =
    match (left, rest) with
    | 0, _ | _, [] -> (List.rev taken, rest)
    | left, value :: rest -> take (left - 1) (value :: taken) rest
  in
  let rec loop batches = function
    | [] -> List.rev batches
    | values ->
        let batch, rest = take size [] values in
        loop (batch :: batches) rest
  in
  loop [] values

type set_labels = { succeeded : string; action : string; activity : string }

type set_report = {
  changed : Proto.Id.t list;
  refused : (Proto.Id.t * Proto.Error.Set_error.t) list;
  unmentioned : Proto.Id.t list;
  interrupted : (Proto.Id.t list * Proto.Id.t list * string) option;
}

let run_set client ?(capabilities = mail_capabilities) cfg ~outcome ids set =
  let batch_size =
    match Sync.max_objects_in_set client with
    | Some 0L ->
        die "the server advertises maxObjectsInSet=0, so nothing can be changed"
    | Some maximum ->
        Int.max 1
          (Int64.to_int (Int64.min maximum (Int64.of_int (List.length ids))))
    | None -> Int.max 1 (List.length ids)
  in
  let stop changed refused unmentioned batch remaining reason =
    {
      changed = List.rev changed;
      refused = List.rev refused;
      unmentioned = List.rev unmentioned;
      interrupted = Some (batch, List.concat remaining, reason);
    }
  in
  let rec run changed refused unmentioned = function
    | [] ->
        {
          changed = List.rev changed;
          refused = List.rev refused;
          unmentioned = List.rev unmentioned;
          interrupted = None;
        }
    | batch :: remaining -> (
        let chain = set ~ids:batch in
        debug_json cfg "Request" Proto.Request.jsont
          (Chain.build_request ~capabilities chain);
        match Client.chain client ~capabilities chain with
        | Error error ->
            stop changed refused unmentioned batch remaining
              (Client.error_to_string error)
        | Ok (handle, response) -> (
            debug_json cfg "Response" Proto.Response.jsont response;
            match Chain.parse handle response with
            | Error error ->
                stop changed refused unmentioned batch remaining
                  (Chain.parse_error_to_string error)
            | Ok result ->
                let acted, failed = outcome result in
                let mentioned = Hashtbl.create (List.length batch) in
                let note id =
                  Hashtbl.replace mentioned (Proto.Id.to_string id) ()
                in
                List.iter note acted;
                List.iter (fun (id, _) -> note id) failed;
                let omitted =
                  List.filter
                    (fun id ->
                      not (Hashtbl.mem mentioned (Proto.Id.to_string id)))
                    batch
                in
                run
                  (List.rev_append acted changed)
                  (List.rev_append failed refused)
                  (List.rev_append omitted unmentioned)
                  remaining))
  in
  run [] [] [] (chunks batch_size ids)

let report_set labels report =
  if report.changed <> [] then begin
    Fmt.pr "@[<v>%a %d email(s):@,"
      Fmt.(styled `Green string)
      labels.succeeded
      (List.length report.changed);
    List.iter (Fmt.pr "  %a@," Proto.Id.pp) report.changed;
    Fmt.pr "@]@."
  end;
  if report.refused <> [] then begin
    Fmt.epr "@[<v>%a to %s %d email(s):@,"
      Fmt.(styled `Red string)
      "Failed" labels.action
      (List.length report.refused);
    List.iter (Fmt.epr "  %a@," Proto.Method.pp_set_failure) report.refused;
    Fmt.epr "@]@."
  end;
  if report.unmentioned <> [] then begin
    Fmt.epr "@[<v>The server reported no outcome for %d email(s):@,"
      (List.length report.unmentioned);
    List.iter (Fmt.epr "  %a@," Proto.Id.pp) report.unmentioned;
    Fmt.epr "@]@."
  end;
  (match report.interrupted with
  | None -> ()
  | Some (uncertain, unattempted, reason) ->
      Fmt.epr "@[<v>%s stopped after a request failed: %s@," labels.activity
        reason;
      Fmt.epr "The outcome is unknown for %d email(s) in that request:@,"
        (List.length uncertain);
      List.iter (Fmt.epr "  %a@," Proto.Id.pp) uncertain;
      Fmt.epr "Not attempted: %d email(s):@," (List.length unattempted);
      List.iter (Fmt.epr "  %a@," Proto.Id.pp) unattempted;
      Fmt.epr "@]@.");
  if
    report.refused <> [] || report.unmentioned <> []
    || Option.is_some report.interrupted
  then exit 1
