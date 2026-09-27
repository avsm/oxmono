module P = Imap.Proto
module Flag = Mail_flag.Imap_flag
module Uids = Map.Make (Int64)

type error =
  | Client of Imap_eio.Error.t
  | Invalid_intent of string
  | Invalid_scope of string
  | Incomplete of string
  | Limit of string

let pp_error ppf = function
  | Client error -> Imap_eio.Client.pp_error ppf error
  | Invalid_intent message -> Format.fprintf ppf "invalid APPEND intent: %s" message
  | Invalid_scope message -> Format.fprintf ppf "invalid mailbox scope: %s" message
  | Incomplete message -> Format.fprintf ppf "incomplete APPEND evidence: %s" message
  | Limit message -> Format.fprintf ppf "APPEND inspection limit: %s" message

type candidate = {
  uid : P.Uid.t;
  length : int64;
  sha256 : string;
  flags_match : bool option;
}

type report =
  | Epoch_changed of {
      journal_uidvalidity : P.Uidvalidity.t;
      server_uidvalidity : P.Uidvalidity.t;
    }
  | Inspected of {
      uidvalidity : P.Uidvalidity.t;
      covered_upper : int64;
      examined : int;
      matches : candidate list;
    }

let ( let* ) result f = match result with Ok x -> f x | Error _ as e -> e
let network = function Ok value -> Ok value | Error error -> Error (Client error)

let flags_equal expected raw =
  let rec parse acc = function
    | [] -> Ok acc
    | value :: rest ->
        (match Flag.of_wire value with
         | Error message -> Error (Incomplete ("invalid server flag: " ^ message))
         | Ok flag -> parse (flag :: acc) rest) in
  let* actual = parse [] raw in
  Ok (Flag.equal_durable actual expected)

let inspect_append ?(max_windows=1000) ?(max_candidates=1000)
    ?(max_bytes=1_073_741_824L) ~client ~store ~scope ~mailbox ~id
    ~spool () =
  if max_windows < 1 || max_candidates < 1 || max_bytes < 0L then
    Error (Limit "inspection budgets must be positive")
  else
    match Imap_store.find_intent store ~id with
    | None -> Error (Invalid_intent "unknown ID")
    | Some intent when intent.scope <> scope ->
        Error (Invalid_scope "journal scope differs from requested scope")
    | Some intent ->
        let* content_digest, expected_length, expected_flags, frontier =
          match intent.kind with
          | Imap_store.Append {content_digest; expected_length;
              expected_flags; pre_send_uid_frontier; _} ->
              Ok (content_digest, expected_length, expected_flags,
                pre_send_uid_frontier)
          | Imap_store.Other _ -> Error (Invalid_intent "operation is not APPEND") in
        let* () = match intent.state with
          | Imap_store.Sent | Imap_store.Ambiguous -> Ok ()
          | _ -> Error (Invalid_intent "operation is not pending after send") in
        let* journal_uidvalidity = match intent.uidvalidity with
          | Some value -> Ok value
          | None -> Error (Incomplete "journal has no UIDVALIDITY") in
        let* frontier = match frontier with
          | Some value -> Ok value
          | None -> Error (Incomplete "journal has no pre-send UID frontier") in
        let mode = Imap_eio.Client.mailbox_mode client in
        if mode <> scope.Imap.Mirror.encoding then
          Error (Invalid_scope "mailbox encoding changed")
        else
          let* wire_name = match Imap.Mailbox_name.encode ~mode mailbox with
            | Ok value -> Ok value
            | Error message -> Error (Invalid_scope message) in
          if wire_name <> scope.raw_name then
            Error (Invalid_scope "mailbox wire name differs from journal scope")
          else
            let selected_result = network
              (Imap_eio.Client.with_mailbox client ~mode:`Read_only mailbox
                (fun selected ->
                  let result =
                    let* info = network (Imap_eio.Selected.info selected) in
                    let* server_uidvalidity = match
                      P.Uidvalidity.of_int64 info.uidvalidity with
                      | Ok value -> Ok value
                      | Error message -> Error (Incomplete message) in
                    if server_uidvalidity <> journal_uidvalidity then
                      Ok (Epoch_changed {journal_uidvalidity;
                        server_uidvalidity})
                    else
                      let upper = Int64.pred info.uidnext in
                      if upper < frontier then
                        Error (Incomplete "UIDNEXT regressed below pre-send frontier")
                      else
                        let first = Int64.succ frontier in
                        let windows = if first > upper then 0L else
                          Int64.succ (Int64.div (Int64.sub upper first) 1000L) in
                        if windows > Int64.of_int max_windows then
                          Error (Limit "UID range exceeds window budget")
                        else
                          let examined = ref 0 in
                          let rec scan first matches =
                            if first > upper then
                              Ok (Inspected {uidvalidity=server_uidvalidity;
                                covered_upper=upper; examined = !examined;
                                matches=List.rev matches})
                            else
                              let last = Int64.min upper
                                (Int64.add first 999L) in
                              let criterion = Printf.sprintf "UID %Ld:%Ld"
                                first last in
                              let* uids = network
                                (Imap_eio.Selected.uid_search selected criterion) in
                              let uids = List.sort_uniq Int64.compare uids in
                              let* rows = network
                                (Imap_eio.Selected.fetch_metadata_range selected
                                  ~first ~last ~modseq:false) in
                              let flags = List.fold_left (fun acc
                                  (row : Imap.Response.fetch) ->
                                match row.uid, row.flags with
                                | Some uid, Some flags -> Uids.add uid flags acc
                                | _ -> acc) Uids.empty rows in
                              let rec candidates matches = function
                                | [] -> scan (Int64.succ last) matches
                                | raw_uid :: rest ->
                                    if raw_uid < first || raw_uid > last then
                                      Error (Incomplete "SEARCH UID outside requested range")
                                    else if !examined >= max_candidates then
                                      Error (Limit "candidate count exceeds budget")
                                    else (
                                      incr examined;
                                      let* uid = match P.Uid.of_int64 raw_uid with
                                        | Ok uid -> Ok uid
                                        | Error message -> Error (Incomplete message) in
                                      let fetched = Spool.with_spool spool
                                        (fun output ->
                                          let* () = network
                                            (Imap_eio.Selected.fetch_to selected
                                              ~max_bytes ~uid:raw_uid output) in
                                          let length, sha256 = Spool.hash_file spool in
                                          Ok (length,sha256)) in
                                      let* length, sha256 = fetched in
                                      let length_match = match
                                        expected_length with
                                        | None -> true
                                        | Some expected -> expected=length in
                                      if not length_match ||
                                         sha256 <> content_digest then
                                        candidates matches rest
                                      else
                                        let* flags_match = match
                                          expected_flags,
                                          Uids.find_opt raw_uid flags with
                                          | Some expected, Some raw ->
                                              let* equal = flags_equal expected raw in
                                              Ok (Some equal)
                                          | _ -> Ok None in
                                        candidates ({uid;length;sha256;
                                          flags_match} :: matches) rest) in
                              candidates matches uids in
                          scan first []
                  in Ok result)) in
            let* report = selected_result in
            report
