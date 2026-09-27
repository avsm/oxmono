type error = Error.t =
  | Closed
  | Protocol of string
  | Transport of string
  | Rejected of { tag : string; status : [ `No | `Bad ];
      code : Imap.Response.code option; text : string }
  | State of string
  | Missing_uid of Imap.Uid.t
  | Limit of string
  | Uncertain of string
  | Unsupported of Imap.Capability.t
  | Not_enabled of Imap.Capability.t

type t = {
  flow : Transport.flow;
  mutable wire : Imap.Wire.t;
  mutable queued : Imap.Wire.event list;
  mutex : Eio.Mutex.t;
  mutable closed : bool;
  mutable tag_number : int;
  mutable generation : int;
  mutable saved_search_nonce : unit ref;
  mutable selected : string option;
  mutable uidbatches_last_mailbox : string option;
  mutable readonly : bool;
  mutable capabilities : Imap.Capability.Set.t;
  mutable enabled : Imap.Capability.Set.t;
  input : Cstruct.t;
  mutable read_size : int;
  max_metadata : int;
  max_responses : int;
  max_command_metadata : int;
}

exception Failure of error

let create ?(max_metadata=16_777_216) ?(max_responses=10_000)
    ?(max_command_metadata=67_108_864) flow =
  if max_metadata < 1024 || max_responses < 1 ||
     max_command_metadata < max_metadata then
    invalid_arg "Session.create limits";
  { flow; wire = Imap.Wire.create (); queued = [];
    mutex = Eio.Mutex.create (); closed = false; tag_number = 0;
    generation = 0; saved_search_nonce = ref (); selected = None; uidbatches_last_mailbox = None;
    readonly = false;
    capabilities = Imap.Capability.Set.empty;
    enabled = Imap.Capability.Set.empty;
    input = Cstruct.create 65536; read_size = 65536; max_metadata; max_responses;
    max_command_metadata }

let close t =
  if not t.closed then (
    t.closed <- true;
    t.generation <- t.generation + 1;
    try Transport.close t.flow with _ -> ())

let check_open t = if t.closed then raise (Failure Closed)

let advertised t c = Imap.Capability.Set.mem c t.capabilities
let is_enabled t c = Imap.Capability.Set.mem c t.enabled

let revision_two t =
  advertised t Imap4rev2 &&
  (not (advertised t Imap4rev1) || is_enabled t Imap4rev2)

let has t c =
  advertised t c || (Imap.Capability.implied_by_rev2 c && revision_two t)

let require t c = if not (has t c) then raise (Failure (Unsupported c))

let require_enabled t c =
  if not (is_enabled t c) then raise (Failure (Not_enabled c))

let mailbox_mode t =
  if revision_two t || is_enabled t (Utf8 `Accept)
  then Imap.Mailbox_name.Utf8 else Imap.Mailbox_name.Rev1

let mailbox_wire t name =
  match Imap.Mailbox_name.encode ~mode:(mailbox_mode t) name with
  | Ok raw -> raw
  | Error e -> raise (Failure (State ("invalid mailbox name: " ^ e)))

let write t s =
  check_open t;
  if String.length s > 65536 then raise (Failure (Limit "command syntax exceeds 64 KiB"));
  Transport.write t.flow [Cstruct.of_string s]

let next_tag t =
  t.tag_number <- t.tag_number + 1;
  if t.tag_number < 0 then raise (Failure (State "tag space exhausted"));
  Printf.sprintf "A%08d" t.tag_number

let read_event t =
  let feed chunk =
    match Imap.Wire.feed t.wire chunk with
    | Error e -> raise (Failure (Protocol
        (Printf.sprintf "wire offset %Ld: %s" e.offset e.message)))
    | Ok events -> t.queued <- events in
  let rec take () = match t.queued with
  | x :: xs -> t.queued <- xs; x
  | [] ->
      check_open t;
      (* The decoder defers an error found after framed events. Surface it
         before blocking on another read. *)
      feed "";
      let n = Transport.read t.flow (Cstruct.sub t.input 0 t.read_size) in
      if n = 0 then raise End_of_file;
      feed (Cstruct.to_string (Cstruct.sub t.input 0 n));
      take ()
  in take ()

let literal_item text =
  match String.rindex_opt text '{' with
  | Some k when k > 0 && text.[k-1] = '~' -> String.sub text 0 (k-1)
  | Some k -> String.sub text 0 k
  | None -> text

let ends_with_ci ~suffix s =
  let n = String.length s and m = String.length suffix in
  n >= m && String.uppercase_ascii (String.sub s (n-m) m) = suffix

(* [body_item item] holds when [item] ends with a message-body data item name
   such as [BODY[HEADER]] or [BINARY[1]<0>], followed by one space. *)
let body_item item =
  let n = String.length item in
  let n = if n > 0 && item.[n-1] = ' ' then n-1 else n in
  let n =
    if n > 0 && item.[n-1] = '>' then
      match String.rindex_from_opt item (n-1) '<' with Some k -> k | None -> n
    else n in
  n > 0 && item.[n-1] = ']' &&
  match String.rindex_from_opt item (n-1) '[' with
  | None -> false
  | Some k ->
      let rec start i =
        if i > 0 && item.[i-1] <> ' ' && item.[i-1] <> '(' then start (i-1)
        else i in
      let s = start k in
      List.mem (String.uppercase_ascii (String.sub item s (k-s)))
        ["BODY"; "BODY.PEEK"; "BINARY"; "BINARY.PEEK"]

let read_response ?on_literal ?(on_literal_start=(fun _ -> ())) t =
  let fetch_response = ref None in
  let streaming = ref false in
  let rec loop acc size =
    match read_event t with
    | Imap.Wire.Literal_start n as event ->
        let fetch = !fetch_response = Some true in
        let item = match acc with
          | Imap.Wire.Text s :: _ -> literal_item s
          | _ -> "" in
        if fetch && n > 1024L && ends_with_ci ~suffix:"PREVIEW " item then
          raise (Failure (Limit "PREVIEW literal exceeds 1024 bytes"));
        streaming := fetch && Option.is_some on_literal && body_item item;
        if !streaming then on_literal_start n;
        loop (event :: acc) size
    | Imap.Wire.Literal_chunk s as event ->
        (match on_literal with
         | Some on_literal when !streaming -> on_literal s; loop acc size
         | _ ->
             let size = size + String.length s in
             if size > t.max_metadata then
               raise (Failure (Limit
                 "response literal exceeds metadata limit"));
             loop (event :: acc) size)
    | Imap.Wire.End_of_response -> List.rev (Imap.Wire.End_of_response :: acc)
    | Imap.Wire.Text s as event ->
        (if !fetch_response=None then
           fetch_response:=Some (match String.split_on_char ' ' s with
             | "*"::seq::kind::_ when Int64.of_string_opt seq<>None ->
                 let kind=String.uppercase_ascii kind in
                 kind="FETCH" || kind="UIDFETCH"
             | _ -> false));
        let size = size + String.length s in
        if size > t.max_metadata then
          raise (Failure (Limit "response metadata exceeds configured limit"));
        loop (event :: acc) size
    | event -> loop (event :: acc) size
  in loop [] 0

let parse parts = match Imap.Response.parse_parts parts with
| Ok response -> response
| Error e -> raise (Failure (Protocol e))

let parse_active t parts =
  let response = parse parts in
  (match response with
   | Imap.Response.Tagged {code=Some (Imap.Response.Uidvalidity _ | Imap.Response.Closed);_}
   | Imap.Response.Untagged (Imap.Response.Ok
       (Some (Imap.Response.Uidvalidity _ | Imap.Response.Closed),_))
   | Imap.Response.Untagged (Imap.Response.No
       (Some (Imap.Response.Uidvalidity _ | Imap.Response.Closed),_))
   | Imap.Response.Untagged (Imap.Response.Bad
       (Some (Imap.Response.Uidvalidity _ | Imap.Response.Closed),_)) ->
       t.saved_search_nonce <- ref ();
       if t.selected<>None then
         raise (Failure (Protocol "selected mailbox identity was reset"))
   | _ -> ());
  let uidonly = is_enabled t Uidonly in
  (match response with
   | Imap.Response.Untagged (Imap.Response.Fetch _ | Imap.Response.Expunge _)
     when uidonly ->
       raise (Failure (Protocol "sequence-number response in UIDONLY mode"))
   | Imap.Response.Untagged (Imap.Response.Uidfetch _) when not uidonly ->
       raise (Failure (Protocol "UIDFETCH outside UIDONLY mode"))
   | _ -> ());
  response

let parts_size parts =
  List.fold_left (fun size -> function
    | Imap.Wire.Text s | Imap.Wire.Literal_chunk s ->
        Int64.add size (Int64.of_int (String.length s))
    | _ -> size) 0L parts

type budget = { what : string; mutable bytes : int64; mutable count : int }

let budget what = { what; bytes = 0L; count = 0 }

let next ?on_literal ?on_literal_start ?(bye=false) t budget =
  let parts = read_response ?on_literal ?on_literal_start t in
  budget.bytes <- Int64.add budget.bytes (parts_size parts);
  if budget.bytes > Int64.of_int t.max_command_metadata then
    raise (Failure (Limit
      (budget.what ^ " metadata exceeds configured limit")));
  match parse_active t parts with
  | Imap.Response.Untagged (Imap.Response.Bye (_, text)) when not bye ->
      raise (Failure (Protocol ("server BYE: " ^ text)))
  | response -> response

let charge t budget =
  if budget.count >= t.max_responses then
    raise (Failure (Limit (budget.what ^ " response count exceeds limit")));
  budget.count <- budget.count + 1

let io_failure = function
  | Eio.Io _ | Unix.Unix_error _ | End_of_file
  | Tls_eio.Tls_alert _ | Tls_eio.Tls_failure _ -> true
  | _ -> false

(* A tagged rejection leaves [t] in step with the server. Any other failure
   after bytes were sent, and any exception that is not a [Failure], closes
   [t] because later replies can no longer be matched to commands. With
   [uncertain], the whole command reached the server, so a failure other
   than a rejection leaves its effect unknown. *)
let abandon ?uncertain t ~sent ex bt =
  match ex, uncertain with
  | Failure (Rejected _), _ -> Printexc.raise_with_backtrace ex bt
  | (Failure (Uncertain _) | Eio.Cancel.Cancelled _), _ ->
      close t; Printexc.raise_with_backtrace ex bt
  | Failure e, Some what ->
      close t; raise (Failure (Uncertain (what ^ ": " ^ Error.to_string e)))
  | ex, Some what when io_failure ex ->
      close t; raise (Failure (Uncertain (what ^ ": " ^ Printexc.to_string ex)))
  | Failure _, _ when not sent -> Printexc.raise_with_backtrace ex bt
  | _ -> close t; Printexc.raise_with_backtrace ex bt

type command_result = {
  untagged : Imap.Response.t list;
  completion : Imap.Response.t;
  partial : (int64 * int64 option) option;
}

let command_result ?on_literal ?on_literal_start
    ?(mutation=false) ?(accept_partial=false) ?saved_search_criterion t syntax =
  check_open t;
  (* Raw SEARCH criteria can include RETURN (SAVE). Invalidate before every
     UID SEARCH dispatch, including rejected commands, so no handle can name
     a silently replaced server variable. Physical refs never wrap. *)
  let preserves_saved=match saved_search_criterion with
    | None -> false
    | Some criterion ->
        (match Imap.Command.uid_search_saved ~criterion with
         | Ok expected when syntax=expected -> true
         | _ -> raise (Failure (State "invalid saved SEARCH refinement command"))) in
  if not preserves_saved &&
     String.starts_with ~prefix:"UID SEARCH " (String.uppercase_ascii syntax) then
    t.saved_search_nonce <- ref ();
  let tag = next_tag t in
  let sent = ref false in
  let partial_limit = ref None in
  let budget = budget "command" in
  try
    write t (tag ^ " " ^ syntax ^ "\r\n");
    sent := true;
    let rec receive acc =
      let response = next ?on_literal ?on_literal_start t budget in
      match response with
      | Imap.Response.Tagged {
          tag=got; status=`Ok;
          code=Some (Imap.Response.Messagelimit (limit,last)); _}
        when got = tag ->
          if accept_partial && not mutation then
            {untagged=List.rev acc; completion=response; partial=Some (limit,last)}
          else if mutation then
            raise (Failure (Uncertain
              "server reported a partial mutation with MESSAGELIMIT"))
          else raise (Failure (Limit
              "server reported a partial response with MESSAGELIMIT"))
      | Imap.Response.Tagged {tag=got; status=`Ok; _} when got = tag ->
          if Option.is_some !partial_limit && not (accept_partial && not mutation) then
            raise (Failure (if mutation then Uncertain
              "server reported a partial mutation with MESSAGELIMIT"
              else Limit "server reported a partial response with MESSAGELIMIT"));
          {untagged=List.rev acc; completion=response; partial= !partial_limit}
      | Imap.Response.Tagged {tag=got; status=(`No | `Bad as status);
          code; text; _}
        when got = tag ->
          (* RFC 9738 makes a limit-rejected COPY atomic. Other mutations
             with a processed UID, or any prior partial notice, may have
             changed server state before the final NO. *)
          let atomic_copy =
            String.starts_with ~prefix:"UID COPY " syntax ||
            String.starts_with ~prefix:"COPY " syntax in
          let processed = match code with
            | Some (Imap.Response.Messagelimit (_, Some _)) -> true
            | _ -> false in
          if mutation && (Option.is_some !partial_limit ||
              (processed && not atomic_copy)) then
            raise (Failure (Uncertain
              "server reported a partial mutation with MESSAGELIMIT"))
          else raise (Failure (Rejected {tag; status; code; text}))
      | Imap.Response.Tagged {tag=got; _} ->
          raise (Failure (Protocol ("unexpected tagged completion " ^ got)))
      | Imap.Response.Continuation _ -> raise (Failure (Protocol "unexpected continuation"))
      | Imap.Response.Untagged _ ->
          (match response with
           | Imap.Response.Untagged (Imap.Response.No
               (Some (Imap.Response.Messagelimit (limit,last)), _)) ->
               partial_limit := Some (limit,last)
           | _ -> ());
          charge t budget;
          receive (response :: acc)
    in receive []
  with ex ->
    let bt = Printexc.get_raw_backtrace () in
    let uncertain = if mutation && !sent then
        Some "mutation outcome unknown after command bytes were sent"
      else None in
    abandon ?uncertain t ~sent:!sent ex bt

let command ?on_literal ?on_literal_start ?mutation t syntax =
  (command_result ?on_literal ?on_literal_start ?mutation t syntax).untagged

let compress_deflate t =
  check_open t;
  if Transport.compressed t.flow then
    raise (Failure (State "COMPRESS is already active"));
  require t (Compress `Deflate);
  (* The server switches immediately after the tagged OK CRLF. Reading one
     byte at a time only during this handshake leaves a coalesced compressed
     tail in the underlying flow (including TLS's own read buffer), never in
     the plaintext IMAP framer. Existing queued unsolicited plaintext drains
     normally. A NO/BAD response leaves the original framer/transport usable. *)
  let previous_read_size=t.read_size in
  Fun.protect ~finally:(fun () -> t.read_size<-previous_read_size) (fun () ->
    t.read_size<-1;
    ignore (command_result t "COMPRESS DEFLATE");
    if t.queued<>[] || Result.is_error (Imap.Wire.finish t.wire) then (
      close t;
      raise (Failure (Protocol "unclean COMPRESS framing boundary")));
    try
      Transport.compress_deflate t.flow;
      t.wire<-Imap.Wire.create ()
    with ex ->
      let bt = Printexc.get_raw_backtrace () in
      close t;
      Printexc.raise_with_backtrace ex bt)

type append_part = { prefix : string; length : int64; read : Cstruct.t -> int; synchronizing : bool }

let append_many t parts =
  check_open t;
  if parts=[] || List.exists (fun part -> part.length<0L) parts then
    raise (Failure (State "invalid APPEND parts"));
  let tag = next_tag t in
  let started = ref false and complete = ref false in
  let partial = ref false in
  let budget = budget "APPEND" in
  let response () =
    let response = next t budget in
    (match response with
     | Imap.Response.Untagged (Imap.Response.No
         (Some (Imap.Response.Messagelimit _), _)) -> partial:=true
     | _ -> ());
    response in
  let rec continuation () =
    match response () with
    | Imap.Response.Continuation _ -> ()
    | Imap.Response.Tagged {tag=got; status=(`No | `Bad as status); code; text}
      when got = tag -> raise (Failure (Rejected {tag; status; code; text}))
    | Imap.Response.Untagged _ -> charge t budget; continuation ()
    | _ -> raise (Failure (Protocol "expected APPEND continuation")) in
  let buffer = Cstruct.create 65536 in
  let rec copy read left =
    if left > 0L then (
      let amount = Int64.to_int (Int64.min left 65536L) in
      let chunk = Cstruct.sub buffer 0 amount in
      let n = match read chunk with
        | n -> n
        | exception End_of_file -> 0
        | exception (Eio.Cancel.Cancelled _ as ex) -> raise ex
        | exception ex -> raise (Failure (State
            ("APPEND source failed: " ^ Printexc.to_string ex))) in
      if n = 0 then
        raise (Failure (State "APPEND source ended before declared length"));
      Transport.write t.flow [Cstruct.sub chunk 0 n];
      copy read (Int64.sub left (Int64.of_int n))) in
  let rec completion () =
    match response () with
    | Imap.Response.Tagged {
        tag=got; status=`Ok;
        code=Some (Imap.Response.Messagelimit _); _} when got = tag ->
        raise (Failure (Uncertain
          "server reported a partial APPEND with MESSAGELIMIT"))
    | Imap.Response.Tagged {tag=got; status=`Ok; _} as response
      when got = tag ->
        if !partial then raise (Failure (Uncertain
          "server reported a partial APPEND before success"));
        response
    | Imap.Response.Tagged {tag=got; status=(`No | `Bad as status); code; text}
      when got = tag -> raise (Failure (Rejected {tag; status; code; text}))
    | Imap.Response.Tagged {tag=got; _} ->
        raise (Failure (Protocol ("unexpected tagged completion " ^ got)))
    | Imap.Response.Continuation _ ->
        raise (Failure (Protocol
          "unexpected APPEND continuation after final literal"))
    | Imap.Response.Untagged _ -> charge t budget; completion () in
  try
    List.iteri (fun index part ->
      write t ((if index = 0 then tag ^ " " else "") ^ part.prefix);
      started := true;
      if part.synchronizing then continuation ();
      copy part.read part.length) parts;
    Transport.write t.flow [Cstruct.of_string "\r\n"];
    complete := true;
    completion ()
  with ex ->
    let bt = Printexc.get_raw_backtrace () in
    let uncertain = if !complete then
        Some "APPEND outcome unknown after the command was sent"
      else None in
    abandon ?uncertain t ~sent:!started ex bt

let append ?(synchronizing=true) t ~prefix ~length source =
  append_many t [{prefix;length;read=Eio.Flow.single_read source;synchronizing}]

let logout t =
  Fun.protect ~finally:(fun () -> close t) (fun () ->
    check_open t;
    let tag=next_tag t in
    let budget = budget "LOGOUT" in
    write t (tag ^ " " ^ Imap.Command.logout ^ "\r\n");
    let rec receive seen_bye =
      match next ~bye:true t budget with
      | Imap.Response.Tagged {tag=got;status=`Ok;_} when got=tag && seen_bye -> ()
      | Imap.Response.Tagged {tag=got;status=(`No | `Bad as status);code;text}
          when got=tag -> raise (Failure (Rejected {tag;status;code;text}))
      | Imap.Response.Tagged _ ->
          raise (Failure (Protocol "invalid LOGOUT completion or missing BYE"))
      | Imap.Response.Continuation _ ->
          raise (Failure (Protocol "unexpected LOGOUT continuation"))
      | Imap.Response.Untagged response ->
          charge t budget;
          let seen_bye=match response with
            | Imap.Response.Bye _ when seen_bye ->
                raise (Failure (Protocol "repeated LOGOUT BYE"))
            | Imap.Response.Bye _ -> true
            | _ -> seen_bye in
          receive seen_bye
    in receive false)

let idle_once t =
  check_open t;
  let tag = next_tag t in
  let sent = ref false in
  let budget = budget "IDLE" in
  let response () = next t budget in
  let fail_tagged got status code text =
    if got <> tag then
      raise (Failure (Protocol ("unexpected tagged completion " ^ got)))
    else match status with
      | `No | `Bad as status -> raise (Failure (Rejected {tag; status; code; text}))
      | `Ok -> raise (Failure (Protocol "IDLE completed before DONE")) in
  let add acc item = charge t budget; item :: acc in
  try
    write t (tag ^ " IDLE\r\n");
    sent := true;
    let rec continuation acc =
      match response () with
      | Imap.Response.Continuation _ -> acc
      | Imap.Response.Untagged _ as item -> continuation (add acc item)
      | Imap.Response.Tagged {tag=got; status; code; text} ->
          fail_tagged got status code text
    in
    let initial = continuation [] in
    let changed = match initial with
      | _::_ -> initial
      | [] ->
          (match response () with
           | Imap.Response.Untagged _ as item -> add [] item
           | Imap.Response.Tagged {tag=got; status; code; text} ->
               fail_tagged got status code text
           | Imap.Response.Continuation _ ->
               raise (Failure (Protocol "duplicate IDLE continuation"))) in
    write t "DONE\r\n";
    let rec completion acc = match response () with
      | Imap.Response.Tagged {tag=got; status=`Ok; _} when got=tag ->
          List.rev acc
      | Imap.Response.Tagged {tag=got; status=(`No | `Bad as status); code; text}
        when got=tag -> raise (Failure (Rejected {tag; status; code; text}))
      | Imap.Response.Tagged {tag=got; _} ->
          raise (Failure (Protocol ("unexpected tagged completion " ^ got)))
      | Imap.Response.Untagged _ as item -> completion (add acc item)
      | Imap.Response.Continuation _ ->
          raise (Failure (Protocol "unexpected IDLE continuation"))
    in completion changed
  with ex ->
    let bt = Printexc.get_raw_backtrace () in
    abandon t ~sent:!sent ex bt

let protect t f =
  try Ok (f ()) with
  | Failure e -> Error e
  | End_of_file -> close t; Error (Transport "unexpected EOF")
  | ex when io_failure ex ->
      close t; Error (Transport (Printexc.to_string ex))
  | ex ->
      let bt = Printexc.get_raw_backtrace () in
      close t;
      Printexc.raise_with_backtrace ex bt

let locked t f = Eio.Mutex.use_ro t.mutex (fun () -> protect t f)

(* RFC 3501 AUTHENTICATE exchange and RFC 2195 CRAM-MD5. A malformed or
   unexpected continuation closes the session before any secret-derived
   response is sent. A tagged rejection leaves the connection usable. *)
let authenticate_cram_md5 t auth =
  check_open t;
  let respond = Auth.cram_md5_response auth in
  let tag = next_tag t in
  let sent = ref false in
  try
    write t (tag ^ " AUTHENTICATE CRAM-MD5\r\n");
    sent := true;
    let rec challenge skipped =
      match parse_active t (read_response t) with
      | Imap.Response.Continuation encoded ->
          if String.length encoded > 16384 then
            raise (Failure (Limit "CRAM-MD5 challenge exceeds 16 KiB"));
          let decoded = match Base64.decode encoded with
          | Ok data when data <> "" && Base64.encode_string data = encoded -> data
          | _ -> raise (Failure (Protocol "invalid CRAM-MD5 challenge")) in
          write t (respond decoded ^ "\r\n")
      | Imap.Response.Tagged {tag=got; status=(`No | `Bad as status); code; text}
        when got = tag -> raise (Failure (Rejected {tag; status; code; text}))
      | Imap.Response.Untagged (Imap.Response.Bye (_, text)) ->
          raise (Failure (Protocol ("server BYE: " ^ text)))
      | Imap.Response.Untagged _ when skipped < 32 -> challenge (skipped + 1)
      | _ -> raise (Failure (Protocol "expected CRAM-MD5 challenge"))
    in
    challenge 0;
    let rec completion skipped =
      match parse_active t (read_response t) with
      | Imap.Response.Tagged {tag=got; status=`Ok; _} when got = tag -> ()
      | Imap.Response.Tagged {tag=got; status=(`No | `Bad as status); code; text}
        when got = tag -> raise (Failure (Rejected {tag; status; code; text}))
      | Imap.Response.Untagged (Imap.Response.Bye (_, text)) ->
          raise (Failure (Protocol ("server BYE: " ^ text)))
      | Imap.Response.Untagged _ when skipped < 32 -> completion (skipped + 1)
      | _ -> raise (Failure (Protocol "unexpected CRAM-MD5 completion"))
    in completion 0
  with ex ->
    let bt = Printexc.get_raw_backtrace () in
    abandon t ~sent:!sent ex bt

(* RFC 4959 initial response, RFC 4616 PLAIN and RFC 7628 OAUTHBEARER.
   The bearer error continuation must be acknowledged before tagged failure.
   Server diagnostics are deliberately not exposed or retained here. *)
let authenticate_initial t ~mechanism ~encoded ~sasl_ir ~oauthbearer =
  check_open t;
  if mechanism <> "PLAIN" && mechanism <> "OAUTHBEARER" then
    invalid_arg "unsupported initial SASL mechanism";
  let tag = next_tag t in
  let sent = ref false in
  let response () = parse_active t (read_response t) in
  let rejected status code =
    raise (Failure (Rejected {tag; status; code;
      text="authentication rejected"})) in
  let rec await_initial skipped =
    match response () with
    | Imap.Response.Continuation "" -> write t (encoded ^ "\r\n")
    | Imap.Response.Tagged {tag=got; status=(`No | `Bad as status); code; _}
      when got = tag -> rejected status code
    | Imap.Response.Untagged (Imap.Response.Bye _) ->
        raise (Failure (Protocol "server BYE during AUTHENTICATE"))
    | Imap.Response.Untagged _ when skipped < 32 -> await_initial (skipped + 1)
    | _ -> raise (Failure (Protocol "expected empty SASL continuation")) in
  let rec await_completion skipped =
    match response () with
    | Imap.Response.Tagged {tag=got; status=`Ok; _} when got=tag -> ()
    | Imap.Response.Tagged {tag=got; status=(`No | `Bad as status); code; _}
      when got=tag -> rejected status code
    | Imap.Response.Continuation error when oauthbearer ->
        if String.length error > 16384 ||
           (match Base64.decode error with
            | Ok decoded -> Base64.encode_string decoded <> error
            | Error _ -> true) then
          raise (Failure (Protocol "invalid OAUTHBEARER error continuation"));
        write t "AQ==\r\n";
        let rec final skipped = match response () with
        | Imap.Response.Tagged {tag=got; status=(`No | `Bad as status); code; _}
          when got=tag -> rejected status code
        | Imap.Response.Untagged (Imap.Response.Bye _) ->
            raise (Failure (Protocol "server BYE during AUTHENTICATE"))
        | Imap.Response.Untagged _ when skipped < 32 -> final (skipped + 1)
        | _ -> raise (Failure (Protocol "unexpected OAUTHBEARER completion"))
        in final 0
    | Imap.Response.Untagged (Imap.Response.Bye _) ->
        raise (Failure (Protocol "server BYE during AUTHENTICATE"))
    | Imap.Response.Untagged _ when skipped < 32 -> await_completion (skipped + 1)
    | _ -> raise (Failure (Protocol "unexpected SASL continuation or completion")) in
  try
    write t (tag ^ " AUTHENTICATE " ^ mechanism ^
      (if sasl_ir then " " ^ encoded else "") ^ "\r\n");
    sent := true;
    if not sasl_ir then await_initial 0;
    await_completion 0
  with ex ->
    let bt = Printexc.get_raw_backtrace () in
    abandon t ~sent:!sent ex bt
