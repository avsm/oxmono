type error = Error.t =
  | Closed
  | Protocol of string
  | Transport of string
  | Rejected of { tag : string; status : [ `No | `Bad ];
      code : Imap.Response.code option; text : string }
  | State of string
  | Missing_uid of int64
  | Limit of string
  | Uncertain of string

type t = {
  mutable flow : Transport.flow;
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
  mutable capabilities : string list;
  mutable enabled : string list;
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
    capabilities = []; enabled = [];
    input = Cstruct.create 65536; read_size = 65536; max_metadata; max_responses;
    max_command_metadata }

let close t =
  if not t.closed then (
    t.closed <- true;
    t.generation <- t.generation + 1;
    try Eio.Cancel.protect (fun () -> Transport.close t.flow) with _ -> ())

let check_open t = if t.closed then raise (Failure Closed)

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

let read_response ?(on_literal=(fun _ -> ())) ?(on_literal_start=(fun _ -> ()))
    ?(collect_literals=true) t =
  let fetch_response=ref None in
  let rec loop acc size =
    match read_event t with
    | Imap.Wire.Literal_start n as event ->
        on_literal_start n;
        (match acc with
         | Imap.Wire.Text s::_ ->
             let marker=Printf.sprintf "{%Ld}\r\n" n in
             let k=String.length s-String.length marker in
             if !fetch_response=Some true && k>=8 &&
                String.sub s k (String.length marker)=marker &&
                String.uppercase_ascii (String.sub s (k-8) 8)="PREVIEW " &&
                n>1024L then
               raise (Failure (Limit "PREVIEW literal exceeds 1024 bytes"))
         | _ -> ());
        loop (event::acc) size
    | Imap.Wire.Literal_chunk s as event ->
        on_literal s;
        if collect_literals then (
          let size = size + String.length s in
          if size > t.max_metadata then
            raise (Failure (Limit "response literal exceeds metadata limit"));
          loop (event :: acc) size)
        else loop acc size
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
  let uidonly = List.mem "UIDONLY" t.enabled in
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
  let written = ref false in
  let partial_limit = ref None in
  try
    written := true;
    write t (tag ^ " " ^ syntax ^ "\r\n");
    let rec receive acc count total_bytes =
      let parts = read_response ?on_literal ?on_literal_start
        ~collect_literals:(Option.is_none on_literal) t in
      let total_bytes = Int64.add total_bytes (parts_size parts) in
      if total_bytes > Int64.of_int t.max_command_metadata then
        raise (Failure (Limit "command metadata exceeds configured limit"));
      let response = parse_active t parts in
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
      | Imap.Response.Untagged (Imap.Response.Bye (_, text)) ->
          raise (Failure (Protocol ("server BYE: " ^ text)))
      | Imap.Response.Continuation _ -> raise (Failure (Protocol "unexpected continuation"))
      | _ ->
          (match response with
           | Imap.Response.Untagged (Imap.Response.No
               (Some (Imap.Response.Messagelimit (limit,last)), _)) ->
               partial_limit := Some (limit,last)
           | _ -> ());
          if count >= t.max_responses then
            raise (Failure (Limit "command response count exceeds limit"));
          receive (response :: acc) (count + 1) total_bytes
    in receive [] 0 0L
  with ex ->
    (match ex with Failure (Rejected _) -> () | _ -> if !written then close t);
    (match ex with
     | Failure (Rejected _ | Uncertain _) -> raise ex
     | Eio.Cancel.Cancelled _ -> raise ex
     | _ when mutation && !written ->
         raise (Failure (Uncertain
           "mutation outcome unknown after command bytes were sent"))
     | _ -> raise ex)

let command ?on_literal ?on_literal_start ?mutation t syntax =
  (command_result ?on_literal ?on_literal_start ?mutation t syntax).untagged

let compress_deflate t =
  check_open t;
  if Transport.compressed t.flow then
    raise (Failure (State "COMPRESS is already active"));
  if not (List.mem "COMPRESS=DEFLATE" t.capabilities) then
    raise (Failure (State "COMPRESS=DEFLATE unavailable"));
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
    with ex -> close t; raise ex)

type append_part = { prefix : string; length : int64; read : Cstruct.t -> int; synchronizing : bool }

let append_many t parts =
  check_open t;
  if parts=[] || List.exists (fun part -> part.length<0L) parts then
    raise (Failure (State "invalid APPEND parts"));
  let tag = next_tag t in
  let written = ref false in
  let partial = ref false in
  try
    written := true;
    let rec continuation count total_bytes =
      let parts=read_response t in
      let total_bytes=Int64.add total_bytes (parts_size parts) in
      if total_bytes > Int64.of_int t.max_command_metadata then
        raise (Failure (Limit "APPEND continuation metadata exceeds limit"));
      let response=parse_active t parts in
      (match response with
       | Imap.Response.Untagged (Imap.Response.No
           (Some (Imap.Response.Messagelimit _), _)) -> partial:=true
       | _ -> ());
      match response with
      | Imap.Response.Continuation _ -> count,total_bytes
      | Imap.Response.Tagged {tag=got; status=(`No | `Bad as status); code; text}
        when got = tag -> raise (Failure (Rejected {tag; status; code; text}))
      | Imap.Response.Untagged (Imap.Response.Bye (_, text)) ->
          raise (Failure (Protocol ("server BYE: " ^ text)))
      | Imap.Response.Untagged _ ->
          if count >= t.max_responses then
            raise (Failure (Limit "APPEND continuation response count exceeds limit"));
          continuation (count+1) total_bytes
      | _ -> raise (Failure (Protocol "expected APPEND continuation"))
    in
    let buffer = Cstruct.create 65536 in
    let rec copy read left =
      if left > 0L then (
        let amount = Int64.to_int (Int64.min left 65536L) in
        let chunk = Cstruct.sub buffer 0 amount in
        let n = read chunk in
        if n = 0 then raise (Failure (State "APPEND source ended before declared length"));
        Transport.write t.flow [Cstruct.sub chunk 0 n];
        copy read (Int64.sub left (Int64.of_int n)))
    in
    let _,count,total_bytes=List.fold_left (fun (first,count,total_bytes) part ->
      write t ((if first then tag ^ " " else "") ^ part.prefix);
      let count,total_bytes=if part.synchronizing then continuation count total_bytes
        else count,total_bytes in
      copy part.read part.length;
      false,count,total_bytes) (true,0,0L) parts in
    Transport.write t.flow [Cstruct.of_string "\r\n"];
    let rec completion count total_bytes =
      let parts = read_response t in
      let total_bytes = Int64.add total_bytes (parts_size parts) in
      if total_bytes > Int64.of_int t.max_command_metadata then
        raise (Failure (Limit "APPEND completion metadata exceeds limit"));
      let response = parse_active t parts in
      (match response with
       | Imap.Response.Untagged (Imap.Response.No
           (Some (Imap.Response.Messagelimit _), _)) -> partial:=true
       | _ -> ());
      match response with
      | Imap.Response.Tagged {
          tag=got; status=`Ok;
          code=Some (Imap.Response.Messagelimit _); _} when got = tag ->
          raise (Failure (Uncertain
            "server reported a partial APPEND with MESSAGELIMIT"))
      | Imap.Response.Tagged {tag=got; status=`Ok; _} when got = tag ->
          if !partial then raise (Failure (Uncertain
            "server reported a partial APPEND before success"));
          response
      | Imap.Response.Tagged {tag=got; status=(`No | `Bad as status); code; text}
        when got = tag -> raise (Failure (Rejected {tag; status; code; text}))
      | Imap.Response.Tagged {tag=got; _} ->
          raise (Failure (Protocol ("unexpected tagged completion " ^ got)))
      | Imap.Response.Untagged (Imap.Response.Bye (_, text)) ->
          raise (Failure (Protocol ("server BYE: " ^ text)))
      | Imap.Response.Continuation _ ->
          raise (Failure (Protocol "unexpected APPEND continuation after final literal"))
      | _ ->
          if count >= t.max_responses then
            raise (Failure (Limit "APPEND completion response count exceeds limit"));
          completion (count + 1) total_bytes
    in completion count total_bytes
  with ex ->
    (match ex with Failure (Rejected _) -> () | _ -> if !written then close t);
    (match ex with
     | Failure (Rejected _) -> raise ex
     | Eio.Cancel.Cancelled _ -> raise ex
     | _ when !written ->
         raise (Failure (Uncertain "APPEND outcome unknown after command bytes were sent"))
     | _ -> raise ex)

let append ?(synchronizing=true) t ~prefix ~length source =
  append_many t [{prefix;length;read=Eio.Flow.single_read source;synchronizing}]

let logout t =
  Fun.protect ~finally:(fun () -> close t) (fun () ->
    check_open t;
    let tag=next_tag t in
    write t (tag ^ " " ^ Imap.Command.logout ^ "\r\n");
    let rec receive seen_bye count total_bytes =
      let parts=read_response t in
      let total_bytes=Int64.add total_bytes (parts_size parts) in
      if total_bytes>Int64.of_int t.max_command_metadata then
        raise (Failure (Limit "LOGOUT metadata exceeds limit"));
      match parse_active t parts with
      | Imap.Response.Tagged {tag=got;status=`Ok;_} when got=tag && seen_bye -> ()
      | Imap.Response.Tagged {tag=got;status=(`No | `Bad as status);code;text}
          when got=tag -> raise (Failure (Rejected {tag;status;code;text}))
      | Imap.Response.Tagged _ ->
          raise (Failure (Protocol "invalid LOGOUT completion or missing BYE"))
      | Imap.Response.Continuation _ ->
          raise (Failure (Protocol "unexpected LOGOUT continuation"))
      | Imap.Response.Untagged response ->
          if count>=t.max_responses then
            raise (Failure (Limit "LOGOUT response count exceeds limit"));
          let seen_bye=match response with
            | Imap.Response.Bye _ when seen_bye ->
                raise (Failure (Protocol "repeated LOGOUT BYE"))
            | Imap.Response.Bye _ -> true
            | _ -> seen_bye in
          receive seen_bye (count+1) total_bytes
    in receive false 0 0L)

let idle_once t =
  check_open t;
  let tag = next_tag t in
  let sent = ref false in
  let total_bytes=ref 0L in
  let count=ref 0 in
  let response () =
    let parts=read_response t in
    total_bytes := Int64.add !total_bytes (parts_size parts);
    if !total_bytes > Int64.of_int t.max_command_metadata then
      raise (Failure (Limit "IDLE metadata exceeds limit"));
    parse_active t parts in
  let fail_tagged got status code text =
    if got <> tag then
      raise (Failure (Protocol ("unexpected tagged completion " ^ got)))
    else match status with
      | `No | `Bad as status -> raise (Failure (Rejected {tag; status; code; text}))
      | `Ok -> raise (Failure (Protocol "IDLE completed before DONE")) in
  let add acc item =
    if !count >= t.max_responses then
      raise (Failure (Limit "IDLE event count exceeds limit"));
    incr count;
    item :: acc in
  try
    sent := true;
    write t (tag ^ " IDLE\r\n");
    let rec continuation acc =
      match response () with
      | Imap.Response.Continuation _ -> acc
      | Imap.Response.Untagged (Imap.Response.Bye (_, text)) ->
          raise (Failure (Protocol ("server BYE during IDLE: " ^ text)))
      | Imap.Response.Untagged _ as item -> continuation (add acc item)
      | Imap.Response.Tagged {tag=got; status; code; text} ->
          fail_tagged got status code text
    in
    let initial = continuation [] in
    let changed = match initial with
      | _::_ -> initial
      | [] ->
          (match response () with
           | Imap.Response.Untagged (Imap.Response.Bye (_, text)) ->
               raise (Failure (Protocol ("server BYE during IDLE: " ^ text)))
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
      | Imap.Response.Untagged (Imap.Response.Bye (_, text)) ->
          raise (Failure (Protocol ("server BYE during IDLE: " ^ text)))
      | Imap.Response.Untagged _ as item -> completion (add acc item)
      | Imap.Response.Continuation _ ->
          raise (Failure (Protocol "unexpected IDLE continuation"))
    in completion changed
  with ex ->
    if !sent then close t;
    raise ex

let protect t f =
  try Ok (f ()) with
  | Failure e -> Error e
  | End_of_file -> close t; Error (Transport "unexpected EOF")
  | Eio.Cancel.Cancelled _ as ex -> close t; raise ex
  | ex -> close t; Error (Transport (Printexc.to_string ex))

let locked t f = Eio.Mutex.use_ro t.mutex (fun () -> protect t f)

let authentication_rejected ~tag ~status ~code =
  let code=match code with
    | Some (Imap.Response.Unavailable | Authenticationfailed | Authorizationfailed
        | Expired | Privacyrequired | Contactadmin | Noperm | Inuse | Serverbug
        | Clientbug | Cannot | Limit as code) -> Some code
    | _ -> None in
  Failure (Rejected {tag;status;code;text="authentication rejected"})

(* RFC 3501 AUTHENTICATE exchange and RFC 2195 CRAM-MD5. A malformed or
   unexpected continuation closes the session before any secret-derived
   response is sent. A tagged rejection leaves the connection usable. *)
let authenticate_cram_md5 t auth =
  check_open t;
  let tag = next_tag t in
  let written = ref false in
  try
    written := true;
    write t (tag ^ " AUTHENTICATE CRAM-MD5\r\n");
    let rec challenge skipped =
      match parse_active t (read_response t) with
      | Imap.Response.Continuation encoded ->
          if String.length encoded > 16384 then
            raise (Failure (Limit "CRAM-MD5 challenge exceeds 16 KiB"));
          let decoded = match Base64.decode encoded with
          | Ok data when data <> "" && Base64.encode_string data = encoded -> data
          | _ -> raise (Failure (Protocol "invalid CRAM-MD5 challenge")) in
          let answer = Auth.cram_md5_response auth decoded in
          write t (answer ^ "\r\n")
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
    (match ex with Failure (Rejected _) -> () | _ -> if !written then close t);
    raise ex

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
    raise (authentication_rejected ~tag ~status ~code) in
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
    sent := true;
    write t (tag ^ " AUTHENTICATE " ^ mechanism ^
      (if sasl_ir then " " ^ encoded else "") ^ "\r\n");
    if not sasl_ir then await_initial 0;
    await_completion 0
  with ex ->
    (match ex with Failure (Rejected _) -> () | _ -> if !sent then close t);
    raise ex
