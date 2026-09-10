module Mirror = Jmap.Mirror
module P = Jmap.Proto
module Chain = Jmap.Chain

let invalid () = invalid_arg "Invalid JMAP calendar response."

let received value =
  match P.Id.of_string_received value with Ok id -> id | Error _ -> invalid ()

let strings = List.map P.Id.to_string

let integer n =
  if n < 0L || n > Int64.of_int max_int then invalid ();
  Int64.to_int n

type kind = Calendar | Event | Identity

let kinds = [ Calendar; Identity; Event ]

let kind_name = function
  | Calendar -> "Calendar"
  | Event -> "CalendarEvent"
  | Identity -> "ParticipantIdentity"

let kind_of_string = function
  | "Calendar" -> Calendar
  | "CalendarEvent" -> Event
  | "ParticipantIdentity" -> Identity
  | _ -> invalid_arg "Unknown calendar resource type."

let capability = "urn:ietf:params:jmap:calendars"
let core = "urn:ietf:params:jmap:core"

type identity = { account : string; username : string; page_size : int }
type receipt = { method_name : string; request : string; response : string }
type item = { remote_id : string; raw : string; ical : string option }

type fetched = {
  state : string;
  items : item list;
  not_found : string list;
  receipts : receipt list;
  ical_supported : bool;
}

type changes = {
  old_state : string;
  new_state : string;
  more : bool;
  created : string list;
  updated : string list;
  destroyed : string list;
  receipt : receipt;
}

type page = {
  query_state : string;
  position : int;
  ids : string list;
  total : int option;
  receipt : receipt;
}

exception Expired_state of receipt
exception Method_error of Jmap.Proto.Error.Method_error.t * receipt

type t = { client : Client.t; pinned : identity; mutex : Eio.Mutex.t }

let checked = function
  | Ok value -> value
  | Error error -> raise (Client.Jmap_client_error error)

let identity t =
  let session = Client.session t.client in
  if
    session.username <> t.pinned.username
    || (not (List.mem_assoc capability session.capabilities))
    || not
         (List.exists
            (fun (id, (a : P.Session.Account.t)) ->
              P.Id.to_string id = t.pinned.account
              && List.mem_assoc capability a.account_capabilities)
            session.accounts)
  then
    invalid_arg "The selected account is unavailable or lacks JMAP Calendars.";
  t.pinned

let create ?account_id client =
  let session = Client.session client in
  let account =
    match account_id with
    | Some value -> P.Id.to_string (received value)
    | None -> (
        match P.Session.primary_account_for capability session with
        | Some id -> P.Id.to_string id
        | None -> invalid_arg "No primary JMAP calendar account.")
  in
  let page_size =
    match Sync.max_objects_in_get client with
    | None -> 100
    | Some n -> integer (min 100L n)
  in
  if page_size < 1 then invalid_arg "Invalid JMAP get limit.";
  let pinned = { account; username = session.username; page_size } in
  let t = { client; pinned; mutex = Eio.Mutex.create () } in
  ignore (identity t);
  t

let locked t f =
  match
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        try Ok (f ()) with exn -> Error (exn, Printexc.get_raw_backtrace ()))
  with
  | Ok value -> value
  | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

let call t build =
  let account = identity t in
  let request, handle =
    Chain.build ~capabilities:[ core; capability ]
      (build ~account_id:(received account.account))
  in
  let response = checked (Client.request t.client request) in
  ignore (identity t);
  let receipt =
    {
      method_name = Chain.method_name handle;
      request = Codec.encode_exn P.Request.jsont request;
      response =
        (match P.Response.source response with
        | Some source -> source
        | None -> invalid ());
    }
  in
  match Chain.parse (Chain.attempt handle) response with
  | Ok (Ok value) -> (value, response, receipt)
  | Ok (Error error) ->
      if error.P.Error.Method_error.type_ = `Cannot_calculate_changes then
        raise (Expired_state receipt);
      raise (Method_error (error, receipt))
  | Error (Chain.Json_error error) ->
      raise (Client.Jmap_client_error (Client.Json_error error))
  | Error (Chain.Method_error error) -> raise (Method_error (error, receipt))

let check_account t account =
  if P.Id.to_string account <> (identity t).account then invalid ()

let fetch t ~ids build project =
  let result, response, receipt = call t build in
  let result : _ P.Method.get_response = result in
  check_account t result.account_id;
  let items =
    List.map
      (fun value ->
        let id, meta = project value in
        let remote_id =
          match id with Some id -> P.Id.to_string id | None -> invalid ()
        in
        let raw =
          match P.Response.source_fragment response meta with
          | Some source -> source
          | None -> invalid ()
        in
        { remote_id; raw; ical = None })
      result.list
  in
  let not_found = strings result.not_found in
  let returned = List.map (fun (i : item) -> i.remote_id) items @ not_found in
  let unique = List.sort_uniq String.compare in
  if List.length returned <> List.length (unique returned) then invalid ();
  (match ids with
  | None -> if not_found <> [] then invalid ()
  | Some wanted ->
      if List.sort String.compare wanted <> List.sort String.compare returned
      then invalid ());
  ( result,
    {
      state = result.state;
      items;
      not_found;
      receipts = [ receipt ];
      ical_supported = true;
    } )

let archive ?(include_ical = false) t kind ~ids =
  locked t @@ fun () ->
  if kind = Event && ids = None then
    invalid_arg "Event reads require bounded IDs.";
  Option.iter
    (fun ids ->
      if
        List.length ids > (identity t).page_size
        || List.length ids <> List.length (List.sort_uniq String.compare ids)
      then
        invalid_arg "Calendar reads require distinct IDs within the get limit.")
    ids;
  let requested =
    Option.map (fun ids -> Chain.ids (List.map received ids)) ids
  in
  match kind with
  | Calendar ->
      snd
        (fetch t ~ids
           (fun ~account_id -> Chain.calendar_get ~account_id ?ids:requested ())
           (fun (v : P.Calendar.t) -> (v.id, v.meta)))
  | Identity ->
      snd
        (fetch t ~ids
           (fun ~account_id ->
             Chain.participant_identity_get ~account_id ?ids:requested ())
           (fun (v : P.Participant_identity.t) -> (v.id, v.meta)))
  | Event -> (
      let _, base =
        fetch t ~ids
          (fun ~account_id ->
            Chain.calendar_event_get ~account_id ?ids:requested ())
          (fun (v : P.Calendar_event.t) -> (v.id, v.meta))
      in
      if (not include_ical) || base.items = [] then base
      else
        try
          let typed, side =
            fetch t ~ids
              (fun ~account_id ->
                Chain.calendar_event_get ~account_id ?ids:requested
                  ~properties:[ `Id; `Icalendar ] ())
              (fun (v : P.Calendar_event.t) -> (v.id, v.meta))
          in
          if
            side.state <> base.state
            || List.sort String.compare side.not_found
               <> List.sort String.compare base.not_found
          then
            invalid_arg
              "Calendar changed while reading both representations. Sync will \
               retry.";
          let items =
            List.map
              (fun (item : item) ->
                let side_item =
                  List.find
                    (fun (i : item) -> i.remote_id = item.remote_id)
                    side.items
                in
                let value =
                  List.find
                    (fun (v : P.Calendar_event.t) ->
                      Option.map P.Id.to_string v.id = Some item.remote_id)
                    typed.list
                in
                {
                  item with
                  ical = Option.map (fun _ -> side_item.raw) value.icalendar;
                })
              base.items
          in
          { base with items; receipts = base.receipts @ side.receipts }
        with
        | Method_error (error, receipt)
        when List.mem error.P.Error.Method_error.type_
               [
                 `Invalid_arguments;
                 `Other "unknownProperty";
                 `Other "unsupportedProperty";
               ]
        ->
          {
            base with
            receipts = base.receipts @ [ receipt ];
            ical_supported = false;
          })

let changes t kind ~since =
  locked t @@ fun () ->
  let identity = identity t in
  let max_changes = Int64.of_int identity.page_size in
  let build ~account_id =
    match kind with
    | Calendar ->
        Chain.calendar_changes ~account_id ~since_state:since ~max_changes ()
    | Event ->
        Chain.calendar_event_changes ~account_id ~since_state:since ~max_changes
          ()
    | Identity ->
        Chain.participant_identity_changes ~account_id ~since_state:since
          ~max_changes ()
  in
  let r, _, receipt = call t build in
  check_account t r.account_id;
  let value =
    {
      old_state = r.old_state;
      new_state = r.new_state;
      more = r.has_more_changes;
      created = strings r.created;
      updated = strings r.updated;
      destroyed = strings r.destroyed;
      receipt;
    }
  in
  if
    value.old_state <> since
    || (value.more && value.new_state = since)
    || List.length value.created + List.length value.updated
       + List.length value.destroyed
       > identity.page_size
  then invalid ();
  List.iter
    (fun ids ->
      if List.length ids <> List.length (List.sort_uniq String.compare ids) then
        invalid ())
    [ value.created; value.updated; value.destroyed ];
  value

let page t ~position =
  locked t @@ fun () ->
  if position < 0 then invalid_arg "Query position must be non-negative.";
  let identity = identity t in
  let r, _, receipt =
    call t (fun ~account_id ->
        Chain.calendar_event_query ~account_id ~position:(Int64.of_int position)
          ~limit:(Int64.of_int identity.page_size)
          ~calculate_total:true ~expand_recurrences:false
          ~sort:[ P.Calendar_event.sort `Uid ]
          ())
  in
  check_account t r.account_id;
  let value =
    {
      query_state = r.query_state;
      position = integer r.position;
      ids = strings r.ids;
      total = Option.map integer r.total;
      receipt;
    }
  in
  if
    value.position <> position
    || List.length value.ids > identity.page_size
    || List.length value.ids
       <> List.length (List.sort_uniq String.compare value.ids)
    || value.ids = []
       && Option.fold ~none:false ~some:(fun n -> n > position) value.total
  then invalid ();
  value

let download t ~blob =
  locked t @@ fun () ->
  let identity = identity t in
  checked
    (Client.download t.client
       ~account_id:(received identity.account)
       ~blob_id:(received blob) ())

let mirror_source t kind =
  let id = identity t in
  {
    Mirror.get =
      (fun ~ids ->
        let f = archive ~include_ical:true t kind ~ids in
        {
          Mirror.state = f.state;
          items = f.items;
          not_found = f.not_found;
          receipts = f.receipts;
        });
    changes =
      (fun ~since ->
        try
          let c = changes t kind ~since in
          Ok
            {
              Mirror.old_state = c.old_state;
              new_state = c.new_state;
              more = c.more;
              created = c.created;
              updated = c.updated;
              destroyed = c.destroyed;
              receipts = [ c.receipt ];
            }
        with Expired_state receipt -> Error [ receipt ]);
    page =
      (if kind <> Event then None
       else
         Some
           (fun ~position ->
             let p = page t ~position in
             {
               Mirror.query_state = p.query_state;
               position = p.position;
               ids = p.ids;
               total = p.total;
               receipts = [ p.receipt ];
             }));
    id = (fun (i : item) -> i.remote_id);
    batch_size = id.page_size;
  }
