(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Proto = Jmap.Proto

type error =
  | Client_error of Client.error
  | No_mailbox_with_role of Proto.Mailbox.role
  | Mailbox_missing_id of Proto.Mailbox.role
  | Query_state_changed of { previous : string; current : string }
  | Nonadvancing_query of { requested : int64; returned : int64 }
  | Page_fuel_exhausted of int
  | Nonadvancing_changes of string
  | Mismatched_changes_state of { requested : string; returned : string }

let pp_error ppf = function
  | Client_error e -> Client.pp_error ppf e
  | No_mailbox_with_role r ->
      Format.fprintf ppf "no mailbox has the role %a" Proto.Error.pp_escaped
        (Proto.Mailbox.role_to_string r)
  | Mailbox_missing_id r ->
      Format.fprintf ppf "the mailbox with role %a has no id"
        Proto.Error.pp_escaped
        (Proto.Mailbox.role_to_string r)
  | Query_state_changed { previous; current } ->
      Format.fprintf ppf "query state changed while paging from %a to %a"
        Proto.Error.pp_escaped previous Proto.Error.pp_escaped current
  | Nonadvancing_query { requested; returned } ->
      Format.fprintf ppf
        "query did not advance: requested position %Ld but server returned %Ld"
        requested returned
  | Page_fuel_exhausted fuel ->
      Format.fprintf ppf "query paging exhausted its %d-request fuel" fuel
  | Nonadvancing_changes state ->
      Format.fprintf ppf "changes did not advance from state %a"
        Proto.Error.pp_escaped state
  | Mismatched_changes_state { requested; returned } ->
      Format.fprintf ppf "changes oldState %a does not match sinceState %a"
        Proto.Error.pp_escaped returned Proto.Error.pp_escaped requested

let error_to_string e = Format.asprintf "%a" pp_error e
let client_error = function Ok v -> Ok v | Error e -> Error (Client_error e)

let invalid_request message =
  Error
    (Client_error (Client.Transport (Fetch.Invalid_request message, message)))

let core_limits client = Proto.Session.core_capability (Client.session client)
let limit f client = Option.map f (core_limits client)

let max_calls_in_request =
  limit (fun (c : Proto.Capability.Core.t) -> c.max_calls_in_request)

let max_objects_in_get =
  limit (fun (c : Proto.Capability.Core.t) -> c.max_objects_in_get)

let max_objects_in_set =
  limit (fun (c : Proto.Capability.Core.t) -> c.max_objects_in_set)

let max_concurrent_requests =
  limit (fun (c : Proto.Capability.Core.t) -> c.max_concurrent_requests)

let max_concurrent_upload =
  limit (fun (c : Proto.Capability.Core.t) -> c.max_concurrent_upload)

let default_max_objects_in_get = 4096L
let default_page_fuel = 1000

let check_positive_uint53 caller name value =
  match Proto.Int53.Unsigned.of_int64 value with
  | Ok _ when value > 0L -> ()
  | Ok _ | Error _ ->
      invalid_arg (Fmt.str "%s: %s must be between 1 and 2^53-1" caller name)

let calls_in_chain ~capabilities c =
  List.length (Chain.build_request ~capabilities c).Proto.Request.method_calls

let chain_fits client ?capabilities c =
  let capabilities =
    Option.value capabilities ~default:(Client.default_capabilities client)
  in
  match max_calls_in_request client with
  | None -> true
  | Some max -> Int64.of_int (calls_in_chain ~capabilities c) <= max

(* [v] is positive: [get_all] rejects a batch of zero before chunking. *)
let to_positive_int v =
  if v > Int64.of_int max_int then max_int else Int64.to_int v

(* What makes one page recognisable as the previous one again: the first id
   it answered with and how many it sent. The position it echoed is no
   evidence, since a page reporting anything but the position requested has
   already ended the walk, so a server re-serving a page while echoing the
   position asked for would otherwise go undetected. *)
let page_signature (q : Proto.Method.query_response) ~got =
  (Option.map Proto.Id.to_string (List.nth_opt q.ids 0), got)

let same_page (i, g) (i', g') = Option.equal String.equal i i' && Int.equal g g'

let pages client ?capabilities ?(page_size = 50L) ?(fuel = default_page_fuel)
    query =
  check_positive_uint53 "Jmap_eio.Sync.pages" "page_size" page_size;
  if fuel < 1 then invalid_arg "Jmap_eio.Sync.pages: fuel must be at least 1";
  let initial_fuel = fuel in
  let exhausted () =
    Seq.Cons (Error (Page_fuel_exhausted initial_fuel), Seq.empty)
  in
  let rec step ~position ~limit ~previous ~query_state ~fuel () =
    match
      client_error (Client.call client ?capabilities (query ~position ~limit))
    with
    | Error e -> Seq.Cons (Error e, Seq.empty)
    | Ok (q : Proto.Method.query_response) -> (
        match query_state with
        | Some previous when not (String.equal previous q.query_state) ->
            Seq.Cons
              ( Error (Query_state_changed { previous; current = q.query_state }),
                Seq.empty )
        | None | Some _ ->
            let query_state = Some q.query_state in
            let ids = q.ids in
            let got = List.length ids in
            if got = 0 then Seq.Nil
            else if not (Int64.equal q.position position) then
              Seq.Cons
                ( Error
                    (Nonadvancing_query
                       { requested = position; returned = q.position }),
                  Seq.empty )
            else
              let signature = page_signature q ~got in
              if Option.fold ~none:false ~some:(same_page signature) previous
              then
                Seq.Cons
                  ( Error
                      (Nonadvancing_query
                         { requested = position; returned = q.position }),
                    Seq.empty )
              else
                (* RFC 8620 Section 5.5: the response "limit" is the limit the
               server applied, "only returned if the server set a limit or used
               a different limit than that given in the request", so it is the
               one to page with from here on. *)
                let limit = match q.limit with Some l -> l | None -> limit in
                let more = limit > 0L && Int64.of_int got >= limit in
                let next = Int64.add position (Int64.of_int got) in
                Seq.Cons
                  ( Ok ids,
                    if more && fuel = 1 then exhausted
                    else if more then
                      step ~position:next ~limit ~previous:(Some signature)
                        ~query_state ~fuel:(fuel - 1)
                    else Seq.empty ))
  in
  step ~position:0L ~limit:page_size ~previous:None ~query_state:None ~fuel

let all_ids client ?capabilities ?page_size ?fuel ?max query =
  (match max with
  | Some max when max < 0 ->
      invalid_arg "Jmap_eio.Sync.all_ids: max must be non-negative"
  | None | Some _ -> ());
  let seq = pages client ?capabilities ?page_size ?fuel query in
  let rec go ~have acc seq =
    let room = Option.map (fun m -> m - have) max in
    match room with
    | Some n when n <= 0 -> Ok (List.rev acc)
    | room -> (
        match seq () with
        | Seq.Nil -> Ok (List.rev acc)
        | Seq.Cons (Error e, _) -> Error e
        | Seq.Cons (Ok ids, rest) ->
            let got = List.length ids in
            let ids, got =
              match room with
              | Some n when n < got -> ((let rec take n xs = match xs with
                 | _ when n <= 0 -> []
                 | [] -> []
                 | x :: xs -> x :: take (n - 1) xs in take n ids), n)
              | _ -> (ids, got)
            in
            go ~have:(have + got) (List.rev_append ids acc) rest)
  in
  go ~have:0 [] seq

type changes = {
  created : Proto.Id.t list;
  updated : Proto.Id.t list;
  destroyed : Proto.Id.t list;
  new_state : string;
  has_more : bool;
}

type mailbox_delta = {
  changes : changes;
  updated_properties : string list option;
}

type 'a delta = [ `Changes of 'a | `Cannot_calculate_changes ]

(* The state an id ends up in once every round has been folded in, per the
   rules of RFC 8620 Section 5.2. [Gone] is the "created AND destroyed since
   the old state" case, which is reported in no list at all. *)
type id_state = Created | Updated | Destroyed | Gone

(* Each list has a rule of its own for what an id already in the table
   becomes, and [Destroyed] and [Gone] survive whichever list a later round
   names the id in. That matters across rounds, not within one: RFC 8620
   Section 5.2 lets a server return an updated-and-destroyed id in both
   lists, and a server may repeat an id around the [hasMoreChanges] boundary,
   so a last-writer-wins fold would resurrect a record the server deleted. *)
let fold_rounds rounds new_state ~has_more =
  let tbl = Hashtbl.create 64 in
  let seen = ref [] in
  let note id next =
    let key = Proto.Id.to_string id in
    let prev = Hashtbl.find_opt tbl key in
    if prev = None then seen := (key, id) :: !seen;
    Hashtbl.replace tbl key (next prev)
  in
  List.iter
    (fun (c : Proto.Method.changes_response) ->
      List.iter
        (fun id ->
          note id (function
            | Some Destroyed -> Destroyed
            | Some Gone -> Gone
            | _ -> Created))
        c.created;
      (* "created AND updated ... return the id in the created list" *)
      List.iter
        (fun id ->
          note id (function
            | Some Created -> Created
            | Some Destroyed -> Destroyed
            | Some Gone -> Gone
            | _ -> Updated))
        c.updated;
      (* "created AND destroyed ... remove the id from the response
         entirely"; "updated AND destroyed ... just the destroyed list" *)
      List.iter
        (fun id ->
          note id (function Some (Created | Gone) -> Gone | _ -> Destroyed))
        c.destroyed)
    rounds;
  let order = List.rev !seen in
  let pick want =
    List.filter_map
      (fun (key, id) ->
        match Hashtbl.find_opt tbl key with
        | Some st when st = want -> Some id
        | _ -> None)
      order
  in
  {
    created = pick Created;
    updated = pick Updated;
    destroyed = pick Destroyed;
    new_state;
    has_more;
  }

(* The drain shared by every /changes method. [base] projects the standard
   response out of whatever the method returns (Mailbox/changes returns more,
   RFC 8621 Section 2.2) and [round] folds the extra part of each response
   into an accumulator. *)
let drain client ?capabilities ~since ?(max_changes = 256L) ?(fuel = 100) ~base
    ~init ~round build =
  check_positive_uint53 "Jmap_eio.Sync" "max_changes" max_changes;
  if fuel < 1 then invalid_arg "Jmap_eio.Sync: fuel must be at least 1";
  let seen_states = Hashtbl.create 16 in
  Hashtbl.add seen_states since ();
  let rec go ~since_state ~fuel ~rounds ~acc =
    let attempted = Chain.attempt_call (build ~since_state ~max_changes) in
    match client_error (Client.call client ?capabilities attempted) with
    | Error e -> Error e
    (* RFC 8620 Section 5.2: not a failure, an instruction to resync. *)
    | Ok
        (Error { Proto.Error.Method_error.type_ = `Cannot_calculate_changes; _ })
      ->
        Ok `Cannot_calculate_changes
    | Ok (Error e) -> Error (Client_error (Client.Method_error e))
    | Ok (Ok v) ->
        let c : Proto.Method.changes_response = base v in
        if not (String.equal c.old_state since_state) then
          Error
            (Mismatched_changes_state
               { requested = since_state; returned = c.old_state })
        else
          let rounds = c :: rounds in
          let acc = round acc v in
          let state = c.Proto.Method.new_state in
          if c.Proto.Method.has_more_changes && Hashtbl.mem seen_states state
          then Error (Nonadvancing_changes state)
          else if c.Proto.Method.has_more_changes && fuel > 1 then begin
            Hashtbl.add seen_states state ();
            go ~since_state:state ~fuel:(fuel - 1) ~rounds ~acc
          end
          else
            let has_more = c.Proto.Method.has_more_changes in
            Ok (`Changes (fold_rounds (List.rev rounds) state ~has_more, acc))
  in
  go ~since_state:since ~fuel ~rounds:[] ~acc:init

let changes client ?capabilities ~since ?max_changes ?fuel build =
  match
    drain client ?capabilities ~since ?max_changes ?fuel ~base:Fun.id ~init:()
      ~round:(fun () _ -> ())
      build
  with
  | Error e -> Error e
  | Ok `Cannot_calculate_changes -> Ok `Cannot_calculate_changes
  | Ok (`Changes (c, ())) -> Ok (`Changes c)

let mail_capabilities = [ Proto.Capability.core; Proto.Capability.mail ]

let email_changes client ?(capabilities = mail_capabilities) ~account_id ~since
    ?max_changes ?fuel () =
  changes client ~capabilities ~since ?max_changes ?fuel
    (fun ~since_state ~max_changes ->
      Chain.email_changes ~account_id ~since_state ~max_changes ())

let thread_changes client ?(capabilities = mail_capabilities) ~account_id ~since
    ?max_changes ?fuel () =
  changes client ~capabilities ~since ?max_changes ?fuel
    (fun ~since_state ~max_changes ->
      Chain.thread_changes ~account_id ~since_state ~max_changes ())

let mailbox_changes client ?(capabilities = mail_capabilities) ~account_id
    ~since ?max_changes ?fuel () =
  (* RFC 8621 Section 2.2: updatedProperties is "String[]|null", and a null
     round cannot be narrowed by a later one, so None wins over any union. *)
  let round props (r : Chain.mailbox_changes_response) =
    match (props, r.updated_properties) with
    | None, _ | _, None -> None
    | Some acc, Some p ->
        Some (acc @ List.filter (fun x -> not (List.mem x acc)) p)
  in
  match
    drain client ~capabilities ~since ?max_changes ?fuel
      ~base:(fun (r : Chain.mailbox_changes_response) -> r.changes)
      ~init:(Some []) ~round
      (fun ~since_state ~max_changes ->
        Chain.mailbox_changes ~account_id ~since_state ~max_changes ())
  with
  | Error e -> Error e
  | Ok `Cannot_calculate_changes -> Ok `Cannot_calculate_changes
  | Ok (`Changes (c, updated_properties)) ->
      Ok (`Changes { changes = c; updated_properties })

let chunk n l =
  let rec go acc cur count = function
    | [] -> List.rev (if cur = [] then acc else List.rev cur :: acc)
    | x :: rest ->
        if count = n then go (List.rev cur :: acc) [ x ] 1 rest
        else go acc (x :: cur) (count + 1) rest
  in
  go [] [] 0 l

let get_all client ?capabilities ?batch ?max_concurrent ids get =
  let server_batch = max_objects_in_get client in
  let batch =
    match batch with
    | None -> Option.value server_batch ~default:default_max_objects_in_get
    | Some b ->
        check_positive_uint53 "Jmap_eio.Sync.get_all" "batch" b;
        Option.fold ~none:b ~some:(Int64.min b) server_batch
  in
  let max_fibers =
    match max_concurrent with
    | Some n ->
        if n < 1 then
          invalid_arg "Jmap_eio.Sync.get_all: max_concurrent must be positive";
        n
    | None -> fst (Client.concurrency_limits client)
  in
  match ids with
  | [] -> Ok ([], [])
  | _ when batch <= 0L ->
      invalid_request
        "the session advertises maxObjectsInGet=0, so no non-empty /get can be \
         sent"
  | _ -> (
      let batches = chunk (to_positive_int batch) ids in
      let count = List.length batches in
      let pending = ref (List.mapi (fun i ids -> (i, ids)) batches) in
      let results = Array.make count None in
      let first_error = ref None in
      let rec worker () =
        match (!first_error, !pending) with
        | Some _, _ | None, [] -> ()
        | None, (i, ids) :: rest -> (
            pending := rest;
            match
              client_error (Client.call client ?capabilities (get ~ids))
            with
            | Error error ->
                if Option.is_none !first_error then first_error := Some error
            | Ok response ->
                results.(i) <- Some response;
                worker ())
      in
      let workers = Int.min max_fibers count in
      Eio.Fiber.List.iter (fun _ -> worker ()) (List.init workers Fun.id);
      match !first_error with
      | Some error -> Error error
      | None ->
          let results =
            Array.to_list
              (Array.map
                 (function Some result -> result | None -> assert false)
                 results)
          in
          let rec fold objs not_found = function
            | [] -> (List.rev objs, List.rev not_found)
            | (g : _ Proto.Method.get_response) :: rest ->
                fold
                  (List.rev_append g.list objs)
                  (List.rev_append g.not_found not_found)
                  rest
          in
          Ok (fold [] [] results))

let mailbox_with_role client ?(capabilities = mail_capabilities) ~account_id
    role =
  let query () =
    Chain.mailbox_query ~account_id
      ~filter:
        (Proto.Filter.Condition
           { Proto.Mailbox.Filter_condition.empty with role = Some (Some role) })
      ~limit:1L ()
  in
  let mailbox_of_get (g : _ Proto.Method.get_response) =
    match g.list with [] -> None | mailbox :: _ -> Some mailbox
  in
  match max_calls_in_request client with
  | Some max when max < 1L ->
      invalid_request "the session permits no method calls in a request"
  | Some 1L -> (
      match client_error (Client.call client ~capabilities (query ())) with
      | Error _ as error -> error
      | Ok (q : Proto.Method.query_response) -> (
          match q.ids with
          | [] -> Ok None
          | id :: _ ->
              Result.map mailbox_of_get
                (client_error
                   (Client.call client ~capabilities
                      (Chain.mailbox_get ~account_id ~ids:(Chain.id id) ())))))
  | None | Some _ ->
      Result.map mailbox_of_get
        (client_error
           (Client.call client ~capabilities
              (Chain.mailbox_by_role ~account_id role)))

let mailbox_id client ?capabilities ~account_id role =
  match mailbox_with_role client ?capabilities ~account_id role with
  | Error e -> Error e
  | Ok (Some { Proto.Mailbox.id = Some id; _ }) -> Ok id
  | Ok (Some _) -> Error (Mailbox_missing_id role)
  | Ok None -> Error (No_mailbox_with_role role)

exception Sync_error of error

let () =
  Printexc.register_printer (function
    | Sync_error e -> Some ("Jmap_eio.Sync.Sync_error: " ^ error_to_string e)
    | _ -> None)

let mailbox_id_exn client ?capabilities ~account_id role =
  match mailbox_id client ?capabilities ~account_id role with
  | Ok id -> id
  | Error e -> raise (Sync_error e)
