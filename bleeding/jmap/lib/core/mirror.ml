type phase = New | Listing | Catching_up | Live

type cursor = {
  phase : phase;
  state : string option;
  position : int;
  query_state : string option;
}

let initial = { phase = New; state = None; position = 0; query_state = None }

let max_position =
  Int64.to_int (min (Int64.of_int max_int) Proto_int53.Unsigned.max_value)

let cursor ~phase ?state ?(position = 0) ?query_state () =
  let valid =
    position >= 0 && position <= max_position
    &&
    match phase with
    | New -> state = None && position = 0 && query_state = None
    | Listing -> state <> None && (position = 0 || query_state <> None)
    | Catching_up -> state <> None && query_state <> None
    | Live -> state <> None
  in
  if not valid then invalid_arg "Invalid JMAP mirror cursor.";
  { phase; state; position; query_state }

let cursor_jsont =
  let position_jsont =
    let decode n =
      if n > Int64.of_int max_int then
        Jsont.Error.msg Jsont.Meta.none "Mirror position exceeds machine range.";
      Int64.to_int n
    in
    Jsont.map ~dec:decode ~enc:Int64.of_int Proto_int53.Unsigned.jsont
  in
  let phase_jsont : phase Jsont.t =
    Jsont.enum
      [
        ("new", New);
        ("listing", Listing);
        ("catchup", Catching_up);
        ("live", Live);
      ]
  in
  let make phase state position query_state =
    try cursor ~phase ?state ~position ?query_state ()
    with Invalid_argument message -> Jsont.Error.msg Jsont.Meta.none message
  in
  Jsont.Object.map ~kind:"Mirror cursor" make
  |> Jsont.Object.mem "phase" phase_jsont ~enc:(fun c -> c.phase)
  |> Jsont.Object.opt_mem "state" Jsont.string ~enc:(fun c -> c.state)
  |> Jsont.Object.mem "position" position_jsont ~enc:(fun c -> c.position)
  |> Jsont.Object.opt_mem "queryState" Jsont.string ~enc:(fun c ->
      c.query_state)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

type ('item, 'receipt) fetched = {
  state : string;
  items : 'item list;
  not_found : string list;
  receipts : 'receipt list;
}

type 'receipt changes = {
  old_state : string;
  new_state : string;
  more : bool;
  created : string list;
  updated : string list;
  destroyed : string list;
  receipts : 'receipt list;
}

type 'receipt page = {
  query_state : string;
  position : int;
  ids : string list;
  total : int option;
  receipts : 'receipt list;
}

type ('item, 'receipt) source = {
  get : ids:string list option -> ('item, 'receipt) fetched;
  changes : since:string -> ('receipt changes, 'receipt list) result;
  page : (position:int -> 'receipt page) option;
  id : 'item -> string;
  batch_size : int;
}

type ('item, 'receipt) update = {
  cursor : cursor;
  items : 'item list;
  destroyed : string list;
  receipts : 'receipt list;
  more : bool;
  publish : bool;
}

type ('item, 'receipt) step =
  | Restart of 'receipt list
  | Update of ('item, 'receipt) update

let invalid () = invalid_arg "Invalid JMAP mirror page or cursor."
let unique ids = List.sort_uniq String.compare ids

let check_ids ids =
  if List.exists (( = ) "") ids || List.length ids <> List.length (unique ids)
  then invalid ()

let fetch source ids =
  let fetched = source.get ~ids in
  let returned = List.map source.id fetched.items @ fetched.not_found in
  check_ids returned;
  (match ids with
  | None -> if fetched.not_found <> [] then invalid ()
  | Some ids -> if unique ids <> unique returned then invalid ());
  fetched

let chunks size ids =
  let rec take n acc rest =
    match (n, rest) with
    | 0, _ | _, [] -> (List.rev acc, rest)
    | _, x :: xs -> take (n - 1) (x :: acc) xs
  in
  let rec loop acc = function
    | [] -> List.rev acc
    | ids ->
        let batch, rest = take size [] ids in
        loop (batch :: acc) rest
  in
  loop [] ids

let fetch_ids source ids =
  List.fold_left
    (fun (items, missing, receipts) batch ->
      let f = fetch source (Some batch) in
      ( List.rev_append f.items items,
        List.rev_append f.not_found missing,
        List.rev_append f.receipts receipts ))
    ([], [], [])
    (chunks source.batch_size ids)
  |> fun (items, missing, receipts) ->
  (List.rev items, List.rev missing, List.rev receipts)

let step source (cursor : cursor) =
  if source.batch_size < 1 || cursor.position < 0 then invalid ();
  let update ?(publish = false) ?more cursor items destroyed receipts =
    let more = Option.value ~default:(cursor.phase <> Live) more in
    Update { cursor; items; destroyed; receipts; more; publish }
  in
  match cursor.phase with
  | New -> (
      let f = fetch source (Option.map (fun _ -> []) source.page) in
      match source.page with
      | None ->
          update ~publish:true
            { initial with phase = Live; state = Some f.state }
            f.items f.not_found f.receipts
      | Some _ ->
          update
            { initial with phase = Listing; state = Some f.state }
            [] [] f.receipts)
  | Listing ->
      let page = match source.page with Some f -> f | None -> invalid () in
      if cursor.state = None then invalid ();
      let p = page ~position:cursor.position in
      check_ids p.ids;
      if
        p.position <> cursor.position
        || List.length p.ids > max_position - cursor.position
      then invalid ();
      if Option.fold ~none:false ~some:(( <> ) p.query_state) cursor.query_state
      then Restart p.receipts
      else
        let position = cursor.position + List.length p.ids in
        Option.iter
          (fun total ->
            if total < position || (p.ids = [] && position < total) then
              invalid ())
          p.total;
        let items, missing, receipts = fetch_ids source p.ids in
        let complete = p.ids = [] || p.total = Some position in
        update
          {
            cursor with
            phase = (if complete then Catching_up else Listing);
            position;
            query_state = Some p.query_state;
          }
          items missing (p.receipts @ receipts)
  | Catching_up | Live -> (
      let since = match cursor.state with Some s -> s | _ -> invalid () in
      match source.changes ~since with
      | Error receipts -> Restart receipts
      | Ok c ->
          check_ids c.created;
          check_ids c.updated;
          check_ids c.destroyed;
          if c.old_state <> since || (c.more && c.new_state = since) then
            invalid ();
          let ids =
            unique (c.created @ c.updated)
            |> List.filter (fun id -> not (List.mem id c.destroyed))
          in
          let items, missing, receipts = fetch_ids source ids in
          let publish = cursor.phase = Catching_up && not c.more in
          update ~publish
            ~more:(c.more || (cursor.phase = Catching_up && not publish))
            {
              cursor with
              state = Some c.new_state;
              phase = (if publish then Live else cursor.phase);
            }
            items
            (unique (missing @ c.destroyed))
            (c.receipts @ receipts))
