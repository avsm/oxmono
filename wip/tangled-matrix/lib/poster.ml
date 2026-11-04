(* SPDX-License-Identifier: ISC *)
module Api = Tangled.Api
module W = Tangled.Watch

(* A message waiting to be sent or edited. *)
type pending = {
  entry : Messages.entry;
  mutable render : Render.t;
  mutable finished : bool;
  mutable last : float;  (** When the room was last sent to, or 0. *)
  mutable held : bool;  (** Whether a not-ready room was already logged. *)
}

let run ~sw ~clock ~api ~bot ~messages ~accounts ?(rooms = []) ?(interval = 15.)
    ?(edit_interval = 10.) ?(bindings_interval = 60.) ?appview () =
  let clock = (clock :> float Eio.Time.clock_ty Eio.Resource.t) in
  let bindings = ref [] in
  let refresh () =
    let everything =
      List.filter_map
        (fun spec ->
          match Bot.find_room bot spec with
          | Some id -> Some id
          | None ->
              Logs.warn (fun m -> m "No room %s yet" spec);
              None)
        rooms
    in
    bindings :=
      Bindings.read api ~accounts ~bot:(Bot.user_id bot) ~everything ();
    Logs.info (fun m -> m "Reporting %d repositories" (List.length !bindings))
  in
  refresh ();
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Time.sleep clock bindings_interval;
        try refresh () with
        | Eio.Cancel.Cancelled _ as e -> raise e
        | e ->
            Logs.warn (fun m ->
                m "Could not read the bindings: %s" (Printexc.to_string e))
      done;
      `Stop_daemon);
  let handles = Hashtbl.create 8 in
  let name (r : Api.repository) =
    let handle =
      match Hashtbl.find_opt handles r.owner with
      | Some h -> h
      | None ->
          let h = Api.handle_of_did api r.owner in
          Hashtbl.replace handles r.owner h;
          h
    in
    Option.value ~default:r.owner handle ^ "/" ^ r.name
  in
  let pending = Hashtbl.create 16 in
  let rooms (p : W.pipeline) =
    List.concat_map
      (fun (b : Bindings.t) -> if b.repo.did = p.repo.did then b.rooms else [])
      !bindings
  in
  let update (p : W.pipeline) =
    let render = Render.pipeline ?appview ~name:(name p.repo) p in
    List.iter
      (fun room ->
        let key = (p.repo.did, p.id, room) in
        match Hashtbl.find_opt pending key with
        | Some x ->
            x.render <- render;
            x.finished <- W.finished p
        | None ->
            let entry =
              match
                Messages.find messages ~repo:p.repo.did ~pipeline:p.id ~room
              with
              | Some e -> e
              | None ->
                  let e =
                    {
                      Messages.repo = p.repo.did;
                      pipeline = p.id;
                      room;
                      event = None;
                      finished = false;
                      shown = "";
                    }
                  in
                  Messages.add messages e;
                  e
            in
            Hashtbl.replace pending key
              {
                entry;
                render;
                finished = W.finished p;
                last = 0.;
                held = false;
              })
      (rooms p)
  in
  (* One fiber sends, so a room's messages go in order and a slow homeserver
     holds back only the poster. Sending yields to the watcher, which may
     change [pending] and renders meanwhile, so it works on a snapshot and
     records what it sent, not what is newest. *)
  let flush_one key x =
    let room = x.entry.room in
    let now = Eio.Time.now clock in
    let render = x.render and finished = x.finished in
    let done_ () = if finished then Hashtbl.remove pending key in
    if x.entry.shown = render.plain && x.entry.event <> None then (
      if finished && not x.entry.finished then (
        x.entry.finished <- true;
        Messages.save messages);
      done_ ())
    else if x.entry.event <> None && now -. x.last < edit_interval then ()
    else if not (Bot.ready bot ~room) then (
      if not x.held then
        Logs.warn (fun m ->
            m
              "Holding messages for %s until the bot has joined it and knows \
               its encryption"
              room);
      x.held <- true)
    else
      match
        (match x.entry.event with
        | None -> x.entry.event <- Some (Bot.send bot ~room render)
        | Some event -> Bot.edit bot ~room ~event render);
        x.last <- Eio.Time.now clock;
        x.entry.shown <- render.plain;
        x.entry.finished <- finished;
        Messages.save messages
      with
      | () -> if x.render == render then done_ ()
      | exception (Eio.Cancel.Cancelled _ as e) -> raise e
      | exception e ->
          Logs.warn (fun m ->
              m "Could not post to %s: %s" room (Printexc.to_string e));
          x.last <- Eio.Time.now clock
  in
  let flush () =
    Hashtbl.fold (fun k x acc -> (k, x) :: acc) pending []
    |> List.iter (fun (k, x) -> flush_one k x)
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Time.sleep clock 2.;
        flush ()
      done;
      `Stop_daemon);
  let watcher = W.v ~interval ~clock api in
  W.run watcher
    ~repos:(fun () -> List.map (fun (b : Bindings.t) -> b.repo) !bindings)
    ~resume:(Messages.unfinished messages)
    ~on_error:(fun r e ->
      Logs.warn (fun m ->
          m "Could not poll %s: %s" (name r) (Printexc.to_string e)))
    (function W.Started p | W.Changed (p, _) | W.Finished p -> update p)
