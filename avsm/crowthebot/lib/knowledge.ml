type t = {
  dispatch :
    check:(unit -> unit) -> string -> string -> (string, string) result;
  allowed : actor:string -> room:string -> bool;
}
(** Operator-configured access to the shared Bushel and Sortal corpus. *)

let create ~tools ~allowed = { dispatch = Bushel_agent.invoke tools; allowed }
let available t ~actor ~room = t.allowed ~actor ~room

let invoke t ~actor ~room ~check name arguments =
  let check () =
    check ();
    if not (available t ~actor ~room) then
      failwith "Knowledge access denied for this actor and room"
  in
  if not (available t ~actor ~room) then
    Error "Knowledge access denied for this actor and room"
  else
    try
      check ();
      let result = t.dispatch ~check name arguments in
      check ();
      result
    with Invalid_argument e | Failure e -> Error e

let sources ~fs = Bushel_knowledge_store.sources ~fs ()

let authorized store ~actor ~room =
  (Store.person store actor).allowed
  && (List.mem room (Store.rooms store)
     || Store.direct_peer store room = Some actor)

let load ~fs ~clock ~fetch ~store ~max_bytes () =
  if not (Bushel_knowledge_store.available ~fs ()) then None
  else
    let dispatch ~check name arguments =
      Bushel_knowledge_store.with_store ~fs ~clock ~fetch ~check
        (fun shared ->
          let tools = Bushel_agent.of_store shared ~max_bytes in
          Bushel_agent.invoke tools ~check name arguments)
    in
    Some { dispatch; allowed = authorized store }
