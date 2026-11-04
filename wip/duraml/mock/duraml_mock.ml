module D = Duraml
type handler = {call : 'a. 'a D.Pi.request -> 'a}
type observer = {observe : 'a. 'a D.Pi.request -> unit}
type t = {handler : handler; await : D.waiting -> unit}
module Backend = struct
  type nonrec t = t
  let call t request = t.handler.call request
  let await t waiting = t.await waiting
end
let make ~await handler =
  Eio.Resource.T ({handler;await},D.Pi.handler (module Backend))
let wrap ?before ?after t =
  let call : type a. a D.Pi.request -> a = fun request ->
    Option.iter (fun o -> o.observe request) before;
    let result=D.Pi.call t request in
    Option.iter (fun o -> o.observe request) after;
    result in
  make ~await:(D.Pi.await t) {call}
