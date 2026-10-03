(** Eio driver for {!Console.Display}. *)

val run :
  sw:Eio.Switch.t ->
  ctx:Console.Display.ctx ->
  ?mode:Console.Display.mode ->
  ?theme:Console.Theme.t ->
  ?palette:Console.Color.t list ->
  ?bar:Console.Display.Line.t ->
  ?width:int ->
  ?height:int ->
  ?header:string ->
  (Console.Display.t -> 'a) ->
  'a
(** [run ~sw ~ctx f] is the only Eio-specific display operation. It scopes
    terminal ownership, drives refreshes by waiting on [ctx]'s own clock
    ({!Console.Display.wait}), and routes {!Logs} messages into permanent
    session history. The callback receives the ordinary {!Console.Display.t};
    all task and semantic operations remain in {!Console.Display}.

    @raise Invalid_argument if [ctx] was built without [~wait]. *)
