type traceln = {
  traceln : 'a. ?__POS__:string * int * int * int -> ('a, Format.formatter, unit, unit) format4 -> 'a;
} [@@unboxed]

let traceln_key : traceln Fiber.key = Fiber.create_key ()

let traceln_mutex = Mutex.create ()

let default_traceln ?__POS__:pos fmt =
  let k go =
    Trace.with_span "traceln" @@ fun () ->
    let b = Buffer.create 512 in
    let f = Format.formatter_of_buffer b in
    go f;
    Option.iter (fun (file, lnum, _, _) -> Format.fprintf f " [%s:%d]" file lnum) pos;
    Format.pp_close_box f ();
    Format.pp_print_flush f ();
    let msg = Buffer.contents b in
    Trace.log msg;
    let lines = String.split_on_char '\n' msg in
    Mutex.lock traceln_mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock traceln_mutex) @@ fun () ->
    List.iter (Printf.eprintf "+%s\n") lines;
    flush stderr
  in
  Format.kdprintf k ("@[" ^^ fmt)

let get () =
  match Fiber.get traceln_key with
  | Some traceln -> traceln
  | None
  | exception (Effect.Unhandled _) -> { traceln = default_traceln }

type with_prefix_fn =
  { wp : 'a. (Format.formatter -> unit) -> (unit -> 'a) -> 'a }
[@@unboxed]

(* See [traceln_p]. *)
let with_trace_prefix_p =
  Obj.magic_portable
    { wp = (fun prefix fn ->
        let { traceln } = get () in
        let traceln ?__POS__ fmt =
          traceln ?__POS__ ("%t" ^^ fmt) prefix
        in
        Fiber.with_binding traceln_key { traceln } fn) }

let with_trace_prefix prefix fn = with_trace_prefix_p.wp prefix fn

(* Formats with Fmt and reads the fiber-local override, neither of which
   the checker accepts in a portable function. Tracing writes only to
   stderr under a mutex, so the assertion is sound. *)
let traceln_p =
  Obj.magic_portable
    { traceln = (fun ?__POS__ fmt ->
        let { traceln } = get () in
        traceln ?__POS__ fmt) }

let traceln ?__POS__ fmt = traceln_p.traceln ?__POS__ fmt

type t = <
  traceln : traceln Fiber.key;
>

(* An object value cannot cross portability. It only exposes the
   (immutable) traceln key. *)
let v = Obj.magic_portable @@ object
  method traceln = traceln_key
end
