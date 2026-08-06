(* Checks that the annotated eio surface is callable from portable code. *)
let _f : (unit -> unit) @ portable =
 fun () ->
  Eio.Switch.run @@ fun sw ->
  ignore sw;
  ignore Eio.Net.accept;
  ignore Eio.Net.listen;
  ignore Eio.Net.connect;
  ignore Eio.Flow.copy_string;
  ignore Eio.Flow.single_read;
  ignore Eio.Fiber.fork;
  ignore Eio.Promise.await;
  ignore Eio.Time.sleep
