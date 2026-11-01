module Fake = struct
  type t = { mutable closed : bool; fail : unit -> unit }

  let send t ~on_event:_ _ = t.fail ()
  let stats _ = assert false
  let cancel _ = ()
  let close t = t.closed <- true
end

let () =
  Eio_main.run @@ fun env ->
  let root = Filename.temp_dir "agentkit-wake" "" in
  let fs = Eio.Stdenv.fs env in
  let rec remove path =
    if Eio.Path.is_directory path then begin
      List.iter
        (fun name -> remove Eio.Path.(path / name))
        (Eio.Path.read_dir path);
      Eio.Path.rmdir path
    end
    else Eio.Path.unlink path
  in
  Fun.protect
    ~finally:(fun () -> remove Eio.Path.(fs / root))
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let store = Store.open_ ~sw ~clock Eio.Path.(fs / root) in
      List.iter
        (fun fail_publish ->
          let agent = { Fake.closed = false; fail = (fun () -> raise Exit) } in
          let published = ref None and progress = ref (fun () -> (42, 42)) in
          let publish job =
            published := job;
            if fail_publish && Option.is_some job then raise Exit
          in
          (match
             Wake.run ~cancel_on_stop:false ~clock ~store
               ~create:(fun ~system:_ ->
                 Agentkit.Driver.session (module Fake) agent)
               ~config:Wake.default_config
               ~fault:(fun () -> None)
               ~stop:(fun () -> false)
               ~publish
               ~prefill:(fun f -> progress := f)
               ~task:"test" ~prompt:"go"
           with
          | exception Exit -> ()
          | _ -> assert false);
          assert agent.closed;
          assert (!published = None);
          assert (!progress () = (0, 0)))
        [ false; true ])
