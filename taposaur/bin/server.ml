open Cmdliner
let serve root db port =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let archive = Taposaur_archive.open_root ~sw Eio.Path.(env#fs / root) in
  let catalog = Taposaur_catalog.open_read ~sw
    Eio.Path.(env#fs / db) in
  let state = Taposaur_web.create ~archive ~catalog in
  Proffer_httpz.run ~sw ~port env ~env:state Taposaur_web.site

let term = Term.(const serve $
  Arg.(value & opt string "/data/papers/taposaur" & info ["root"]
    ~doc:"Archive root. The browser reads this tree without modifying it.") $
  Arg.(value & opt string "taposaur.db" & info ["db"]
    ~doc:"Existing local catalogue, opened read-only.") $
  Arg.(value & opt int 8765 & info ["port"] ~doc:"Loopback HTTP port."))
let () = exit (Cmd.eval (Cmd.v
  (Cmd.info "taposaur-web" ~doc:"Browse the paper archive with Proffer.") term))
