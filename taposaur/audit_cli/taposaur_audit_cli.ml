module A = Taposaur_audit
let setting name fallback = match Sys.getenv_opt name with
  | None | Some "" -> fallback | Some value -> value
let default_path ()  = setting "TAPOSAUR_AUDIT_DB" "taposaur-audit.db"
let open_session ~sw ~fs ~component =
  let db = A.open_write ~sw Eio.Path.(fs / default_path ()) in
  A.session ~actor:(setting "TAPOSAUR_ACTOR" "system") ~component db
let execute f = try
  Eio_main.run (fun env -> Eio.Switch.run (fun sw -> f env sw)); Ok ()
  with ex -> Error (Printexc.to_string ex)
open Cmdliner
let path = Arg.(value & opt string (default_path ()) & info ["audit-db"]
  ~doc:"Audit SQLite database. Also configurable with TAPOSAUR_AUDIT_DB.")
let filter name = Arg.(value & opt (some string) None & info [name])
let command = Cmd.group (Cmd.info "audit" ~doc:"Query the append-only audit log.") [
  Cmd.v (Cmd.info "init" ~doc:"Create or validate an audit database.")
    Term.(const (fun path -> execute (fun env sw ->
      let store = A.open_write ~sw Eio.Path.(env#fs / path) in
      let session = A.session ~actor:(setting "TAPOSAUR_ACTOR" "system")
        ~component:"taposaur.audit" store in
      A.record session ~action:"audit.initialize" ~subject:path
        ~details:["schema_version","1"] ())) $ path);
  Cmd.v (Cmd.info "query" ~doc:"Print filtered audit events as JSONL, newest first.")
    Term.(const (fun path actor action subject run operation phase before limit incomplete ->
      execute (fun env sw ->
        let db = A.open_read ~sw Eio.Path.(env#fs / path) in
        let phase = Option.map A.phase_of_string phase in
        let before = Option.map Int64.of_string before in
        A.query ?actor ?action ?subject ?run ?operation ?phase ?before ~limit ~incomplete db
        |> List.iter (fun event -> print_endline (A.to_json event)))) $
      path $ filter "actor" $ filter "action" $ filter "subject" $
      filter "run" $ filter "operation" $ filter "phase" $ filter "before" $
      Arg.(value & opt int 100 & info ["limit"]) $
      Arg.(value & flag & info ["incomplete"]))]
