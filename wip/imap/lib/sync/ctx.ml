type t = {
  client : Imap_eio.Client.t;
  store : Imap_store.t;
  scope : Imap.Mirror.scope;
  mailbox : string;
  spool_dir : Eio.Fs.dir_ty Eio.Path.t;
  next_id : unit -> string;
}

let v ~client ~store ~(scope:Imap.Mirror.scope) ~mailbox ~spool_dir
    ~next_id =
  let mode=Imap_eio.Client.mailbox_mode client in
  if scope.encoding<>mode then
    Error (Error.Invalid_scope "mailbox encoding changed")
  else match Imap.Mailbox_name.encode ~mode mailbox with
    | Error why -> Error (Error.Invalid_scope why)
    | Ok wire when wire<>scope.raw_name -> Error (Error.Invalid_scope
        "mailbox wire name differs from cursor scope")
    | Ok _ ->
        let spool_dir=(spool_dir :> Eio.Fs.dir_ty Eio.Path.t) in
        Ok {client;store;scope;mailbox;spool_dir;next_id}
