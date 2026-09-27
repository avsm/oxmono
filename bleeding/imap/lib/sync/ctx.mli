(** The connection, store and mailbox an online sync call works on.

    Every online entry point of [Imap_sync] takes a context instead of
    repeating these arguments. A context is valid for the client's mailbox
    name encoding at construction, so build it after any ENABLE that changes
    that encoding, such as UTF8=ACCEPT. *)

type t = private {
  client : Imap_eio.Client.t;
  store : Imap_store.t;
  scope : Imap.Mirror.scope;
  mailbox : string;
      (** [mailbox] is the UTF-8 mailbox name the client selects. *)
  spool_dir : Eio.Fs.dir_ty Eio.Path.t;
      (** [spool_dir] holds the provisional files of body transfers and
          staged Maildir inventories. *)
  next_id : unit -> string;
      (** [next_id ()] is a fresh, globally unique, filesystem-safe
          identifier for a journal operation or spool file. *)
}

val v :
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  scope:Imap.Mirror.scope -> mailbox:string ->
  spool_dir:[> Eio.Fs.dir_ty ] Eio.Path.t -> next_id:(unit -> string) ->
  (t, Error.t) result
(** [v ~client ~store ~scope ~mailbox ~spool_dir ~next_id] is a context when
    [mailbox] encodes to [scope.raw_name] under [client]'s current mailbox
    name encoding and [scope.encoding] is that encoding, and
    [Error (Invalid_scope _)] otherwise. It does no I/O. *)
