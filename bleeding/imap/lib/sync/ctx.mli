(** The connection, store and mailbox of an online sync call.

    Every online entry point of [Imap_sync] takes a context in place of
    these arguments. A Maildir handle or writer stays a separate argument,
    and an offline call takes a store and a scope instead. A context is
    valid for the client's mailbox name encoding at construction, so build
    it after any ENABLE that changes that encoding, such as UTF8=ACCEPT. *)

type t = private {
  client : Imap_eio.Client.t;
      (** [client] is the authenticated connection. *)
  store : Imap_store.t;
      (** [store] holds the published inventory, the pairs and the
          journal. *)
  scope : Imap.Mirror.scope;
      (** [scope] is the published scope of [mailbox]. *)
  mailbox : string;
      (** [mailbox] is the UTF-8 mailbox name the client selects. *)
  spool_dir : Eio.Fs.dir_ty Eio.Path.t;
      (** [spool_dir] holds the provisional files of body transfers and
          staged Maildir inventories. *)
  next_id : unit -> string;
      (** [next_id ()] is a fresh, globally unique, filesystem-safe
          identifier for a journal operation or spool file. *)
}
(** The type for sync contexts. *)

val v :
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  scope:Imap.Mirror.scope -> mailbox:string ->
  spool_dir:[> Eio.Fs.dir_ty ] Eio.Path.t -> next_id:(unit -> string) ->
  (t, Error.t) result
(** [v ~client ~store ~scope ~mailbox ~spool_dir ~next_id] is the context of
    [mailbox] on [client] with [store], [scope], [spool_dir] and [next_id].
    It is [Error (Invalid_scope _)] unless [scope.encoding] is [client]'s
    current mailbox name encoding and [mailbox] encodes to [scope.raw_name]
    under it. It does no I/O. *)
