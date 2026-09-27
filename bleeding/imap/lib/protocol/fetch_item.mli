@@ portable

(** Metadata FETCH data items.

    The items a metadata FETCH can request. Body items are absent, since
    bodies stream through [Imap_eio.Selected.fetch_to] and
    [Imap_eio.Selected.Binary.fetch_binary_to]. *)

type t =
  | Uid
  | Flags
  | Internal_date
  | Rfc822_size
  | Envelope
  | Bodystructure
  | Modseq  (** RFC 7162. *)
  | Emailid  (** RFC 8474. *)
  | Threadid  (** RFC 8474. *)
  | Objectid  (** The draft OBJECTID+ compound identity. *)
  | Preview of { lazy_ : bool }  (** RFC 8970, with LAZY when [lazy_]. *)
  | Binary_size of int list
      (** The RFC 3516 decoded size of the MIME part at the section
          path. *)
(** The type for metadata FETCH items. *)

val to_wire : t -> string
(** [to_wire i] is the item name of [i], such as [RFC822.SIZE],
    [PREVIEW (LAZY)] or [BINARY.SIZE[1.2]]. It does not check the section
    path, which {!Command.uid_fetch_items} does. *)

val capabilities : t -> Capability.t list
(** [capabilities i] is the extensions [i] needs. [Modseq] needs
    [Condstore], [Emailid] and [Threadid] need [Objectid], [Objectid]
    needs [Objectid_plus], [Preview] needs [Preview], and [Binary_size]
    needs [Binary]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same item. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf i] prints [to_wire i] on [ppf]. *)
