(** IMAP command encoders.

    An encoder returns the syntax of one command without its tag or final
    CRLF. The connection adds a unique tag and owns continuations,
    literals and the IDLE handshake. Encoders check syntax only.
    Negotiating the capability a command needs belongs to the caller.

    Mailbox arguments are wire names, already encoded with
    {!Mailbox_name.encode}. An argument sent as an astring goes out as an
    atom when it is one and as a quoted string otherwise. A quoted string
    must be valid UTF-8 without control characters, since these encoders
    send no literal. A UID set argument is an RFC 9051 sequence set of
    canonical nonzero numbers and may use [*]. A criterion argument is
    SEARCH syntax that must be nonempty and free of control characters,
    and must not end in a literal marker such as [{5}] or [{5+}], which
    would make the server read the next command as literal data. The
    server checks the rest of its syntax. *)

(** {1 Errors} *)

type error = {
  command : string;  (** The IMAP command, such as [UID FETCH]. *)
  argument : string option;
      (** The label of the failing argument, such as [mailbox]. *)
  reason : string;  (** Why the encoder refused. *)
}
(** The type for encoding errors. *)

val to_string : error -> string
(** [to_string e] is [e] as one line, [COMMAND argument: reason]. *)

val pp : Format.formatter -> error -> unit
(** [pp ppf e] prints [to_string e] on [ppf]. *)

(** {1 Session} *)

val capability : string
(** [capability] is the CAPABILITY command. *)

val noop : string
(** [noop] is the NOOP command. *)

val logout : string
(** [logout] is the LOGOUT command. *)

val idle : string
(** [idle] is the RFC 2177 IDLE command. The server answers with a
    continuation before it sends events. *)

val done_idle : string
(** [done_idle] is the untagged DONE that ends IDLE. The tagged completion
    of the IDLE command follows it. *)

val get_jmap_access : string
(** [get_jmap_access] is the GETJMAPACCESS command. *)

val namespace : string
(** [namespace] is the NAMESPACE command. *)

val enable : Capability.t list -> (string, error) result
(** [enable caps] is the RFC 5161 ENABLE of [caps] in {!Capability.to_wire}
    spelling. The error covers an empty [caps] and a token that is not an
    atom. *)

val login : username:string -> password:string -> (string, error) result
(** [login ~username ~password] is LOGIN with [username] and [password]
    each sent as a quoted string. *)

(** {1 Mailboxes} *)

val list : reference:string -> pattern:string -> (string, error) result
(** [list ~reference ~pattern] is LIST of [pattern] under [reference],
    each sent as an astring. *)

val lsub : reference:string -> pattern:string -> (string, error) result
(** [lsub ~reference ~pattern] is LSUB of [pattern] under [reference],
    each sent as an astring. *)

val list_extended : reference:string -> patterns:string list ->
  ?selection:Mailbox_list.selection list ->
  ?returns:Mailbox_list.return list ->
  ?status:Status_item.t list -> unit -> (string, error) result
(** [list_extended ~reference ~patterns ~selection ~returns ~status ()] is
    the RFC 5258 extended LIST of [patterns] under [reference].
    [selection] and [returns] default to no options. [status], omitted by
    default, adds the RFC 5819 STATUS return option for its items. The
    error covers an empty [patterns] or [status], a repeated option and a
    [Recursive_match] without [Subscribed] or [Special_use] beside it. *)

val list_status : reference:string -> pattern:string ->
  items:Status_item.t list -> (string, error) result
(** [list_status ~reference ~pattern ~items] is
    [list_extended ~reference ~patterns:[pattern] ~status:items ()]. The
    server answers with a LIST row and a STATUS row for each mailbox, to be
    matched by mailbox name. *)

val status : mailbox:string -> items:Status_item.t list ->
  (string, error) result
(** [status ~mailbox ~items] is STATUS of [items] for [mailbox]. The error
    covers an empty [items]. *)

val create : mailbox:string -> (string, error) result
(** [create ~mailbox] is CREATE of [mailbox]. *)

val delete : mailbox:string -> (string, error) result
(** [delete ~mailbox] is DELETE of [mailbox]. *)

val rename : old_name:string -> new_name:string -> (string, error) result
(** [rename ~old_name ~new_name] is RENAME of [old_name] to [new_name]. *)

val subscribe : mailbox:string -> (string, error) result
(** [subscribe ~mailbox] is SUBSCRIBE of [mailbox]. *)

val unsubscribe : mailbox:string -> (string, error) result
(** [unsubscribe ~mailbox] is UNSUBSCRIBE of [mailbox]. *)

val select : ?readonly:bool -> ?condstore:bool -> ?qresync:(int64 * int64) ->
  ?known_uids:string -> ?sequence_match:(string * string) ->
  ?objectid:(string * string) ->
  string -> (string, error) result
(** [select ~readonly ~condstore ~qresync ~known_uids ~sequence_match
    ~objectid mailbox] is SELECT of [mailbox], or EXAMINE when [readonly]
    is [true]. [readonly] defaults to [false]. [condstore] defaults to
    [false] and adds the CONDSTORE parameter, also beside QRESYNC.
    [qresync], omitted by default, is the RFC 7162 checkpoint
    [(uidvalidity, modseq)], which requires QRESYNC to be enabled.
    [known_uids] and [sequence_match], omitted by default, extend it and
    may not use [*]. [sequence_match] is a pair of sets of equal size and
    requires [known_uids]. [objectid], omitted by default, is the draft
    OBJECTID+ identity [(account_id, mailbox_id)] the server must match,
    which requires OBJECTID+ to be enabled. The error covers a checkpoint
    outside the UIDVALIDITY or MODSEQ range, a UID parameter without
    [qresync], a malformed or unequal pair of sets, and an identifier that
    is not an RFC 8474 [objectid]. *)

(** {1 Access control} *)

val getacl : mailbox:string -> (string, error) result
(** [getacl ~mailbox] is the RFC 4314 GETACL of [mailbox]. *)

val myrights : mailbox:string -> (string, error) result
(** [myrights ~mailbox] is MYRIGHTS of [mailbox]. *)

val listrights : mailbox:string -> identifier:string -> (string, error) result
(** [listrights ~mailbox ~identifier] is LISTRIGHTS of [identifier] on
    [mailbox]. [identifier] must be nonempty and is sent as an astring
    without SASLprep. *)

val setacl : mailbox:string -> identifier:string ->
  operation:[ `Add | `Remove | `Replace ] -> rights:string ->
  (string, error) result
(** [setacl ~mailbox ~identifier ~operation ~rights] is SETACL of [rights]
    for [identifier] on [mailbox], added, removed or replacing the current
    rights by [operation]. [identifier] is as for {!listrights}. [rights]
    holds lowercase ASCII letters and digits, including extension rights
    this module does not know, and must be nonempty for [`Add] and
    [`Remove]. *)

val deleteacl : mailbox:string -> identifier:string -> (string, error) result
(** [deleteacl ~mailbox ~identifier] is DELETEACL of [identifier] on
    [mailbox]. [identifier] is as for {!listrights}. *)

(** {1 Quota} *)

val getquota : root:string -> (string, error) result
(** [getquota ~root] is the RFC 9208 GETQUOTA of [root]. *)

val getquotaroot : mailbox:string -> (string, error) result
(** [getquotaroot ~mailbox] is GETQUOTAROOT of [mailbox]. *)

val setquota : root:string -> limits:(string * int64) list ->
  (string, error) result
(** [setquota ~root ~limits] is SETQUOTA of [limits] on [root]. It
    replaces every limit of [root], so a resource omitted from [limits]
    loses its limit. Resource names are atoms, sent in uppercase and
    unique ignoring case, and limits are non-negative. *)

(** {1 Metadata} *)

val getmetadata : mailbox:string -> entries:string list ->
  ?maxsize:int64 -> ?depth:Metadata.depth -> unit -> (string, error) result
(** [getmetadata ~mailbox ~entries ~maxsize ~depth ()] is the RFC 5464
    GETMETADATA of [entries] on [mailbox]. [entries] must be nonempty, and
    each follows RFC 5464 §3.2. It starts with [/], does not end with [/],
    and holds no [//], [*], [%], control character or non-ASCII byte.
    [maxsize], from 0 to 4294967295, and [depth] are omitted by
    default. *)

val setmetadata : mailbox:string -> values:(string * string option) list ->
  (string, error) result
(** [setmetadata ~mailbox ~values] is SETMETADATA of [values] on
    [mailbox], with [None] sent as NIL. [values] must be nonempty, entry
    names follow the rules of {!getmetadata} and are unique ignoring case,
    and each value is sent as a quoted string. A value that needs a
    literal, such as one holding CR or LF, is an error. *)

(** {1 Notification} *)

val notify_none : string
(** [notify_none] is the RFC 5465 NOTIFY NONE command. *)

val notify_set : ?status:bool -> groups:Notify.group list -> unit ->
  (string, error) result
(** [notify_set ~status ~groups ()] is NOTIFY SET of [groups], with the
    STATUS option when [status] is [true]. [status] defaults to [false].
    [groups] must be nonempty and hold at most one group whose filter
    satisfies {!Notify.is_selected}. In each group MessageNew and
    MessageExpunge appear together, FlagChange needs them, no event
    repeats, and a selected filter takes only message events and
    AnnotationChange. MessageNew is sent without FETCH attributes. After a
    NOTIFICATIONOVERFLOW code the server behaves as after NOTIFY NONE. *)

(** {1 Fetching} *)

val uid_fetch : set:string -> items:string list -> (string, error) result
(** [uid_fetch ~set ~items] is UID FETCH of [items] for [set]. Each item is
    one of UID, FLAGS, MODSEQ, RFC822.SIZE, INTERNALDATE, ENVELOPE,
    BODYSTRUCTURE, EMAILID, THREADID, OBJECTID, BODY[], BODY.PEEK[],
    BODY[HEADER], BODY.PEEK[HEADER], BODY[TEXT], BODY.PEEK[TEXT],
    BINARY[], BINARY.PEEK[] and BINARY.SIZE[], in any case, and [items]
    must be nonempty. *)

val uid_fetch_mod : ?changedsince:int64 -> ?vanished:bool ->
  ?partial:(int64 * int64) ->
  set:string -> items:string list -> unit -> (string, error) result
(** [uid_fetch_mod ~changedsince ~vanished ~partial ~set ~items ()] is
    {!uid_fetch} with FETCH modifiers. [changedsince], omitted by default,
    is the RFC 7162 CHANGEDSINCE value and may be 0. [vanished] defaults to
    [false] and requires [changedsince]. [partial], omitted by default, is
    the RFC 9394 range [(first, last)], two nonzero positions of the same
    sign with magnitudes up to 4294967295. *)

val uid_fetch_binary : set:string -> section:int list ->
  ?partial:(int64 * int64) -> unit -> (string, error) result
(** [uid_fetch_binary ~set ~section ~partial ()] is UID FETCH of UID and the
    RFC 3516 BINARY.PEEK of [section] for [set], which leaves [\Seen]
    unchanged. [section] holds at most 100 parts, each from 1 to
    4294967295, and an empty [section] encodes as [[]]. IMAP4rev2 allows
    BINARY only on a leaf body part. [partial], omitted by default,
    is [(offset, count)] in decoded octets with a non-negative [offset]
    and a positive [count]. A decoded section cannot replace an archival
    BODY[]. *)

val uid_fetch_binary_size : set:string -> section:int list ->
  (string, error) result
(** [uid_fetch_binary_size ~set ~section] is UID FETCH of UID and the
    BINARY.SIZE of [section] for [set]. [section] is as for
    {!uid_fetch_binary}. *)

val uid_fetch_preview : set:string -> lazy_:bool -> (string, error) result
(** [uid_fetch_preview ~set ~lazy_] is UID FETCH of UID and the RFC 8970
    PREVIEW for [set], with the LAZY modifier when [lazy_] is [true]. *)

val uid_fetch_items : ?partial:(int64 * int64) -> set:string ->
  items:Fetch_item.t list -> unit -> (string, error) result
(** [uid_fetch_items ~partial ~set ~items ()] is UID FETCH of [items] in
    order for [set]. [partial], omitted by default, adds the PARTIAL
    modifier as for {!uid_fetch_mod}. The error covers an empty [items] and
    a [Binary_size] section outside the rules of {!uid_fetch_binary}. *)

(** {1 Searching, sorting and threading} *)

val uid_search : criterion:string -> (string, error) result
(** [uid_search ~criterion] is UID SEARCH of [criterion]. *)

val uid_search_partial : range:(int64 * int64) -> criterion:string ->
  (string, error) result
(** [uid_search_partial ~range ~criterion] is UID SEARCH of [criterion]
    returning the RFC 9394 PARTIAL [range], as for {!uid_fetch_mod}.
    Result positions shift as the mailbox changes, so a page is not proof
    of a complete inventory. *)

val uid_sort : keys:(Sort.key * Sort.order) list -> charset:string ->
  criterion:string -> (string, error) result
(** [uid_sort ~keys ~charset ~criterion] is the RFC 5256 UID SORT of the
    messages matching [criterion] by [keys], in priority order, each with
    its own direction. [keys] holds 1 to 100 keys. [charset] is a nonempty
    ASCII astring. [criterion] may use sequence sets although the results
    are UIDs. *)

val uid_sort_extended : returns:Sort.return list ->
  keys:(Sort.key * Sort.order) list -> charset:string -> criterion:string ->
  (string, error) result
(** [uid_sort_extended ~returns ~keys ~charset ~criterion] is {!uid_sort}
    with the RFC 5267 ESORT return options [returns]. An empty [returns]
    means ALL. The error covers a repeated option, ALL together with
    PARTIAL, and a PARTIAL position outside 1 to 4294967295. Reversed
    PARTIAL bounds are allowed. PARTIAL needs CONTEXT=SORT. *)

val uid_thread : algorithm:Thread.algorithm -> charset:string ->
  criterion:string -> (string, error) result
(** [uid_thread ~algorithm ~charset ~criterion] is the RFC 5256 UID THREAD
    of the messages matching [criterion] by [algorithm]. The name of an
    [Other] algorithm must be an atom. [charset] and [criterion] are as for
    {!uid_sort}. *)

val uid_batches : ?range:(int64 * int64) -> size:int64 -> unit ->
  (string, error) result
(** [uid_batches ~range ~size ()] is the RFC 10022 UIDBATCHES request for
    batches of [size] messages. [size] runs from 500 to 4294967295.
    [range], omitted by default, is an ascending range of batch indexes
    [(first, last)] from 1 to 4294967295 whose batches hold at most
    100,000 messages in total. The returned ranges are boundaries, not
    proof that every UID within them exists. The caller must keep to the
    RFC's limits on how often UIDBATCHES is reissued. *)

(** {1 Storing, copying and expunging} *)

val uid_store : set:string -> operation:[ `Add | `Remove | `Replace ] ->
  silent:bool -> flags:string list -> (string, error) result
(** [uid_store ~set ~operation ~silent ~flags] is UID STORE of [flags] on
    [set], added, removed or replacing the current flags by [operation],
    with FLAGS.SILENT when [silent] is [true]. Each flag must be a valid
    IMAP flag. *)

val uid_store_mod : ?unchangedsince:int64 -> set:string ->
  operation:[ `Add | `Remove | `Replace ] ->
  silent:bool -> flags:string list -> unit -> (string, error) result
(** [uid_store_mod ~unchangedsince ~set ~operation ~silent ~flags ()] is
    {!uid_store} with the RFC 7162 UNCHANGEDSINCE modifier. [unchangedsince]
    is omitted by default and must be non-negative. *)

val uid_copy : set:string -> mailbox:string -> (string, error) result
(** [uid_copy ~set ~mailbox] is UID COPY of [set] to [mailbox]. *)

val uid_move : set:string -> mailbox:string -> (string, error) result
(** [uid_move ~set ~mailbox] is the RFC 6851 UID MOVE of [set] to
    [mailbox]. *)

val uid_expunge : set:string -> (string, error) result
(** [uid_expunge ~set] is the RFC 4315 UID EXPUNGE of [set]. *)

(** {1 Saved results}

    These encoders act on the RFC 5182 saved result [$]. Every other
    encoder rejects [$] as a UID set. The saved result belongs to the
    selected connection and changes with SAVE, selection and expunges.
    These encoders do not track it. *)

val uid_search_save : criterion:string -> (string, error) result
(** [uid_search_save ~criterion] is UID SEARCH of [criterion] with RETURN
    (SAVE COUNT), which saves the whole matching set without listing it.
    Use the saved result only after the tagged OK and its COUNT. *)

val uid_search_saved : criterion:string -> (string, error) result
(** [uid_search_saved ~criterion] is UID SEARCH of UID [$] and the
    parenthesised [criterion] with RETURN (ALL COUNT). It leaves the saved
    result unchanged. [criterion] must balance its parentheses and quotes,
    with at most 100 levels of nesting, and a backslash inside quotes may
    escape only a backslash or a quote. *)

val uid_fetch_saved : ?changedsince:int64 -> ?vanished:bool ->
  ?partial:(int64 * int64) -> items:string list -> unit ->
  (string, error) result
(** [uid_fetch_saved ~changedsince ~vanished ~partial ~items ()] is
    {!uid_fetch_mod} on [$]. *)

val uid_fetch_saved_items : ?partial:(int64 * int64) ->
  items:Fetch_item.t list -> unit -> (string, error) result
(** [uid_fetch_saved_items ~partial ~items ()] is {!uid_fetch_items} on
    [$]. *)

val uid_store_saved : ?unchangedsince:int64 ->
  operation:[ `Add | `Remove | `Replace ] -> silent:bool ->
  flags:string list -> unit -> (string, error) result
(** [uid_store_saved ~unchangedsince ~operation ~silent ~flags ()] is
    {!uid_store_mod} on [$]. *)

val uid_copy_saved : mailbox:string -> (string, error) result
(** [uid_copy_saved ~mailbox] is {!uid_copy} of [$]. *)

val uid_move_saved : mailbox:string -> (string, error) result
(** [uid_move_saved ~mailbox] is {!uid_move} of [$]. *)

val uid_expunge_saved : string
(** [uid_expunge_saved] is UID EXPUNGE of [$]. *)

(** {1 Appending}

    An APPEND prefix ends in a literal marker and CRLF. With a
    synchronizing marker the caller waits for the continuation, then sends
    exactly [size] bytes and CRLF. A non-synchronizing marker skips the
    wait. The caller must negotiate it and enforce its size cap. *)

val append_prefix : mailbox:string -> ?non_sync:bool -> ?flags:string list ->
  ?internal_date:Internal_date.t -> size:int64 ->
  unit -> (string, error) result
(** [append_prefix ~mailbox ~non_sync ~flags ~internal_date ~size ()] is
    APPEND to [mailbox] up to the literal marker [{size}], or [{size+}]
    when [non_sync] is [true]. [non_sync] defaults to [false]. [flags]
    defaults to none and each must be a valid IMAP flag. [internal_date]
    is omitted by default. [size] must be non-negative. *)

val append_part_prefix : ?non_sync:bool -> ?flags:string list ->
  ?internal_date:Internal_date.t -> size:int64 -> unit ->
  (string, error) result
(** [append_part_prefix ~non_sync ~flags ~internal_date ~size ()] is the
    next message of a MULTIAPPEND after the first, starting with a space
    and ending in its literal marker. The arguments are as for
    {!append_prefix}. *)

val append_binary_prefix : mailbox:string -> ?non_sync:bool ->
  ?flags:string list -> ?internal_date:Internal_date.t -> size:int64 ->
  unit -> (string, error) result
(** [append_binary_prefix ~mailbox ~non_sync ~flags ~internal_date ~size ()]
    is {!append_prefix} with the RFC 3516 [literal8] marker [~{size}], or
    [~{size+}] when [non_sync] is [true]. It needs the BINARY capability,
    which IMAP4rev2 alone does not provide for APPEND. The server may
    change the content transfer encoding without losing data. *)
