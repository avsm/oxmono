(** Validated command syntax, without tags or final CRLF. The Eio connection
    generates a unique tag and owns continuation handshakes.

    Mailbox arguments are wire names. Encode them with {!Mailbox_name.encode}
    first. Quoted arguments must be valid UTF-8 without control characters.
    UID set arguments are RFC 9051 sequence sets of canonical nonzero
    numbers and may use [*]. Every encoder that validates returns an
    {!error} naming the command and, where one is at fault, the labelled
    argument. *)

type error = {
  command : string;  (** The IMAP command, such as [UID FETCH]. *)
  argument : string option;
      (** The label of the failing argument, such as [mailbox]. *)
  reason : string;  (** Why the encoder refused. *)
}

val to_string : error -> string
(** [to_string e] is [e] as one line, [COMMAND argument: reason]. *)

val pp : Format.formatter -> error -> unit
(** [pp] prints {!to_string}. *)

val capability : string
val noop : string
val logout : string
val idle : string
val done_idle : string
(** These are distinct protocol writes: after tagged IDLE, wait for the server
    continuation before expecting events, and send untagged DONE before
    awaiting the original tag's completion. The Eio session owns that state. *)
val get_jmap_access : string
val enable : Capability.t list -> (string, error) result
(** [enable caps] is RFC 5161 ENABLE for [caps] in {!Capability.to_wire}
    spelling. It is an error when [caps] is empty or a token is not an
    atom. *)
val getacl : mailbox:string -> (string, error) result
val myrights : mailbox:string -> (string, error) result
val listrights : mailbox:string -> identifier:string -> (string, error) result
val setacl : mailbox:string -> identifier:string ->
  operation:[ `Add | `Remove | `Replace ] -> rights:string ->
  (string, error) result
val deleteacl : mailbox:string -> identifier:string -> (string, error) result
(** ACL identifiers are encoded as IMAP astrings but SASLprep is not performed
    locally. Rights are lowercase ASCII letters/digits, including unknown
    extension rights supplied by the caller. [`Add] and [`Remove] need at
    least one right. *)
val getquota : root:string -> (string, error) result
val getquotaroot : mailbox:string -> (string, error) result
val setquota : root:string -> limits:(string * int64) list ->
  (string, error) result
(** RFC 9208 SETQUOTA replaces the complete limit list for the root, including
    removing limits omitted from [limits]. This API represents nonnegative
    values through signed int64 only. Callers must read/confirm policy. *)
val getmetadata : mailbox:string -> entries:string list ->
  ?maxsize:int64 -> ?depth:Metadata.depth -> unit -> (string, error) result
(** Entry names follow RFC 5464 section 3.2. They start with [/], do not end
    with [/], and contain no [//], [*], [%], controls or non-ASCII bytes.
    [maxsize] is a 32-bit number. [maxsize] and [depth] are omitted by
    default. *)
val setmetadata : mailbox:string -> values:(string * string option) list ->
  (string, error) result
(** This encoder supports NIL and quoted values only. Values requiring
    literals, including CR/LF, need a session-level streaming path. Entry
    names compare case-insensitively when checking for duplicates. *)
val notify_none : string
val notify_set : ?status:bool -> groups:Notify.group list -> unit ->
  (string, error) result
(** Known RFC 5465 event names only, without MessageNew FETCH attributes.
    Call only when NOTIFY is advertised. NOTIFICATIONOVERFLOW cancels the watch. *)
val login : username:string -> password:string -> (string, error) result
val namespace : string
val list : reference:string -> pattern:string -> (string, error) result
val lsub : reference:string -> pattern:string -> (string, error) result
val list_extended : reference:string -> patterns:string list ->
  ?selection:Mailbox_list.selection list ->
  ?returns:Mailbox_list.return list ->
  ?status:Status_item.t list -> unit -> (string, error) result
(** RFC 5258 selection/return options. [Recursive_match] requires
    [Subscribed] or [Special_use]. [status] adds RFC 5819 LIST-STATUS.
    Use this only after the relevant capability has been negotiated. *)
val status : mailbox:string -> items:Status_item.t list ->
  (string, error) result
val list_status : reference:string -> pattern:string ->
  items:Status_item.t list -> (string, error) result
(** RFC 5819 return option. Call only when LIST-STATUS is advertised, and
    correlate untagged LIST/STATUS rows by mailbox name. *)
val create : mailbox:string -> (string, error) result
val delete : mailbox:string -> (string, error) result
val rename : old_name:string -> new_name:string -> (string, error) result
val subscribe : mailbox:string -> (string, error) result
val unsubscribe : mailbox:string -> (string, error) result
val select : ?readonly:bool -> ?condstore:bool -> ?qresync:(int64 * int64) ->
  ?known_uids:string -> ?sequence_match:(string * string) ->
  ?objectid:(string * string) ->
  string -> (string, error) result
(** [condstore] defaults to [false] and adds the CONDSTORE parameter, also
    alongside [qresync]. QRESYNC requires the caller to have successfully
    ENABLEd QRESYNC. [known_uids] and [sequence_match] may not use [*].
    [objectid] is the draft OBJECTID+ [(account_id, mailbox_id)] identity;
    the client must enable OBJECTID+ and verify the selected identity. *)
val uid_fetch : set:string -> items:string list -> (string, error) result
val uid_fetch_binary : set:string -> section:int list ->
  ?partial:(int64 * int64) -> unit -> (string, error) result
val uid_fetch_binary_size : set:string -> section:int list -> (string, error) result
(** RFC 3516 decoded MIME sections. BINARY.PEEK leaves Seen unchanged. Empty
    sections encode [[]]; IMAP4rev2 permits these requests only for leaf body
    parts. Numeric paths have at most 100 positive
    uint32 components. A partial is [(offset,count)] in decoded octets, with a
    nonnegative signed-int64 offset and positive signed-int64 count, as required
    for IMAP4rev2 sizes. Capability negotiation belongs to
    the caller. Decoded sections are not replacements for archival BODY[]. *)
val uid_fetch_preview : set:string -> lazy_:bool -> (string, error) result
(** RFC 8970 PREVIEW, optionally with the LAZY modifier. *)
val uid_fetch_items : ?partial:(int64 * int64) -> set:string ->
  items:Fetch_item.t list -> unit -> (string, error) result
(** [uid_fetch_items ~set ~items ()] is [UID FETCH] of [items] in order,
    with the RFC 9394 PARTIAL modifier when [partial] is given. It is an
    error when [items] is empty or a [Binary_size] section path is
    invalid. [partial] is omitted by default. *)
val uid_fetch_saved_items : ?partial:(int64 * int64) ->
  items:Fetch_item.t list -> unit -> (string, error) result
(** [uid_fetch_saved_items ~items ()] is {!uid_fetch_items} on the saved
    result [$]. *)
val uid_fetch_mod : ?changedsince:int64 -> ?vanished:bool ->
  ?partial:(int64 * int64) ->
  set:string -> items:string list -> unit -> (string, error) result
val uid_search : criterion:string -> (string, error) result
(** [uid_search ~criterion] is [UID SEARCH criterion]. Every criterion
    argument of this module must be nonempty and free of control characters,
    and must not end in a literal marker such as [{5}] or [{5+}], which would
    make the server read the next command as literal data. *)
val uid_search_save : criterion:string -> (string, error) result
(** RFC 5182 SEARCHRES, returning SAVE COUNT so the complete matching set is
    saved without enumerating it. Requires SEARCHRES; COUNT must be correlated
    with tagged success before using the saved result. The variable belongs to
    the selected connection and changes with SAVE, selection and expunges. *)
val uid_search_saved : criterion:string -> (string, error) result
(** Refine the saved set with [UID $] and a grouped criterion, returning ALL
    and COUNT without SAVE. Parentheses and quoted strings must balance,
    with at most 100 nested groups. Full search-key syntax remains server
    validated. The saved result variable is preserved. *)
val uid_fetch_saved : ?changedsince:int64 -> ?vanished:bool ->
  ?partial:(int64 * int64) -> items:string list -> unit -> (string, error) result
val uid_store_saved : ?unchangedsince:int64 ->
  operation:[ `Add | `Remove | `Replace ] -> silent:bool -> flags:string list ->
  unit -> (string, error) result
val uid_copy_saved : mailbox:string -> (string, error) result
val uid_move_saved : mailbox:string -> (string, error) result
val uid_expunge_saved : string
(** Explicit saved-result encoders using [$]. Ordinary UID-set constructors
    continue rejecting [$]. These constructors do not track saved-variable
    lifetime or negotiate SEARCHRES or command-specific extensions. *)
val uid_sort : keys:(Sort.key * Sort.order) list -> charset:string ->
  criterion:string -> (string, error) result
val uid_sort_extended : returns:Sort.return list ->
  keys:(Sort.key * Sort.order) list -> charset:string -> criterion:string ->
  (string, error) result
(** RFC 5267 ESORT. Empty return options mean ALL. PARTIAL requires
    CONTEXT=SORT and positive 32-bit positions; reversed bounds are equivalent.
    ALL and PARTIAL are mutually exclusive and options cannot repeat. *)
val uid_thread : algorithm:Thread.algorithm -> charset:string ->
  criterion:string -> (string, error) result
(** RFC 5256 UID results. An [Other] algorithm name must be an atom.
    Charset is mandatory. Criteria retain SEARCH syntax and may contain
    sequence sets even though results contain UIDs. Literal
    search strings are not supported by these single-line constructors.
    SORT accepts 1..100 priority-ordered keys; descending applies per key. *)
val uid_search_partial : range:(int64 * int64) -> criterion:string ->
  (string, error) result
(** RFC 9394 result positions may shift while the mailbox changes. A page is
    not proof of complete inventory. Both bounds must have the same sign. *)
val uid_batches : ?range:(int64 * int64) -> size:int64 -> unit ->
  (string, error) result
(** RFC 10022. Size is at least 500. A requested batch-index range is
    ascending and may span at most 100000 messages. Ranges returned by the
    server are boundaries, not proof that every UID within them exists. Caller
    must enforce the RFC's limits on how often UIDBATCHES is reissued. *)
val uid_store : set:string -> operation:[ `Add | `Remove | `Replace ] ->
  silent:bool -> flags:string list -> (string, error) result
val uid_store_mod : ?unchangedsince:int64 -> set:string ->
  operation:[ `Add | `Remove | `Replace ] ->
  silent:bool -> flags:string list -> unit -> (string, error) result
val uid_copy : set:string -> mailbox:string -> (string, error) result
val uid_move : set:string -> mailbox:string -> (string, error) result
val uid_expunge : set:string -> (string, error) result
(** [uid_expunge ~set] is the RFC 4315 [UID EXPUNGE set]. *)
val append_part_prefix : ?non_sync:bool -> ?flags:string list ->
  ?internal_date:Internal_date.t -> size:int64 -> unit -> (string, error) result
(** [append_part_prefix ()] is the space-prefixed next MULTIAPPEND argument,
    ending in a literal marker. [non_sync=true] appends [+] to the size;
    the caller must negotiate permission and enforce the applicable size cap. *)

val append_prefix : mailbox:string -> ?non_sync:bool -> ?flags:string list ->
  ?internal_date:Internal_date.t -> size:int64 ->
  unit -> (string, error) result
(** Returns syntax ending in a synchronizing [{size}\r\n]. The caller waits for
    continuation, sends exactly [size] bytes, then sends [\r\n].
    [non_sync=true] emits [{size+}] and skips the continuation wait; the caller
    must negotiate this syntax and enforce the applicable size cap.
    [internal_date] is a validated RFC 9051 date-time value. *)

val append_binary_prefix : mailbox:string -> ?non_sync:bool -> ?flags:string list ->
  ?internal_date:Internal_date.t -> size:int64 ->
  unit -> (string, error) result
(** RFC 3516 APPEND literal8, ending in synchronizing [~{size}\r\n]. Requires
    the BINARY capability; IMAP4rev2 alone does not include binary APPEND.
    [non_sync=true] emits [~{size+}] when separately negotiated; otherwise
    send exactly [size] octets after continuation. The server may transform
    content-transfer encoding without losing data. *)
