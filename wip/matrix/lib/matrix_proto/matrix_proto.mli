@@ portable

(** Matrix protocol types and their JSON codecs.

    Each type is paired with a [Jsont.t] named [jsont] in the module that owns
    it, so a value decodes from and encodes to the JSON the client-server API
    puts on the wire. Nothing here performs I/O. *)

(** {1 Protocol modules} *)

module Id = Matrix_id
(** Matrix identifiers for users, rooms, events, devices and aliases.

    @canonical Matrix_proto.Id *)

module Event = Matrix_event
(** Event envelopes and the content of the standard event types.

    @canonical Matrix_proto.Event *)

module Sync = Matrix_sync
(** The [/sync] response types.

    @canonical Matrix_proto.Sync *)

module Sliding_sync = Matrix_sliding_sync
(** The simplified sliding sync request and response bodies (MSC4186).

    @canonical Matrix_proto.Sliding_sync *)

module Push = Matrix_push
(** Push rules and the notification actions they produce.

    @canonical Matrix_proto.Push *)

module Json = Matrix_json
(** Projections for JSON whose shape is not known until it is read.

    @canonical Matrix_proto.Json *)

module Common = Matrix_common
(** Request and response shapes shared by many endpoints.

    @canonical Matrix_proto.Common *)

module Base64 = Matrix_base64
(** Base64 in the form Matrix puts on the wire.

    @canonical Matrix_proto.Base64 *)

module Signed_json = Matrix_signed_json
(** Canonical JSON, the encoding Matrix signatures cover.

    @canonical Matrix_proto.Signed_json *)

(** {1 Compilation units}

    The modules above are aliases for the library's compilation units. These are
    the same modules under the names their files give them, which is what a
    reference written either way resolves through. *)

module Matrix_id = Matrix_id
(** @canonical Matrix_proto.Id *)

module Matrix_event = Matrix_event
(** @canonical Matrix_proto.Event *)

module Matrix_event_core = Matrix_event_core
(** @canonical Matrix_proto.Event *)

module Matrix_event_state = Matrix_event_state
(** @canonical Matrix_proto.Event *)

module Matrix_event_space = Matrix_event_space
(** @canonical Matrix_proto.Event *)

module Matrix_event_call = Matrix_event_call
(** @canonical Matrix_proto.Event *)

module Matrix_event_verification = Matrix_event_verification
(** @canonical Matrix_proto.Event *)

module Matrix_event_message = Matrix_event_message
(** @canonical Matrix_proto.Event *)

module Matrix_event_extensible = Matrix_event_extensible
(** @canonical Matrix_proto.Event *)

module Matrix_event_encrypted = Matrix_event_encrypted
(** @canonical Matrix_proto.Event *)

module Matrix_sync = Matrix_sync
(** @canonical Matrix_proto.Sync *)

module Matrix_sliding_sync = Matrix_sliding_sync
(** @canonical Matrix_proto.Sliding_sync *)

module Matrix_push = Matrix_push
(** @canonical Matrix_proto.Push *)

module Matrix_json = Matrix_json
(** @canonical Matrix_proto.Json *)

module Matrix_common = Matrix_common
(** @canonical Matrix_proto.Common *)

module Matrix_base64 = Matrix_base64
(** @canonical Matrix_proto.Base64 *)

module Matrix_signed_json = Matrix_signed_json
(** @canonical Matrix_proto.Signed_json *)
