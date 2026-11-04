[@@@ocaml.warning "-27-32-37-60"]

type ro = Capnp.Message.ro
type rw = Capnp.Message.rw

module type S = sig
  module MessageWrapper : Capnp.RPC.S
  type 'cap message_t = 'cap MessageWrapper.Message.t
  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t
  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t

  module Type_12865824133959433560 : sig
    type t =
      | Failed
      | Overloaded
      | Disconnected
      | Unimplemented
      | Undefined of int
  end

  module Reader : sig
    type array_t
    type builder_array_t
    type pointer_t = ro MessageWrapper.Slice.t option
    val of_pointer : pointer_t -> 'a reader_t
    module Message : sig
      type struct_t = [`Message_91b79f1f808db032]
      type t = struct_t reader_t
      type unnamed_union_t =
        | Unimplemented of [`Message_91b79f1f808db032] reader_t
        | Abort of [`Exception_d625b7063acf691a] reader_t
        | Bootstrap of [`Bootstrap_e94ccf8031176ec4] reader_t
        | Call of [`Call_836a53ce789d4cd4] reader_t
        | Return of [`Return_9e19b28d3db3573a] reader_t
        | Finish of [`Finish_d37d2eb2c2f80e63] reader_t
        | Resolve of [`Resolve_bbc29655fa89086e] reader_t
        | Release of [`Release_ad1a6c0d7dd07497] reader_t
        | Disembargo of [`Disembargo_f964368b0fbd3711] reader_t
        | ObsoleteSave of pointer_t
        | ObsoleteDelete of pointer_t
        | Provide of [`Provide_9c6a046bfbc1ac5a] reader_t
        | Accept of [`Accept_d4c9b56290554016] reader_t
        | Join of [`Join_fbe1980490e001af] reader_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Bootstrap : sig
      type struct_t = [`Bootstrap_e94ccf8031176ec4]
      type t = struct_t reader_t
      val question_id_get : t -> Stdint.Uint32.t
      val question_id_get_int_exn : t -> int
      val deprecated_object_id_get : t -> pointer_t
      val deprecated_object_id_get_interface : t -> 'a MessageWrapper.Capability.t option
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Call : sig
      type struct_t = [`Call_836a53ce789d4cd4]
      type t = struct_t reader_t
      module SendResultsTo : sig
        type struct_t = [`SendResultsTo_dae8b0f61aab5f99]
        type t = struct_t reader_t
        type unnamed_union_t =
          | Caller
          | Yourself
          | ThirdParty of pointer_t
          | Undefined of int
        val get : t -> unnamed_union_t
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      val question_id_get : t -> Stdint.Uint32.t
      val question_id_get_int_exn : t -> int
      val has_target : t -> bool
      val target_get : t -> [`MessageTarget_95bc14545813fbc1] reader_t
      val target_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`MessageTarget_95bc14545813fbc1] MessageWrapper.StructRef.t
      val interface_id_get : t -> Stdint.Uint64.t
      val interface_id_get_int_exn : t -> int
      val method_id_get : t -> int
      val allow_third_party_tail_call_get : t -> bool
      val has_params : t -> bool
      val params_get : t -> [`Payload_9a0e61223d96743b] reader_t
      val params_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`Payload_9a0e61223d96743b] MessageWrapper.StructRef.t
      val send_results_to_get : t -> SendResultsTo.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Return : sig
      type struct_t = [`Return_9e19b28d3db3573a]
      type t = struct_t reader_t
      type unnamed_union_t =
        | Results of [`Payload_9a0e61223d96743b] reader_t
        | Exception of [`Exception_d625b7063acf691a] reader_t
        | Canceled
        | ResultsSentElsewhere
        | TakeFromOtherQuestion of Stdint.Uint32.t
        | AcceptFromThirdParty of pointer_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val answer_id_get : t -> Stdint.Uint32.t
      val answer_id_get_int_exn : t -> int
      val release_param_caps_get : t -> bool
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Finish : sig
      type struct_t = [`Finish_d37d2eb2c2f80e63]
      type t = struct_t reader_t
      val question_id_get : t -> Stdint.Uint32.t
      val question_id_get_int_exn : t -> int
      val release_result_caps_get : t -> bool
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Resolve : sig
      type struct_t = [`Resolve_bbc29655fa89086e]
      type t = struct_t reader_t
      type unnamed_union_t =
        | Cap of [`CapDescriptor_8523ddc40b86b8b0] reader_t
        | Exception of [`Exception_d625b7063acf691a] reader_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val promise_id_get : t -> Stdint.Uint32.t
      val promise_id_get_int_exn : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Release : sig
      type struct_t = [`Release_ad1a6c0d7dd07497]
      type t = struct_t reader_t
      val id_get : t -> Stdint.Uint32.t
      val id_get_int_exn : t -> int
      val reference_count_get : t -> Stdint.Uint32.t
      val reference_count_get_int_exn : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Disembargo : sig
      type struct_t = [`Disembargo_f964368b0fbd3711]
      type t = struct_t reader_t
      module Context : sig
        type struct_t = [`Context_d562b4df655bdd4d]
        type t = struct_t reader_t
        type unnamed_union_t =
          | SenderLoopback of Stdint.Uint32.t
          | ReceiverLoopback of Stdint.Uint32.t
          | Accept
          | Provide of Stdint.Uint32.t
          | Undefined of int
        val get : t -> unnamed_union_t
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      val has_target : t -> bool
      val target_get : t -> [`MessageTarget_95bc14545813fbc1] reader_t
      val target_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`MessageTarget_95bc14545813fbc1] MessageWrapper.StructRef.t
      val context_get : t -> Context.t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Provide : sig
      type struct_t = [`Provide_9c6a046bfbc1ac5a]
      type t = struct_t reader_t
      val question_id_get : t -> Stdint.Uint32.t
      val question_id_get_int_exn : t -> int
      val has_target : t -> bool
      val target_get : t -> [`MessageTarget_95bc14545813fbc1] reader_t
      val target_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`MessageTarget_95bc14545813fbc1] MessageWrapper.StructRef.t
      val recipient_get : t -> pointer_t
      val recipient_get_interface : t -> 'a MessageWrapper.Capability.t option
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Accept : sig
      type struct_t = [`Accept_d4c9b56290554016]
      type t = struct_t reader_t
      val question_id_get : t -> Stdint.Uint32.t
      val question_id_get_int_exn : t -> int
      val provision_get : t -> pointer_t
      val provision_get_interface : t -> 'a MessageWrapper.Capability.t option
      val embargo_get : t -> bool
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Join : sig
      type struct_t = [`Join_fbe1980490e001af]
      type t = struct_t reader_t
      val question_id_get : t -> Stdint.Uint32.t
      val question_id_get_int_exn : t -> int
      val has_target : t -> bool
      val target_get : t -> [`MessageTarget_95bc14545813fbc1] reader_t
      val target_get_pipelined : struct_t MessageWrapper.StructRef.t -> [`MessageTarget_95bc14545813fbc1] MessageWrapper.StructRef.t
      val key_part_get : t -> pointer_t
      val key_part_get_interface : t -> 'a MessageWrapper.Capability.t option
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module MessageTarget : sig
      type struct_t = [`MessageTarget_95bc14545813fbc1]
      type t = struct_t reader_t
      type unnamed_union_t =
        | ImportedCap of Stdint.Uint32.t
        | PromisedAnswer of [`PromisedAnswer_d800b1d6cd6f1ca0] reader_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Payload : sig
      type struct_t = [`Payload_9a0e61223d96743b]
      type t = struct_t reader_t
      val content_get : t -> pointer_t
      val content_get_interface : t -> 'a MessageWrapper.Capability.t option
      val has_cap_table : t -> bool
      val cap_table_get : t -> (ro, [`CapDescriptor_8523ddc40b86b8b0] reader_t, array_t) Capnp.Array.t
      val cap_table_get_list : t -> [`CapDescriptor_8523ddc40b86b8b0] reader_t list
      val cap_table_get_array : t -> [`CapDescriptor_8523ddc40b86b8b0] reader_t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module CapDescriptor : sig
      type struct_t = [`CapDescriptor_8523ddc40b86b8b0]
      type t = struct_t reader_t
      type unnamed_union_t =
        | None
        | SenderHosted of Stdint.Uint32.t
        | SenderPromise of Stdint.Uint32.t
        | ReceiverHosted of Stdint.Uint32.t
        | ReceiverAnswer of [`PromisedAnswer_d800b1d6cd6f1ca0] reader_t
        | ThirdPartyHosted of [`ThirdPartyCapDescriptor_d37007fde1f0027d] reader_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module PromisedAnswer : sig
      type struct_t = [`PromisedAnswer_d800b1d6cd6f1ca0]
      type t = struct_t reader_t
      module Op : sig
        type struct_t = [`Op_f316944415569081]
        type t = struct_t reader_t
        type unnamed_union_t =
          | Noop
          | GetPointerField of int
          | Undefined of int
        val get : t -> unnamed_union_t
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      val question_id_get : t -> Stdint.Uint32.t
      val question_id_get_int_exn : t -> int
      val has_transform : t -> bool
      val transform_get : t -> (ro, [`Op_f316944415569081] reader_t, array_t) Capnp.Array.t
      val transform_get_list : t -> [`Op_f316944415569081] reader_t list
      val transform_get_array : t -> [`Op_f316944415569081] reader_t array
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ThirdPartyCapDescriptor : sig
      type struct_t = [`ThirdPartyCapDescriptor_d37007fde1f0027d]
      type t = struct_t reader_t
      val id_get : t -> pointer_t
      val id_get_interface : t -> 'a MessageWrapper.Capability.t option
      val vine_id_get : t -> Stdint.Uint32.t
      val vine_id_get_int_exn : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Exception : sig
      type struct_t = [`Exception_d625b7063acf691a]
      type t = struct_t reader_t
      module Type : sig
        type t = Type_12865824133959433560.t =
          | Failed
          | Overloaded
          | Disconnected
          | Unimplemented
          | Undefined of int
      end
      val has_reason : t -> bool
      val reason_get : t -> string
      val type_get : t -> Type_12865824133959433560.t
      val obsolete_is_callers_fault_get : t -> bool
      val obsolete_durability_get : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
  end

  module Builder : sig
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t
    module Message : sig
      type struct_t = [`Message_91b79f1f808db032]
      type t = struct_t builder_t
      type unnamed_union_t =
        | Unimplemented of [`Message_91b79f1f808db032] builder_t
        | Abort of [`Exception_d625b7063acf691a] builder_t
        | Bootstrap of [`Bootstrap_e94ccf8031176ec4] builder_t
        | Call of [`Call_836a53ce789d4cd4] builder_t
        | Return of [`Return_9e19b28d3db3573a] builder_t
        | Finish of [`Finish_d37d2eb2c2f80e63] builder_t
        | Resolve of [`Resolve_bbc29655fa89086e] builder_t
        | Release of [`Release_ad1a6c0d7dd07497] builder_t
        | Disembargo of [`Disembargo_f964368b0fbd3711] builder_t
        | ObsoleteSave of pointer_t
        | ObsoleteDelete of pointer_t
        | Provide of [`Provide_9c6a046bfbc1ac5a] builder_t
        | Accept of [`Accept_d4c9b56290554016] builder_t
        | Join of [`Join_fbe1980490e001af] builder_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val unimplemented_set_reader : t -> [`Message_91b79f1f808db032] reader_t -> [`Message_91b79f1f808db032] builder_t
      val unimplemented_set_builder : t -> [`Message_91b79f1f808db032] builder_t -> [`Message_91b79f1f808db032] builder_t
      val unimplemented_init : t -> [`Message_91b79f1f808db032] builder_t
      val abort_set_reader : t -> [`Exception_d625b7063acf691a] reader_t -> [`Exception_d625b7063acf691a] builder_t
      val abort_set_builder : t -> [`Exception_d625b7063acf691a] builder_t -> [`Exception_d625b7063acf691a] builder_t
      val abort_init : t -> [`Exception_d625b7063acf691a] builder_t
      val bootstrap_set_reader : t -> [`Bootstrap_e94ccf8031176ec4] reader_t -> [`Bootstrap_e94ccf8031176ec4] builder_t
      val bootstrap_set_builder : t -> [`Bootstrap_e94ccf8031176ec4] builder_t -> [`Bootstrap_e94ccf8031176ec4] builder_t
      val bootstrap_init : t -> [`Bootstrap_e94ccf8031176ec4] builder_t
      val call_set_reader : t -> [`Call_836a53ce789d4cd4] reader_t -> [`Call_836a53ce789d4cd4] builder_t
      val call_set_builder : t -> [`Call_836a53ce789d4cd4] builder_t -> [`Call_836a53ce789d4cd4] builder_t
      val call_init : t -> [`Call_836a53ce789d4cd4] builder_t
      val return_set_reader : t -> [`Return_9e19b28d3db3573a] reader_t -> [`Return_9e19b28d3db3573a] builder_t
      val return_set_builder : t -> [`Return_9e19b28d3db3573a] builder_t -> [`Return_9e19b28d3db3573a] builder_t
      val return_init : t -> [`Return_9e19b28d3db3573a] builder_t
      val finish_set_reader : t -> [`Finish_d37d2eb2c2f80e63] reader_t -> [`Finish_d37d2eb2c2f80e63] builder_t
      val finish_set_builder : t -> [`Finish_d37d2eb2c2f80e63] builder_t -> [`Finish_d37d2eb2c2f80e63] builder_t
      val finish_init : t -> [`Finish_d37d2eb2c2f80e63] builder_t
      val resolve_set_reader : t -> [`Resolve_bbc29655fa89086e] reader_t -> [`Resolve_bbc29655fa89086e] builder_t
      val resolve_set_builder : t -> [`Resolve_bbc29655fa89086e] builder_t -> [`Resolve_bbc29655fa89086e] builder_t
      val resolve_init : t -> [`Resolve_bbc29655fa89086e] builder_t
      val release_set_reader : t -> [`Release_ad1a6c0d7dd07497] reader_t -> [`Release_ad1a6c0d7dd07497] builder_t
      val release_set_builder : t -> [`Release_ad1a6c0d7dd07497] builder_t -> [`Release_ad1a6c0d7dd07497] builder_t
      val release_init : t -> [`Release_ad1a6c0d7dd07497] builder_t
      val disembargo_set_reader : t -> [`Disembargo_f964368b0fbd3711] reader_t -> [`Disembargo_f964368b0fbd3711] builder_t
      val disembargo_set_builder : t -> [`Disembargo_f964368b0fbd3711] builder_t -> [`Disembargo_f964368b0fbd3711] builder_t
      val disembargo_init : t -> [`Disembargo_f964368b0fbd3711] builder_t
      val obsolete_save_set : t -> pointer_t -> pointer_t
      val obsolete_save_set_reader : t -> Reader.pointer_t -> pointer_t
      val obsolete_save_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
      val obsolete_delete_set : t -> pointer_t -> pointer_t
      val obsolete_delete_set_reader : t -> Reader.pointer_t -> pointer_t
      val obsolete_delete_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
      val provide_set_reader : t -> [`Provide_9c6a046bfbc1ac5a] reader_t -> [`Provide_9c6a046bfbc1ac5a] builder_t
      val provide_set_builder : t -> [`Provide_9c6a046bfbc1ac5a] builder_t -> [`Provide_9c6a046bfbc1ac5a] builder_t
      val provide_init : t -> [`Provide_9c6a046bfbc1ac5a] builder_t
      val accept_set_reader : t -> [`Accept_d4c9b56290554016] reader_t -> [`Accept_d4c9b56290554016] builder_t
      val accept_set_builder : t -> [`Accept_d4c9b56290554016] builder_t -> [`Accept_d4c9b56290554016] builder_t
      val accept_init : t -> [`Accept_d4c9b56290554016] builder_t
      val join_set_reader : t -> [`Join_fbe1980490e001af] reader_t -> [`Join_fbe1980490e001af] builder_t
      val join_set_builder : t -> [`Join_fbe1980490e001af] builder_t -> [`Join_fbe1980490e001af] builder_t
      val join_init : t -> [`Join_fbe1980490e001af] builder_t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Bootstrap : sig
      type struct_t = [`Bootstrap_e94ccf8031176ec4]
      type t = struct_t builder_t
      val question_id_get : t -> Stdint.Uint32.t
      val question_id_get_int_exn : t -> int
      val question_id_set : t -> Stdint.Uint32.t -> unit
      val question_id_set_int_exn : t -> int -> unit
      val deprecated_object_id_get : t -> pointer_t
      val deprecated_object_id_get_interface : t -> 'a MessageWrapper.Capability.t option
      val deprecated_object_id_set : t -> pointer_t -> pointer_t
      val deprecated_object_id_set_reader : t -> Reader.pointer_t -> pointer_t
      val deprecated_object_id_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Call : sig
      type struct_t = [`Call_836a53ce789d4cd4]
      type t = struct_t builder_t
      module SendResultsTo : sig
        type struct_t = [`SendResultsTo_dae8b0f61aab5f99]
        type t = struct_t builder_t
        type unnamed_union_t =
          | Caller
          | Yourself
          | ThirdParty of pointer_t
          | Undefined of int
        val get : t -> unnamed_union_t
        val caller_set : t -> unit
        val yourself_set : t -> unit
        val third_party_set : t -> pointer_t -> pointer_t
        val third_party_set_reader : t -> Reader.pointer_t -> pointer_t
        val third_party_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      val question_id_get : t -> Stdint.Uint32.t
      val question_id_get_int_exn : t -> int
      val question_id_set : t -> Stdint.Uint32.t -> unit
      val question_id_set_int_exn : t -> int -> unit
      val has_target : t -> bool
      val target_get : t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val target_set_reader : t -> [`MessageTarget_95bc14545813fbc1] reader_t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val target_set_builder : t -> [`MessageTarget_95bc14545813fbc1] builder_t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val target_init : t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val interface_id_get : t -> Stdint.Uint64.t
      val interface_id_get_int_exn : t -> int
      val interface_id_set : t -> Stdint.Uint64.t -> unit
      val interface_id_set_int_exn : t -> int -> unit
      val method_id_get : t -> int
      val method_id_set_exn : t -> int -> unit
      val allow_third_party_tail_call_get : t -> bool
      val allow_third_party_tail_call_set : t -> bool -> unit
      val has_params : t -> bool
      val params_get : t -> [`Payload_9a0e61223d96743b] builder_t
      val params_set_reader : t -> [`Payload_9a0e61223d96743b] reader_t -> [`Payload_9a0e61223d96743b] builder_t
      val params_set_builder : t -> [`Payload_9a0e61223d96743b] builder_t -> [`Payload_9a0e61223d96743b] builder_t
      val params_init : t -> [`Payload_9a0e61223d96743b] builder_t
      val send_results_to_get : t -> SendResultsTo.t
      val send_results_to_init : t -> SendResultsTo.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Return : sig
      type struct_t = [`Return_9e19b28d3db3573a]
      type t = struct_t builder_t
      type unnamed_union_t =
        | Results of [`Payload_9a0e61223d96743b] builder_t
        | Exception of [`Exception_d625b7063acf691a] builder_t
        | Canceled
        | ResultsSentElsewhere
        | TakeFromOtherQuestion of Stdint.Uint32.t
        | AcceptFromThirdParty of pointer_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val results_set_reader : t -> [`Payload_9a0e61223d96743b] reader_t -> [`Payload_9a0e61223d96743b] builder_t
      val results_set_builder : t -> [`Payload_9a0e61223d96743b] builder_t -> [`Payload_9a0e61223d96743b] builder_t
      val results_init : t -> [`Payload_9a0e61223d96743b] builder_t
      val exception_set_reader : t -> [`Exception_d625b7063acf691a] reader_t -> [`Exception_d625b7063acf691a] builder_t
      val exception_set_builder : t -> [`Exception_d625b7063acf691a] builder_t -> [`Exception_d625b7063acf691a] builder_t
      val exception_init : t -> [`Exception_d625b7063acf691a] builder_t
      val canceled_set : t -> unit
      val results_sent_elsewhere_set : t -> unit
      val take_from_other_question_set : t -> Stdint.Uint32.t -> unit
      val take_from_other_question_set_int_exn : t -> int -> unit
      val accept_from_third_party_set : t -> pointer_t -> pointer_t
      val accept_from_third_party_set_reader : t -> Reader.pointer_t -> pointer_t
      val accept_from_third_party_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
      val answer_id_get : t -> Stdint.Uint32.t
      val answer_id_get_int_exn : t -> int
      val answer_id_set : t -> Stdint.Uint32.t -> unit
      val answer_id_set_int_exn : t -> int -> unit
      val release_param_caps_get : t -> bool
      val release_param_caps_set : t -> bool -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Finish : sig
      type struct_t = [`Finish_d37d2eb2c2f80e63]
      type t = struct_t builder_t
      val question_id_get : t -> Stdint.Uint32.t
      val question_id_get_int_exn : t -> int
      val question_id_set : t -> Stdint.Uint32.t -> unit
      val question_id_set_int_exn : t -> int -> unit
      val release_result_caps_get : t -> bool
      val release_result_caps_set : t -> bool -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Resolve : sig
      type struct_t = [`Resolve_bbc29655fa89086e]
      type t = struct_t builder_t
      type unnamed_union_t =
        | Cap of [`CapDescriptor_8523ddc40b86b8b0] builder_t
        | Exception of [`Exception_d625b7063acf691a] builder_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val cap_set_reader : t -> [`CapDescriptor_8523ddc40b86b8b0] reader_t -> [`CapDescriptor_8523ddc40b86b8b0] builder_t
      val cap_set_builder : t -> [`CapDescriptor_8523ddc40b86b8b0] builder_t -> [`CapDescriptor_8523ddc40b86b8b0] builder_t
      val cap_init : t -> [`CapDescriptor_8523ddc40b86b8b0] builder_t
      val exception_set_reader : t -> [`Exception_d625b7063acf691a] reader_t -> [`Exception_d625b7063acf691a] builder_t
      val exception_set_builder : t -> [`Exception_d625b7063acf691a] builder_t -> [`Exception_d625b7063acf691a] builder_t
      val exception_init : t -> [`Exception_d625b7063acf691a] builder_t
      val promise_id_get : t -> Stdint.Uint32.t
      val promise_id_get_int_exn : t -> int
      val promise_id_set : t -> Stdint.Uint32.t -> unit
      val promise_id_set_int_exn : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Release : sig
      type struct_t = [`Release_ad1a6c0d7dd07497]
      type t = struct_t builder_t
      val id_get : t -> Stdint.Uint32.t
      val id_get_int_exn : t -> int
      val id_set : t -> Stdint.Uint32.t -> unit
      val id_set_int_exn : t -> int -> unit
      val reference_count_get : t -> Stdint.Uint32.t
      val reference_count_get_int_exn : t -> int
      val reference_count_set : t -> Stdint.Uint32.t -> unit
      val reference_count_set_int_exn : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Disembargo : sig
      type struct_t = [`Disembargo_f964368b0fbd3711]
      type t = struct_t builder_t
      module Context : sig
        type struct_t = [`Context_d562b4df655bdd4d]
        type t = struct_t builder_t
        type unnamed_union_t =
          | SenderLoopback of Stdint.Uint32.t
          | ReceiverLoopback of Stdint.Uint32.t
          | Accept
          | Provide of Stdint.Uint32.t
          | Undefined of int
        val get : t -> unnamed_union_t
        val sender_loopback_set : t -> Stdint.Uint32.t -> unit
        val sender_loopback_set_int_exn : t -> int -> unit
        val receiver_loopback_set : t -> Stdint.Uint32.t -> unit
        val receiver_loopback_set_int_exn : t -> int -> unit
        val accept_set : t -> unit
        val provide_set : t -> Stdint.Uint32.t -> unit
        val provide_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      val has_target : t -> bool
      val target_get : t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val target_set_reader : t -> [`MessageTarget_95bc14545813fbc1] reader_t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val target_set_builder : t -> [`MessageTarget_95bc14545813fbc1] builder_t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val target_init : t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val context_get : t -> Context.t
      val context_init : t -> Context.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Provide : sig
      type struct_t = [`Provide_9c6a046bfbc1ac5a]
      type t = struct_t builder_t
      val question_id_get : t -> Stdint.Uint32.t
      val question_id_get_int_exn : t -> int
      val question_id_set : t -> Stdint.Uint32.t -> unit
      val question_id_set_int_exn : t -> int -> unit
      val has_target : t -> bool
      val target_get : t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val target_set_reader : t -> [`MessageTarget_95bc14545813fbc1] reader_t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val target_set_builder : t -> [`MessageTarget_95bc14545813fbc1] builder_t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val target_init : t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val recipient_get : t -> pointer_t
      val recipient_get_interface : t -> 'a MessageWrapper.Capability.t option
      val recipient_set : t -> pointer_t -> pointer_t
      val recipient_set_reader : t -> Reader.pointer_t -> pointer_t
      val recipient_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Accept : sig
      type struct_t = [`Accept_d4c9b56290554016]
      type t = struct_t builder_t
      val question_id_get : t -> Stdint.Uint32.t
      val question_id_get_int_exn : t -> int
      val question_id_set : t -> Stdint.Uint32.t -> unit
      val question_id_set_int_exn : t -> int -> unit
      val provision_get : t -> pointer_t
      val provision_get_interface : t -> 'a MessageWrapper.Capability.t option
      val provision_set : t -> pointer_t -> pointer_t
      val provision_set_reader : t -> Reader.pointer_t -> pointer_t
      val provision_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
      val embargo_get : t -> bool
      val embargo_set : t -> bool -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Join : sig
      type struct_t = [`Join_fbe1980490e001af]
      type t = struct_t builder_t
      val question_id_get : t -> Stdint.Uint32.t
      val question_id_get_int_exn : t -> int
      val question_id_set : t -> Stdint.Uint32.t -> unit
      val question_id_set_int_exn : t -> int -> unit
      val has_target : t -> bool
      val target_get : t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val target_set_reader : t -> [`MessageTarget_95bc14545813fbc1] reader_t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val target_set_builder : t -> [`MessageTarget_95bc14545813fbc1] builder_t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val target_init : t -> [`MessageTarget_95bc14545813fbc1] builder_t
      val key_part_get : t -> pointer_t
      val key_part_get_interface : t -> 'a MessageWrapper.Capability.t option
      val key_part_set : t -> pointer_t -> pointer_t
      val key_part_set_reader : t -> Reader.pointer_t -> pointer_t
      val key_part_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module MessageTarget : sig
      type struct_t = [`MessageTarget_95bc14545813fbc1]
      type t = struct_t builder_t
      type unnamed_union_t =
        | ImportedCap of Stdint.Uint32.t
        | PromisedAnswer of [`PromisedAnswer_d800b1d6cd6f1ca0] builder_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val imported_cap_set : t -> Stdint.Uint32.t -> unit
      val imported_cap_set_int_exn : t -> int -> unit
      val promised_answer_set_reader : t -> [`PromisedAnswer_d800b1d6cd6f1ca0] reader_t -> [`PromisedAnswer_d800b1d6cd6f1ca0] builder_t
      val promised_answer_set_builder : t -> [`PromisedAnswer_d800b1d6cd6f1ca0] builder_t -> [`PromisedAnswer_d800b1d6cd6f1ca0] builder_t
      val promised_answer_init : t -> [`PromisedAnswer_d800b1d6cd6f1ca0] builder_t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Payload : sig
      type struct_t = [`Payload_9a0e61223d96743b]
      type t = struct_t builder_t
      val content_get : t -> pointer_t
      val content_get_interface : t -> 'a MessageWrapper.Capability.t option
      val content_set : t -> pointer_t -> pointer_t
      val content_set_reader : t -> Reader.pointer_t -> pointer_t
      val content_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
      val has_cap_table : t -> bool
      val cap_table_get : t -> (rw, [`CapDescriptor_8523ddc40b86b8b0] builder_t, array_t) Capnp.Array.t
      val cap_table_get_list : t -> [`CapDescriptor_8523ddc40b86b8b0] builder_t list
      val cap_table_get_array : t -> [`CapDescriptor_8523ddc40b86b8b0] builder_t array
      val cap_table_set : t -> (rw, [`CapDescriptor_8523ddc40b86b8b0] builder_t, array_t) Capnp.Array.t -> (rw, [`CapDescriptor_8523ddc40b86b8b0] builder_t, array_t) Capnp.Array.t
      val cap_table_set_list : t -> [`CapDescriptor_8523ddc40b86b8b0] builder_t list -> (rw, [`CapDescriptor_8523ddc40b86b8b0] builder_t, array_t) Capnp.Array.t
      val cap_table_set_array : t -> [`CapDescriptor_8523ddc40b86b8b0] builder_t array -> (rw, [`CapDescriptor_8523ddc40b86b8b0] builder_t, array_t) Capnp.Array.t
      val cap_table_init : t -> int -> (rw, [`CapDescriptor_8523ddc40b86b8b0] builder_t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module CapDescriptor : sig
      type struct_t = [`CapDescriptor_8523ddc40b86b8b0]
      type t = struct_t builder_t
      type unnamed_union_t =
        | None
        | SenderHosted of Stdint.Uint32.t
        | SenderPromise of Stdint.Uint32.t
        | ReceiverHosted of Stdint.Uint32.t
        | ReceiverAnswer of [`PromisedAnswer_d800b1d6cd6f1ca0] builder_t
        | ThirdPartyHosted of [`ThirdPartyCapDescriptor_d37007fde1f0027d] builder_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val none_set : t -> unit
      val sender_hosted_set : t -> Stdint.Uint32.t -> unit
      val sender_hosted_set_int_exn : t -> int -> unit
      val sender_promise_set : t -> Stdint.Uint32.t -> unit
      val sender_promise_set_int_exn : t -> int -> unit
      val receiver_hosted_set : t -> Stdint.Uint32.t -> unit
      val receiver_hosted_set_int_exn : t -> int -> unit
      val receiver_answer_set_reader : t -> [`PromisedAnswer_d800b1d6cd6f1ca0] reader_t -> [`PromisedAnswer_d800b1d6cd6f1ca0] builder_t
      val receiver_answer_set_builder : t -> [`PromisedAnswer_d800b1d6cd6f1ca0] builder_t -> [`PromisedAnswer_d800b1d6cd6f1ca0] builder_t
      val receiver_answer_init : t -> [`PromisedAnswer_d800b1d6cd6f1ca0] builder_t
      val third_party_hosted_set_reader : t -> [`ThirdPartyCapDescriptor_d37007fde1f0027d] reader_t -> [`ThirdPartyCapDescriptor_d37007fde1f0027d] builder_t
      val third_party_hosted_set_builder : t -> [`ThirdPartyCapDescriptor_d37007fde1f0027d] builder_t -> [`ThirdPartyCapDescriptor_d37007fde1f0027d] builder_t
      val third_party_hosted_init : t -> [`ThirdPartyCapDescriptor_d37007fde1f0027d] builder_t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module PromisedAnswer : sig
      type struct_t = [`PromisedAnswer_d800b1d6cd6f1ca0]
      type t = struct_t builder_t
      module Op : sig
        type struct_t = [`Op_f316944415569081]
        type t = struct_t builder_t
        type unnamed_union_t =
          | Noop
          | GetPointerField of int
          | Undefined of int
        val get : t -> unnamed_union_t
        val noop_set : t -> unit
        val get_pointer_field_set_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      val question_id_get : t -> Stdint.Uint32.t
      val question_id_get_int_exn : t -> int
      val question_id_set : t -> Stdint.Uint32.t -> unit
      val question_id_set_int_exn : t -> int -> unit
      val has_transform : t -> bool
      val transform_get : t -> (rw, [`Op_f316944415569081] builder_t, array_t) Capnp.Array.t
      val transform_get_list : t -> [`Op_f316944415569081] builder_t list
      val transform_get_array : t -> [`Op_f316944415569081] builder_t array
      val transform_set : t -> (rw, [`Op_f316944415569081] builder_t, array_t) Capnp.Array.t -> (rw, [`Op_f316944415569081] builder_t, array_t) Capnp.Array.t
      val transform_set_list : t -> [`Op_f316944415569081] builder_t list -> (rw, [`Op_f316944415569081] builder_t, array_t) Capnp.Array.t
      val transform_set_array : t -> [`Op_f316944415569081] builder_t array -> (rw, [`Op_f316944415569081] builder_t, array_t) Capnp.Array.t
      val transform_init : t -> int -> (rw, [`Op_f316944415569081] builder_t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ThirdPartyCapDescriptor : sig
      type struct_t = [`ThirdPartyCapDescriptor_d37007fde1f0027d]
      type t = struct_t builder_t
      val id_get : t -> pointer_t
      val id_get_interface : t -> 'a MessageWrapper.Capability.t option
      val id_set : t -> pointer_t -> pointer_t
      val id_set_reader : t -> Reader.pointer_t -> pointer_t
      val id_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
      val vine_id_get : t -> Stdint.Uint32.t
      val vine_id_get_int_exn : t -> int
      val vine_id_set : t -> Stdint.Uint32.t -> unit
      val vine_id_set_int_exn : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Exception : sig
      type struct_t = [`Exception_d625b7063acf691a]
      type t = struct_t builder_t
      module Type : sig
        type t = Type_12865824133959433560.t =
          | Failed
          | Overloaded
          | Disconnected
          | Unimplemented
          | Undefined of int
      end
      val has_reason : t -> bool
      val reason_get : t -> string
      val reason_set : t -> string -> unit
      val type_get : t -> Type_12865824133959433560.t
      val type_set : t -> Type_12865824133959433560.t -> unit
      val type_set_unsafe : t -> Type_12865824133959433560.t -> unit
      val obsolete_is_callers_fault_get : t -> bool
      val obsolete_is_callers_fault_set : t -> bool -> unit
      val obsolete_durability_get : t -> int
      val obsolete_durability_set_exn : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
  end
end

module MakeRPC(MessageWrapper : Capnp.RPC.S) = struct
  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t
  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t
  module CamlBytes = Bytes
  module DefaultsMessage_ = Capnp.BytesMessage

  let _builder_defaults_message =
    let message_segments = [
      Bytes.unsafe_of_string "\
      ";
    ] in
    DefaultsMessage_.Message.readonly
      (DefaultsMessage_.Message.of_storage message_segments)

  let invalid_msg = Capnp.Message.invalid_msg

  include Capnp.Runtime.BuilderInc.Make(MessageWrapper)

  type 'cap message_t = 'cap MessageWrapper.Message.t

  module Type_12865824133959433560 = struct
    type t =
      | Failed
      | Overloaded
      | Disconnected
      | Unimplemented
      | Undefined of int
    let decode u16 = match u16 with
      | 0 -> Failed
      | 1 -> Overloaded
      | 2 -> Disconnected
      | 3 -> Unimplemented
      | v -> Undefined v
    let encode_safe enum = match enum with
      | Failed -> 0
      | Overloaded -> 1
      | Disconnected -> 2
      | Unimplemented -> 3
      | Undefined x -> invalid_msg "Cannot encode undefined enum value."
    let encode_unsafe enum = match enum with
      | Failed -> 0
      | Overloaded -> 1
      | Disconnected -> 2
      | Unimplemented -> 3
      | Undefined x -> x
  end
  module DefaultsCopier_ =
    Capnp.Runtime.BuilderOps.Make(Capnp.BytesMessage)(MessageWrapper)

  let _reader_defaults_message =
    MessageWrapper.Message.create
      (DefaultsMessage_.Message.total_size _builder_defaults_message)


  module Reader = struct
    type array_t = ro MessageWrapper.ListStorage.t
    type builder_array_t = rw MessageWrapper.ListStorage.t
    type pointer_t = ro MessageWrapper.Slice.t option
    let of_pointer = RA_.deref_opt_struct_pointer

    module Message = struct
      type struct_t = [`Message_91b79f1f808db032]
      type t = struct_t reader_t
      let has_unimplemented x =
        RA_.has_field x 0
      let unimplemented_get x =
        RA_.get_struct x 0
      let unimplemented_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_abort x =
        RA_.has_field x 0
      let abort_get x =
        RA_.get_struct x 0
      let abort_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_bootstrap x =
        RA_.has_field x 0
      let bootstrap_get x =
        RA_.get_struct x 0
      let bootstrap_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_call x =
        RA_.has_field x 0
      let call_get x =
        RA_.get_struct x 0
      let call_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_return x =
        RA_.has_field x 0
      let return_get x =
        RA_.get_struct x 0
      let return_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_finish x =
        RA_.has_field x 0
      let finish_get x =
        RA_.get_struct x 0
      let finish_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_resolve x =
        RA_.has_field x 0
      let resolve_get x =
        RA_.get_struct x 0
      let resolve_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_release x =
        RA_.has_field x 0
      let release_get x =
        RA_.get_struct x 0
      let release_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_disembargo x =
        RA_.has_field x 0
      let disembargo_get x =
        RA_.get_struct x 0
      let disembargo_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let obsolete_save_get x =
        RA_.get_pointer x 0
      let obsolete_save_get_interface x =
        RA_.get_interface x 0
      let obsolete_delete_get x =
        RA_.get_pointer x 0
      let obsolete_delete_get_interface x =
        RA_.get_interface x 0
      let has_provide x =
        RA_.has_field x 0
      let provide_get x =
        RA_.get_struct x 0
      let provide_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_accept x =
        RA_.has_field x 0
      let accept_get x =
        RA_.get_struct x 0
      let accept_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_join x =
        RA_.has_field x 0
      let join_get x =
        RA_.get_struct x 0
      let join_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      type unnamed_union_t =
        | Unimplemented of [`Message_91b79f1f808db032] reader_t
        | Abort of [`Exception_d625b7063acf691a] reader_t
        | Bootstrap of [`Bootstrap_e94ccf8031176ec4] reader_t
        | Call of [`Call_836a53ce789d4cd4] reader_t
        | Return of [`Return_9e19b28d3db3573a] reader_t
        | Finish of [`Finish_d37d2eb2c2f80e63] reader_t
        | Resolve of [`Resolve_bbc29655fa89086e] reader_t
        | Release of [`Release_ad1a6c0d7dd07497] reader_t
        | Disembargo of [`Disembargo_f964368b0fbd3711] reader_t
        | ObsoleteSave of pointer_t
        | ObsoleteDelete of pointer_t
        | Provide of [`Provide_9c6a046bfbc1ac5a] reader_t
        | Accept of [`Accept_d4c9b56290554016] reader_t
        | Join of [`Join_fbe1980490e001af] reader_t
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 0 with
        | 0 -> Unimplemented (unimplemented_get x)
        | 1 -> Abort (abort_get x)
        | 8 -> Bootstrap (bootstrap_get x)
        | 2 -> Call (call_get x)
        | 3 -> Return (return_get x)
        | 4 -> Finish (finish_get x)
        | 5 -> Resolve (resolve_get x)
        | 6 -> Release (release_get x)
        | 13 -> Disembargo (disembargo_get x)
        | 7 -> ObsoleteSave (obsolete_save_get x)
        | 9 -> ObsoleteDelete (obsolete_delete_get x)
        | 10 -> Provide (provide_get x)
        | 11 -> Accept (accept_get x)
        | 12 -> Join (join_get x)
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Bootstrap = struct
      type struct_t = [`Bootstrap_e94ccf8031176ec4]
      type t = struct_t reader_t
      let question_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let question_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (question_id_get x)
      let deprecated_object_id_get x =
        RA_.get_pointer x 0
      let deprecated_object_id_get_interface x =
        RA_.get_interface x 0
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Call = struct
      type struct_t = [`Call_836a53ce789d4cd4]
      type t = struct_t reader_t
      module SendResultsTo = struct
        type struct_t = [`SendResultsTo_dae8b0f61aab5f99]
        type t = struct_t reader_t
        let caller_get x = ()
        let yourself_get x = ()
        let third_party_get x =
          RA_.get_pointer x 2
        let third_party_get_interface x =
          RA_.get_interface x 2
        type unnamed_union_t =
          | Caller
          | Yourself
          | ThirdParty of pointer_t
          | Undefined of int
        let get x =
          match RA_.get_uint16 ~default:0 x 6 with
          | 0 -> Caller
          | 1 -> Yourself
          | 2 -> ThirdParty (third_party_get x)
          | v -> Undefined v
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      let question_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let question_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (question_id_get x)
      let has_target x =
        RA_.has_field x 0
      let target_get x =
        RA_.get_struct x 0
      let target_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let interface_id_get x =
        RA_.get_uint64 ~default:Stdint.Uint64.zero x 8
      let interface_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (interface_id_get x)
      let method_id_get x =
        RA_.get_uint16 ~default:0 x 4
      let allow_third_party_tail_call_get x =
        RA_.get_bit ~default:false x ~byte_ofs:16 ~bit_ofs:0
      let has_params x =
        RA_.has_field x 1
      let params_get x =
        RA_.get_struct x 1
      let params_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 1
      let send_results_to_get x = RA_.cast_struct x
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Return = struct
      type struct_t = [`Return_9e19b28d3db3573a]
      type t = struct_t reader_t
      let has_results x =
        RA_.has_field x 0
      let results_get x =
        RA_.get_struct x 0
      let results_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_exception x =
        RA_.has_field x 0
      let exception_get x =
        RA_.get_struct x 0
      let exception_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let canceled_get x = ()
      let results_sent_elsewhere_get x = ()
      let take_from_other_question_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 8
      let take_from_other_question_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (take_from_other_question_get x)
      let accept_from_third_party_get x =
        RA_.get_pointer x 0
      let accept_from_third_party_get_interface x =
        RA_.get_interface x 0
      type unnamed_union_t =
        | Results of [`Payload_9a0e61223d96743b] reader_t
        | Exception of [`Exception_d625b7063acf691a] reader_t
        | Canceled
        | ResultsSentElsewhere
        | TakeFromOtherQuestion of Stdint.Uint32.t
        | AcceptFromThirdParty of pointer_t
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 6 with
        | 0 -> Results (results_get x)
        | 1 -> Exception (exception_get x)
        | 2 -> Canceled
        | 3 -> ResultsSentElsewhere
        | 4 -> TakeFromOtherQuestion (take_from_other_question_get x)
        | 5 -> AcceptFromThirdParty (accept_from_third_party_get x)
        | v -> Undefined v
      let answer_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let answer_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (answer_id_get x)
      let release_param_caps_get x =
        RA_.get_bit ~default:true x ~byte_ofs:4 ~bit_ofs:0
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Finish = struct
      type struct_t = [`Finish_d37d2eb2c2f80e63]
      type t = struct_t reader_t
      let question_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let question_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (question_id_get x)
      let release_result_caps_get x =
        RA_.get_bit ~default:true x ~byte_ofs:4 ~bit_ofs:0
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Resolve = struct
      type struct_t = [`Resolve_bbc29655fa89086e]
      type t = struct_t reader_t
      let has_cap x =
        RA_.has_field x 0
      let cap_get x =
        RA_.get_struct x 0
      let cap_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_exception x =
        RA_.has_field x 0
      let exception_get x =
        RA_.get_struct x 0
      let exception_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      type unnamed_union_t =
        | Cap of [`CapDescriptor_8523ddc40b86b8b0] reader_t
        | Exception of [`Exception_d625b7063acf691a] reader_t
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 4 with
        | 0 -> Cap (cap_get x)
        | 1 -> Exception (exception_get x)
        | v -> Undefined v
      let promise_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let promise_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (promise_id_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Release = struct
      type struct_t = [`Release_ad1a6c0d7dd07497]
      type t = struct_t reader_t
      let id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (id_get x)
      let reference_count_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 4
      let reference_count_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (reference_count_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Disembargo = struct
      type struct_t = [`Disembargo_f964368b0fbd3711]
      type t = struct_t reader_t
      module Context = struct
        type struct_t = [`Context_d562b4df655bdd4d]
        type t = struct_t reader_t
        let sender_loopback_get x =
          RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
        let sender_loopback_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (sender_loopback_get x)
        let receiver_loopback_get x =
          RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
        let receiver_loopback_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (receiver_loopback_get x)
        let accept_get x = ()
        let provide_get x =
          RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
        let provide_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (provide_get x)
        type unnamed_union_t =
          | SenderLoopback of Stdint.Uint32.t
          | ReceiverLoopback of Stdint.Uint32.t
          | Accept
          | Provide of Stdint.Uint32.t
          | Undefined of int
        let get x =
          match RA_.get_uint16 ~default:0 x 4 with
          | 0 -> SenderLoopback (sender_loopback_get x)
          | 1 -> ReceiverLoopback (receiver_loopback_get x)
          | 2 -> Accept
          | 3 -> Provide (provide_get x)
          | v -> Undefined v
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      let has_target x =
        RA_.has_field x 0
      let target_get x =
        RA_.get_struct x 0
      let target_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let context_get x = RA_.cast_struct x
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Provide = struct
      type struct_t = [`Provide_9c6a046bfbc1ac5a]
      type t = struct_t reader_t
      let question_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let question_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (question_id_get x)
      let has_target x =
        RA_.has_field x 0
      let target_get x =
        RA_.get_struct x 0
      let target_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let recipient_get x =
        RA_.get_pointer x 1
      let recipient_get_interface x =
        RA_.get_interface x 1
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Accept = struct
      type struct_t = [`Accept_d4c9b56290554016]
      type t = struct_t reader_t
      let question_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let question_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (question_id_get x)
      let provision_get x =
        RA_.get_pointer x 0
      let provision_get_interface x =
        RA_.get_interface x 0
      let embargo_get x =
        RA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:0
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Join = struct
      type struct_t = [`Join_fbe1980490e001af]
      type t = struct_t reader_t
      let question_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let question_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (question_id_get x)
      let has_target x =
        RA_.has_field x 0
      let target_get x =
        RA_.get_struct x 0
      let target_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let key_part_get x =
        RA_.get_pointer x 1
      let key_part_get_interface x =
        RA_.get_interface x 1
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module MessageTarget = struct
      type struct_t = [`MessageTarget_95bc14545813fbc1]
      type t = struct_t reader_t
      let imported_cap_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let imported_cap_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (imported_cap_get x)
      let has_promised_answer x =
        RA_.has_field x 0
      let promised_answer_get x =
        RA_.get_struct x 0
      let promised_answer_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      type unnamed_union_t =
        | ImportedCap of Stdint.Uint32.t
        | PromisedAnswer of [`PromisedAnswer_d800b1d6cd6f1ca0] reader_t
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 4 with
        | 0 -> ImportedCap (imported_cap_get x)
        | 1 -> PromisedAnswer (promised_answer_get x)
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Payload = struct
      type struct_t = [`Payload_9a0e61223d96743b]
      type t = struct_t reader_t
      let content_get x =
        RA_.get_pointer x 0
      let content_get_interface x =
        RA_.get_interface x 0
      let has_cap_table x =
        RA_.has_field x 1
      let cap_table_get x = 
        RA_.get_struct_list x 1
      let cap_table_get_list x =
        Capnp.Array.to_list (cap_table_get x)
      let cap_table_get_array x =
        Capnp.Array.to_array (cap_table_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module CapDescriptor = struct
      type struct_t = [`CapDescriptor_8523ddc40b86b8b0]
      type t = struct_t reader_t
      let none_get x = ()
      let sender_hosted_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 4
      let sender_hosted_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (sender_hosted_get x)
      let sender_promise_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 4
      let sender_promise_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (sender_promise_get x)
      let receiver_hosted_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 4
      let receiver_hosted_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (receiver_hosted_get x)
      let has_receiver_answer x =
        RA_.has_field x 0
      let receiver_answer_get x =
        RA_.get_struct x 0
      let receiver_answer_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_third_party_hosted x =
        RA_.has_field x 0
      let third_party_hosted_get x =
        RA_.get_struct x 0
      let third_party_hosted_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      type unnamed_union_t =
        | None
        | SenderHosted of Stdint.Uint32.t
        | SenderPromise of Stdint.Uint32.t
        | ReceiverHosted of Stdint.Uint32.t
        | ReceiverAnswer of [`PromisedAnswer_d800b1d6cd6f1ca0] reader_t
        | ThirdPartyHosted of [`ThirdPartyCapDescriptor_d37007fde1f0027d] reader_t
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 0 with
        | 0 -> None
        | 1 -> SenderHosted (sender_hosted_get x)
        | 2 -> SenderPromise (sender_promise_get x)
        | 3 -> ReceiverHosted (receiver_hosted_get x)
        | 4 -> ReceiverAnswer (receiver_answer_get x)
        | 5 -> ThirdPartyHosted (third_party_hosted_get x)
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module PromisedAnswer = struct
      type struct_t = [`PromisedAnswer_d800b1d6cd6f1ca0]
      type t = struct_t reader_t
      module Op = struct
        type struct_t = [`Op_f316944415569081]
        type t = struct_t reader_t
        let noop_get x = ()
        let get_pointer_field_get x =
          RA_.get_uint16 ~default:0 x 2
        type unnamed_union_t =
          | Noop
          | GetPointerField of int
          | Undefined of int
        let get x =
          match RA_.get_uint16 ~default:0 x 0 with
          | 0 -> Noop
          | 1 -> GetPointerField (get_pointer_field_get x)
          | v -> Undefined v
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      let question_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let question_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (question_id_get x)
      let has_transform x =
        RA_.has_field x 0
      let transform_get x = 
        RA_.get_struct_list x 0
      let transform_get_list x =
        Capnp.Array.to_list (transform_get x)
      let transform_get_array x =
        Capnp.Array.to_array (transform_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ThirdPartyCapDescriptor = struct
      type struct_t = [`ThirdPartyCapDescriptor_d37007fde1f0027d]
      type t = struct_t reader_t
      let id_get x =
        RA_.get_pointer x 0
      let id_get_interface x =
        RA_.get_interface x 0
      let vine_id_get x =
        RA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let vine_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (vine_id_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Exception = struct
      type struct_t = [`Exception_d625b7063acf691a]
      type t = struct_t reader_t
      module Type = struct
        type t = Type_12865824133959433560.t =
          | Failed
          | Overloaded
          | Disconnected
          | Unimplemented
          | Undefined of int
      end
      let has_reason x =
        RA_.has_field x 0
      let reason_get x =
        RA_.get_text ~default:"" x 0
      let type_get x =
        let discr = RA_.get_uint16 ~default:0 x 4 in
        Type_12865824133959433560.decode discr
      let obsolete_is_callers_fault_get x =
        RA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let obsolete_durability_get x =
        RA_.get_uint16 ~default:0 x 2
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
  end

  module Builder = struct
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t

    module Message = struct
      type struct_t = [`Message_91b79f1f808db032]
      type t = struct_t builder_t
      let has_unimplemented x =
        BA_.has_field x 0
      let unimplemented_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let unimplemented_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 v
      let unimplemented_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let unimplemented_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0
      let has_abort x =
        BA_.has_field x 0
      let abort_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let abort_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0 v
      let abort_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let abort_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0
      let has_bootstrap x =
        BA_.has_field x 0
      let bootstrap_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let bootstrap_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=8; BA_.Discr.byte_ofs=0} x 0 v
      let bootstrap_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=8; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let bootstrap_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=8; BA_.Discr.byte_ofs=0} x 0
      let has_call x =
        BA_.has_field x 0
      let call_get x =
        BA_.get_struct ~data_words:3 ~pointer_words:3 x 0
      let call_set_reader x v =
        BA_.set_struct ~data_words:3 ~pointer_words:3 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0 v
      let call_set_builder x v =
        BA_.set_struct ~data_words:3 ~pointer_words:3 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let call_init x =
        BA_.init_struct ~data_words:3 ~pointer_words:3 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0
      let has_return x =
        BA_.has_field x 0
      let return_get x =
        BA_.get_struct ~data_words:2 ~pointer_words:1 x 0
      let return_set_reader x v =
        BA_.set_struct ~data_words:2 ~pointer_words:1 ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x 0 v
      let return_set_builder x v =
        BA_.set_struct ~data_words:2 ~pointer_words:1 ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let return_init x =
        BA_.init_struct ~data_words:2 ~pointer_words:1 ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x 0
      let has_finish x =
        BA_.has_field x 0
      let finish_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:0 x 0
      let finish_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0 v
      let finish_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let finish_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:0 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0
      let has_resolve x =
        BA_.has_field x 0
      let resolve_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let resolve_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x 0 v
      let resolve_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let resolve_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x 0
      let has_release x =
        BA_.has_field x 0
      let release_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:0 x 0
      let release_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 ~discr:{BA_.Discr.value=6; BA_.Discr.byte_ofs=0} x 0 v
      let release_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:0 ~discr:{BA_.Discr.value=6; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let release_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:0 ~discr:{BA_.Discr.value=6; BA_.Discr.byte_ofs=0} x 0
      let has_disembargo x =
        BA_.has_field x 0
      let disembargo_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let disembargo_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=13; BA_.Discr.byte_ofs=0} x 0 v
      let disembargo_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=13; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let disembargo_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=13; BA_.Discr.byte_ofs=0} x 0
      let obsolete_save_get x =
        BA_.get_pointer x 0
      let obsolete_save_get_interface x =
        BA_.get_interface x 0
      let obsolete_save_set x v =
        BA_.set_pointer ~discr:{BA_.Discr.value=7; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let obsolete_save_set_reader x v =
        BA_.set_pointer ~discr:{BA_.Discr.value=7; BA_.Discr.byte_ofs=0} x 0 v
      let obsolete_save_set_interface x v =
        BA_.set_interface ~discr:{BA_.Discr.value=7; BA_.Discr.byte_ofs=0} x 0 v
      let obsolete_delete_get x =
        BA_.get_pointer x 0
      let obsolete_delete_get_interface x =
        BA_.get_interface x 0
      let obsolete_delete_set x v =
        BA_.set_pointer ~discr:{BA_.Discr.value=9; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let obsolete_delete_set_reader x v =
        BA_.set_pointer ~discr:{BA_.Discr.value=9; BA_.Discr.byte_ofs=0} x 0 v
      let obsolete_delete_set_interface x v =
        BA_.set_interface ~discr:{BA_.Discr.value=9; BA_.Discr.byte_ofs=0} x 0 v
      let has_provide x =
        BA_.has_field x 0
      let provide_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:2 x 0
      let provide_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:2 ~discr:{BA_.Discr.value=10; BA_.Discr.byte_ofs=0} x 0 v
      let provide_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:2 ~discr:{BA_.Discr.value=10; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let provide_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:2 ~discr:{BA_.Discr.value=10; BA_.Discr.byte_ofs=0} x 0
      let has_accept x =
        BA_.has_field x 0
      let accept_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let accept_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=11; BA_.Discr.byte_ofs=0} x 0 v
      let accept_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=11; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let accept_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=11; BA_.Discr.byte_ofs=0} x 0
      let has_join x =
        BA_.has_field x 0
      let join_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:2 x 0
      let join_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:2 ~discr:{BA_.Discr.value=12; BA_.Discr.byte_ofs=0} x 0 v
      let join_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:2 ~discr:{BA_.Discr.value=12; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let join_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:2 ~discr:{BA_.Discr.value=12; BA_.Discr.byte_ofs=0} x 0
      type unnamed_union_t =
        | Unimplemented of [`Message_91b79f1f808db032] builder_t
        | Abort of [`Exception_d625b7063acf691a] builder_t
        | Bootstrap of [`Bootstrap_e94ccf8031176ec4] builder_t
        | Call of [`Call_836a53ce789d4cd4] builder_t
        | Return of [`Return_9e19b28d3db3573a] builder_t
        | Finish of [`Finish_d37d2eb2c2f80e63] builder_t
        | Resolve of [`Resolve_bbc29655fa89086e] builder_t
        | Release of [`Release_ad1a6c0d7dd07497] builder_t
        | Disembargo of [`Disembargo_f964368b0fbd3711] builder_t
        | ObsoleteSave of pointer_t
        | ObsoleteDelete of pointer_t
        | Provide of [`Provide_9c6a046bfbc1ac5a] builder_t
        | Accept of [`Accept_d4c9b56290554016] builder_t
        | Join of [`Join_fbe1980490e001af] builder_t
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 0 with
        | 0 -> Unimplemented (unimplemented_get x)
        | 1 -> Abort (abort_get x)
        | 8 -> Bootstrap (bootstrap_get x)
        | 2 -> Call (call_get x)
        | 3 -> Return (return_get x)
        | 4 -> Finish (finish_get x)
        | 5 -> Resolve (resolve_get x)
        | 6 -> Release (release_get x)
        | 13 -> Disembargo (disembargo_get x)
        | 7 -> ObsoleteSave (obsolete_save_get x)
        | 9 -> ObsoleteDelete (obsolete_delete_get x)
        | 10 -> Provide (provide_get x)
        | 11 -> Accept (accept_get x)
        | 12 -> Join (join_get x)
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module Bootstrap = struct
      type struct_t = [`Bootstrap_e94ccf8031176ec4]
      type t = struct_t builder_t
      let question_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let question_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (question_id_get x)
      let question_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let question_id_set_int_exn x v = question_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let deprecated_object_id_get x =
        BA_.get_pointer x 0
      let deprecated_object_id_get_interface x =
        BA_.get_interface x 0
      let deprecated_object_id_set x v =
        BA_.set_pointer x 0 (Some v)
      let deprecated_object_id_set_reader x v =
        BA_.set_pointer x 0 v
      let deprecated_object_id_set_interface x v =
        BA_.set_interface x 0 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module Call = struct
      type struct_t = [`Call_836a53ce789d4cd4]
      type t = struct_t builder_t
      module SendResultsTo = struct
        type struct_t = [`SendResultsTo_dae8b0f61aab5f99]
        type t = struct_t builder_t
        let caller_get x = ()
        let caller_set x =
          BA_.set_void ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=6} x
        let yourself_get x = ()
        let yourself_set x =
          BA_.set_void ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=6} x
        let third_party_get x =
          BA_.get_pointer x 2
        let third_party_get_interface x =
          BA_.get_interface x 2
        let third_party_set x v =
          BA_.set_pointer ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=6} x 2 (Some v)
        let third_party_set_reader x v =
          BA_.set_pointer ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=6} x 2 v
        let third_party_set_interface x v =
          BA_.set_interface ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=6} x 2 v
        type unnamed_union_t =
          | Caller
          | Yourself
          | ThirdParty of pointer_t
          | Undefined of int
        let get x =
          match BA_.get_uint16 ~default:0 x 6 with
          | 0 -> Caller
          | 1 -> Yourself
          | 2 -> ThirdParty (third_party_get x)
          | v -> Undefined v
        let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:3 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:3 ~pointer_words:3 ()
        let init_pointer ptr =
          BA_.init_struct_pointer ptr ~data_words:3 ~pointer_words:3
      end
      let question_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let question_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (question_id_get x)
      let question_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let question_id_set_int_exn x v = question_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let has_target x =
        BA_.has_field x 0
      let target_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let target_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 v
      let target_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 (Some v)
      let target_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 0
      let interface_id_get x =
        BA_.get_uint64 ~default:Stdint.Uint64.zero x 8
      let interface_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint64_exn (interface_id_get x)
      let interface_id_set x v =
        BA_.set_uint64 ~default:Stdint.Uint64.zero x 8 v
      let interface_id_set_int_exn x v = interface_id_set x (Capnp.Runtime.Util.uint64_of_int_exn v)
      let method_id_get x =
        BA_.get_uint16 ~default:0 x 4
      let method_id_set_exn x v =
        BA_.set_uint16 ~default:0 x 4 v
      let allow_third_party_tail_call_get x =
        BA_.get_bit ~default:false x ~byte_ofs:16 ~bit_ofs:0
      let allow_third_party_tail_call_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:16 ~bit_ofs:0 v
      let has_params x =
        BA_.has_field x 1
      let params_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:2 x 1
      let params_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:2 x 1 v
      let params_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:2 x 1 (Some v)
      let params_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:2 x 1
      let send_results_to_get x = BA_.cast_struct x
      let send_results_to_init x =
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_int16 ~default:0 x 6 0 in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 16;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        BA_.cast_struct x
      let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:3 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:3 ~pointer_words:3 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:3 ~pointer_words:3
    end
    module Return = struct
      type struct_t = [`Return_9e19b28d3db3573a]
      type t = struct_t builder_t
      let has_results x =
        BA_.has_field x 0
      let results_get x =
        BA_.get_struct ~data_words:0 ~pointer_words:2 x 0
      let results_set_reader x v =
        BA_.set_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=6} x 0 v
      let results_set_builder x v =
        BA_.set_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=6} x 0 (Some v)
      let results_init x =
        BA_.init_struct ~data_words:0 ~pointer_words:2 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=6} x 0
      let has_exception x =
        BA_.has_field x 0
      let exception_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let exception_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=6} x 0 v
      let exception_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=6} x 0 (Some v)
      let exception_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=6} x 0
      let canceled_get x = ()
      let canceled_set x =
        BA_.set_void ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=6} x
      let results_sent_elsewhere_get x = ()
      let results_sent_elsewhere_set x =
        BA_.set_void ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=6} x
      let take_from_other_question_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 8
      let take_from_other_question_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (take_from_other_question_get x)
      let take_from_other_question_set x v =
        BA_.set_uint32 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=6} ~default:Stdint.Uint32.zero x 8 v
      let take_from_other_question_set_int_exn x v = take_from_other_question_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let accept_from_third_party_get x =
        BA_.get_pointer x 0
      let accept_from_third_party_get_interface x =
        BA_.get_interface x 0
      let accept_from_third_party_set x v =
        BA_.set_pointer ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=6} x 0 (Some v)
      let accept_from_third_party_set_reader x v =
        BA_.set_pointer ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=6} x 0 v
      let accept_from_third_party_set_interface x v =
        BA_.set_interface ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=6} x 0 v
      type unnamed_union_t =
        | Results of [`Payload_9a0e61223d96743b] builder_t
        | Exception of [`Exception_d625b7063acf691a] builder_t
        | Canceled
        | ResultsSentElsewhere
        | TakeFromOtherQuestion of Stdint.Uint32.t
        | AcceptFromThirdParty of pointer_t
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 6 with
        | 0 -> Results (results_get x)
        | 1 -> Exception (exception_get x)
        | 2 -> Canceled
        | 3 -> ResultsSentElsewhere
        | 4 -> TakeFromOtherQuestion (take_from_other_question_get x)
        | 5 -> AcceptFromThirdParty (accept_from_third_party_get x)
        | v -> Undefined v
      let answer_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let answer_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (answer_id_get x)
      let answer_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let answer_id_set_int_exn x v = answer_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let release_param_caps_get x =
        BA_.get_bit ~default:true x ~byte_ofs:4 ~bit_ofs:0
      let release_param_caps_set x v =
        BA_.set_bit ~default:true x ~byte_ofs:4 ~bit_ofs:0 v
      let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:2 ~pointer_words:1
    end
    module Finish = struct
      type struct_t = [`Finish_d37d2eb2c2f80e63]
      type t = struct_t builder_t
      let question_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let question_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (question_id_get x)
      let question_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let question_id_set_int_exn x v = question_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let release_result_caps_get x =
        BA_.get_bit ~default:true x ~byte_ofs:4 ~bit_ofs:0
      let release_result_caps_set x v =
        BA_.set_bit ~default:true x ~byte_ofs:4 ~bit_ofs:0 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:0 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:0 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:0
    end
    module Resolve = struct
      type struct_t = [`Resolve_bbc29655fa89086e]
      type t = struct_t builder_t
      let has_cap x =
        BA_.has_field x 0
      let cap_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let cap_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=4} x 0 v
      let cap_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=4} x 0 (Some v)
      let cap_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=4} x 0
      let has_exception x =
        BA_.has_field x 0
      let exception_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let exception_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=4} x 0 v
      let exception_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=4} x 0 (Some v)
      let exception_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=4} x 0
      type unnamed_union_t =
        | Cap of [`CapDescriptor_8523ddc40b86b8b0] builder_t
        | Exception of [`Exception_d625b7063acf691a] builder_t
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 4 with
        | 0 -> Cap (cap_get x)
        | 1 -> Exception (exception_get x)
        | v -> Undefined v
      let promise_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let promise_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (promise_id_get x)
      let promise_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let promise_id_set_int_exn x v = promise_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module Release = struct
      type struct_t = [`Release_ad1a6c0d7dd07497]
      type t = struct_t builder_t
      let id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (id_get x)
      let id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let id_set_int_exn x v = id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let reference_count_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 4
      let reference_count_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (reference_count_get x)
      let reference_count_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 4 v
      let reference_count_set_int_exn x v = reference_count_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:0 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:0 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:0
    end
    module Disembargo = struct
      type struct_t = [`Disembargo_f964368b0fbd3711]
      type t = struct_t builder_t
      module Context = struct
        type struct_t = [`Context_d562b4df655bdd4d]
        type t = struct_t builder_t
        let sender_loopback_get x =
          BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
        let sender_loopback_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (sender_loopback_get x)
        let sender_loopback_set x v =
          BA_.set_uint32 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=4} ~default:Stdint.Uint32.zero x 0 v
        let sender_loopback_set_int_exn x v = sender_loopback_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
        let receiver_loopback_get x =
          BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
        let receiver_loopback_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (receiver_loopback_get x)
        let receiver_loopback_set x v =
          BA_.set_uint32 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=4} ~default:Stdint.Uint32.zero x 0 v
        let receiver_loopback_set_int_exn x v = receiver_loopback_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
        let accept_get x = ()
        let accept_set x =
          BA_.set_void ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=4} x
        let provide_get x =
          BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
        let provide_get_int_exn x =
          Capnp.Runtime.Util.int_of_uint32_exn (provide_get x)
        let provide_set x v =
          BA_.set_uint32 ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=4} ~default:Stdint.Uint32.zero x 0 v
        let provide_set_int_exn x v = provide_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
        type unnamed_union_t =
          | SenderLoopback of Stdint.Uint32.t
          | ReceiverLoopback of Stdint.Uint32.t
          | Accept
          | Provide of Stdint.Uint32.t
          | Undefined of int
        let get x =
          match BA_.get_uint16 ~default:0 x 4 with
          | 0 -> SenderLoopback (sender_loopback_get x)
          | 1 -> ReceiverLoopback (receiver_loopback_get x)
          | 2 -> Accept
          | 3 -> Provide (provide_get x)
          | v -> Undefined v
        let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
        let init_pointer ptr =
          BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
      end
      let has_target x =
        BA_.has_field x 0
      let target_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let target_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 v
      let target_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 (Some v)
      let target_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 0
      let context_get x = BA_.cast_struct x
      let context_init x =
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_int16 ~default:0 x 4 0 in
        let () = BA_.set_int32 ~default:0l x 0 0l in
        let () = BA_.set_int32 ~default:0l x 0 0l in
        let () = BA_.set_int32 ~default:0l x 0 0l in
        BA_.cast_struct x
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module Provide = struct
      type struct_t = [`Provide_9c6a046bfbc1ac5a]
      type t = struct_t builder_t
      let question_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let question_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (question_id_get x)
      let question_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let question_id_set_int_exn x v = question_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let has_target x =
        BA_.has_field x 0
      let target_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let target_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 v
      let target_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 (Some v)
      let target_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 0
      let recipient_get x =
        BA_.get_pointer x 1
      let recipient_get_interface x =
        BA_.get_interface x 1
      let recipient_set x v =
        BA_.set_pointer x 1 (Some v)
      let recipient_set_reader x v =
        BA_.set_pointer x 1 v
      let recipient_set_interface x v =
        BA_.set_interface x 1 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:2
    end
    module Accept = struct
      type struct_t = [`Accept_d4c9b56290554016]
      type t = struct_t builder_t
      let question_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let question_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (question_id_get x)
      let question_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let question_id_set_int_exn x v = question_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let provision_get x =
        BA_.get_pointer x 0
      let provision_get_interface x =
        BA_.get_interface x 0
      let provision_set x v =
        BA_.set_pointer x 0 (Some v)
      let provision_set_reader x v =
        BA_.set_pointer x 0 v
      let provision_set_interface x v =
        BA_.set_interface x 0 v
      let embargo_get x =
        BA_.get_bit ~default:false x ~byte_ofs:4 ~bit_ofs:0
      let embargo_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:4 ~bit_ofs:0 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module Join = struct
      type struct_t = [`Join_fbe1980490e001af]
      type t = struct_t builder_t
      let question_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let question_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (question_id_get x)
      let question_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let question_id_set_int_exn x v = question_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let has_target x =
        BA_.has_field x 0
      let target_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let target_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 v
      let target_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 (Some v)
      let target_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 0
      let key_part_get x =
        BA_.get_pointer x 1
      let key_part_get_interface x =
        BA_.get_interface x 1
      let key_part_set x v =
        BA_.set_pointer x 1 (Some v)
      let key_part_set_reader x v =
        BA_.set_pointer x 1 v
      let key_part_set_interface x v =
        BA_.set_interface x 1 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:2
    end
    module MessageTarget = struct
      type struct_t = [`MessageTarget_95bc14545813fbc1]
      type t = struct_t builder_t
      let imported_cap_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let imported_cap_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (imported_cap_get x)
      let imported_cap_set x v =
        BA_.set_uint32 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=4} ~default:Stdint.Uint32.zero x 0 v
      let imported_cap_set_int_exn x v = imported_cap_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let has_promised_answer x =
        BA_.has_field x 0
      let promised_answer_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let promised_answer_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=4} x 0 v
      let promised_answer_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=4} x 0 (Some v)
      let promised_answer_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=4} x 0
      type unnamed_union_t =
        | ImportedCap of Stdint.Uint32.t
        | PromisedAnswer of [`PromisedAnswer_d800b1d6cd6f1ca0] builder_t
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 4 with
        | 0 -> ImportedCap (imported_cap_get x)
        | 1 -> PromisedAnswer (promised_answer_get x)
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module Payload = struct
      type struct_t = [`Payload_9a0e61223d96743b]
      type t = struct_t builder_t
      let content_get x =
        BA_.get_pointer x 0
      let content_get_interface x =
        BA_.get_interface x 0
      let content_set x v =
        BA_.set_pointer x 0 (Some v)
      let content_set_reader x v =
        BA_.set_pointer x 0 v
      let content_set_interface x v =
        BA_.set_interface x 0 v
      let has_cap_table x =
        BA_.has_field x 1
      let cap_table_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 1
      let cap_table_get_list x =
        Capnp.Array.to_list (cap_table_get x)
      let cap_table_get_array x =
        Capnp.Array.to_array (cap_table_get x)
      let cap_table_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:1 x 1 v
      let cap_table_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:1 x 1 n
      let cap_table_set_list x v =
        let builder = cap_table_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let cap_table_set_array x v =
        let builder = cap_table_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:2
    end
    module CapDescriptor = struct
      type struct_t = [`CapDescriptor_8523ddc40b86b8b0]
      type t = struct_t builder_t
      let none_get x = ()
      let none_set x =
        BA_.set_void ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x
      let sender_hosted_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 4
      let sender_hosted_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (sender_hosted_get x)
      let sender_hosted_set x v =
        BA_.set_uint32 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} ~default:Stdint.Uint32.zero x 4 v
      let sender_hosted_set_int_exn x v = sender_hosted_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let sender_promise_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 4
      let sender_promise_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (sender_promise_get x)
      let sender_promise_set x v =
        BA_.set_uint32 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} ~default:Stdint.Uint32.zero x 4 v
      let sender_promise_set_int_exn x v = sender_promise_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let receiver_hosted_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 4
      let receiver_hosted_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (receiver_hosted_get x)
      let receiver_hosted_set x v =
        BA_.set_uint32 ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} ~default:Stdint.Uint32.zero x 4 v
      let receiver_hosted_set_int_exn x v = receiver_hosted_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let has_receiver_answer x =
        BA_.has_field x 0
      let receiver_answer_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let receiver_answer_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0 v
      let receiver_answer_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let receiver_answer_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0
      let has_third_party_hosted x =
        BA_.has_field x 0
      let third_party_hosted_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let third_party_hosted_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x 0 v
      let third_party_hosted_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let third_party_hosted_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x 0
      type unnamed_union_t =
        | None
        | SenderHosted of Stdint.Uint32.t
        | SenderPromise of Stdint.Uint32.t
        | ReceiverHosted of Stdint.Uint32.t
        | ReceiverAnswer of [`PromisedAnswer_d800b1d6cd6f1ca0] builder_t
        | ThirdPartyHosted of [`ThirdPartyCapDescriptor_d37007fde1f0027d] builder_t
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 0 with
        | 0 -> None
        | 1 -> SenderHosted (sender_hosted_get x)
        | 2 -> SenderPromise (sender_promise_get x)
        | 3 -> ReceiverHosted (receiver_hosted_get x)
        | 4 -> ReceiverAnswer (receiver_answer_get x)
        | 5 -> ThirdPartyHosted (third_party_hosted_get x)
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module PromisedAnswer = struct
      type struct_t = [`PromisedAnswer_d800b1d6cd6f1ca0]
      type t = struct_t builder_t
      module Op = struct
        type struct_t = [`Op_f316944415569081]
        type t = struct_t builder_t
        let noop_get x = ()
        let noop_set x =
          BA_.set_void ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x
        let get_pointer_field_get x =
          BA_.get_uint16 ~default:0 x 2
        let get_pointer_field_set_exn x v =
          BA_.set_uint16 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} ~default:0 x 2 v
        type unnamed_union_t =
          | Noop
          | GetPointerField of int
          | Undefined of int
        let get x =
          match BA_.get_uint16 ~default:0 x 0 with
          | 0 -> Noop
          | 1 -> GetPointerField (get_pointer_field_get x)
          | v -> Undefined v
        let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:0 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:0 ()
        let init_pointer ptr =
          BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:0
      end
      let question_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let question_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (question_id_get x)
      let question_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let question_id_set_int_exn x v = question_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let has_transform x =
        BA_.has_field x 0
      let transform_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:0 x 0
      let transform_get_list x =
        Capnp.Array.to_list (transform_get x)
      let transform_get_array x =
        Capnp.Array.to_array (transform_get x)
      let transform_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:0 x 0 v
      let transform_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:0 x 0 n
      let transform_set_list x v =
        let builder = transform_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let transform_set_array x v =
        let builder = transform_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module ThirdPartyCapDescriptor = struct
      type struct_t = [`ThirdPartyCapDescriptor_d37007fde1f0027d]
      type t = struct_t builder_t
      let id_get x =
        BA_.get_pointer x 0
      let id_get_interface x =
        BA_.get_interface x 0
      let id_set x v =
        BA_.set_pointer x 0 (Some v)
      let id_set_reader x v =
        BA_.set_pointer x 0 v
      let id_set_interface x v =
        BA_.set_interface x 0 v
      let vine_id_get x =
        BA_.get_uint32 ~default:Stdint.Uint32.zero x 0
      let vine_id_get_int_exn x =
        Capnp.Runtime.Util.int_of_uint32_exn (vine_id_get x)
      let vine_id_set x v =
        BA_.set_uint32 ~default:Stdint.Uint32.zero x 0 v
      let vine_id_set_int_exn x v = vine_id_set x (Capnp.Runtime.Util.uint32_of_int_exn v)
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module Exception = struct
      type struct_t = [`Exception_d625b7063acf691a]
      type t = struct_t builder_t
      module Type = struct
        type t = Type_12865824133959433560.t =
          | Failed
          | Overloaded
          | Disconnected
          | Unimplemented
          | Undefined of int
      end
      let has_reason x =
        BA_.has_field x 0
      let reason_get x =
        BA_.get_text ~default:"" x 0
      let reason_set x v =
        BA_.set_text x 0 v
      let type_get x =
        let discr = BA_.get_uint16 ~default:0 x 4 in
        Type_12865824133959433560.decode discr
      let type_set x e =
        BA_.set_uint16 ~default:0 x 4 (Type_12865824133959433560.encode_safe e)
      let type_set_unsafe x e =
        BA_.set_uint16 ~default:0 x 4 (Type_12865824133959433560.encode_unsafe e)
      let obsolete_is_callers_fault_get x =
        BA_.get_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0
      let obsolete_is_callers_fault_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:0 ~bit_ofs:0 v
      let obsolete_durability_get x =
        BA_.get_uint16 ~default:0 x 2
      let obsolete_durability_set_exn x v =
        BA_.set_uint16 ~default:0 x 2 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
  end

  module Client = struct
  end

  module Service = struct
  end
  module MessageWrapper = MessageWrapper
end

module Make(M:Capnp.MessageSig.S) = MakeRPC(Capnp.RPC.None(M))
