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

module MakeRPC(MessageWrapper : Capnp.RPC.S) : sig
  include S with module MessageWrapper = MessageWrapper

  module Client : sig
  end

  module Service : sig
  end
end

module Make(M : Capnp.MessageSig.S) : module type of MakeRPC(Capnp.RPC.None(M))
