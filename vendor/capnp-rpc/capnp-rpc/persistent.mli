[@@@ocaml.warning "-27-32-37-60"]

type ro = Capnp.Message.ro
type rw = Capnp.Message.rw

module type S = sig
  module MessageWrapper : Capnp.RPC.S
  type 'cap message_t = 'cap MessageWrapper.Message.t
  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t
  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t


  module Reader : sig
    type array_t
    type builder_array_t
    type pointer_t = ro MessageWrapper.Slice.t option
    val of_pointer : pointer_t -> 'a reader_t
    module Persistent : sig
      type t = [`Persistent_c8cb212fcd9f5691]
      module SaveParams : sig
        type struct_t = [`SaveParams_f76fba59183073a5]
        type t = struct_t reader_t
        val seal_for_get : t -> pointer_t
        val seal_for_get_interface : t -> 'a MessageWrapper.Capability.t option
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module SaveResults : sig
        type struct_t = [`SaveResults_b76848c18c40efbf]
        type t = struct_t reader_t
        val sturdy_ref_get : t -> pointer_t
        val sturdy_ref_get_interface : t -> 'a MessageWrapper.Capability.t option
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      module Save : sig
      end
    end
  end

  module Builder : sig
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t
    module Persistent : sig
      type t = [`Persistent_c8cb212fcd9f5691]
      module SaveParams : sig
        type struct_t = [`SaveParams_f76fba59183073a5]
        type t = struct_t builder_t
        val seal_for_get : t -> pointer_t
        val seal_for_get_interface : t -> 'a MessageWrapper.Capability.t option
        val seal_for_set : t -> pointer_t -> pointer_t
        val seal_for_set_reader : t -> Reader.pointer_t -> pointer_t
        val seal_for_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module SaveResults : sig
        type struct_t = [`SaveResults_b76848c18c40efbf]
        type t = struct_t builder_t
        val sturdy_ref_get : t -> pointer_t
        val sturdy_ref_get_interface : t -> 'a MessageWrapper.Capability.t option
        val sturdy_ref_set : t -> pointer_t -> pointer_t
        val sturdy_ref_set_reader : t -> Reader.pointer_t -> pointer_t
        val sturdy_ref_set_interface : t -> 'a MessageWrapper.Capability.t option -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      module Save : sig
      end
    end
  end
end

module MakeRPC(MessageWrapper : Capnp.RPC.S) : sig
  include S with module MessageWrapper = MessageWrapper

  module Client : sig
    module Persistent : sig
      type t = [`Persistent_c8cb212fcd9f5691]
      val interface_id : Stdint.Uint64.t
      module Save : sig
        module Params = Builder.Persistent.SaveParams
        module Results = Reader.Persistent.SaveResults
        val method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t
      end
    end
  end

  module Service : sig
    module Persistent : sig
      type t = [`Persistent_c8cb212fcd9f5691]
      val interface_id : Stdint.Uint64.t
      module Save : sig
        module Params = Reader.Persistent.SaveParams
        module Results = Builder.Persistent.SaveResults
      end
      class virtual service : object
        inherit MessageWrapper.Untyped.generic_service
        method virtual save_impl : (Save.Params.t, Save.Results.t) MessageWrapper.Service.method_t
      end
      val local : #service -> t MessageWrapper.Capability.t
    end
  end
end

module Make(M : Capnp.MessageSig.S) : module type of MakeRPC(Capnp.RPC.None(M))
