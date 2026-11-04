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

    module Persistent = struct
      type t = [`Persistent_c8cb212fcd9f5691]
      module SaveParams = struct
        type struct_t = [`SaveParams_f76fba59183073a5]
        type t = struct_t reader_t
        let seal_for_get x =
          RA_.get_pointer x 0
        let seal_for_get_interface x =
          RA_.get_interface x 0
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module SaveResults = struct
        type struct_t = [`SaveResults_b76848c18c40efbf]
        type t = struct_t reader_t
        let sturdy_ref_get x =
          RA_.get_pointer x 0
        let sturdy_ref_get_interface x =
          RA_.get_interface x 0
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      module Save = struct
      end
    end
  end

  module Builder = struct
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t

    module Persistent = struct
      type t = [`Persistent_c8cb212fcd9f5691]
      module SaveParams = struct
        type struct_t = [`SaveParams_f76fba59183073a5]
        type t = struct_t builder_t
        let seal_for_get x =
          BA_.get_pointer x 0
        let seal_for_get_interface x =
          BA_.get_interface x 0
        let seal_for_set x v =
          BA_.set_pointer x 0 (Some v)
        let seal_for_set_reader x v =
          BA_.set_pointer x 0 v
        let seal_for_set_interface x v =
          BA_.set_interface x 0 v
        let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
        let init_pointer ptr =
          BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
      end
      module SaveResults = struct
        type struct_t = [`SaveResults_b76848c18c40efbf]
        type t = struct_t builder_t
        let sturdy_ref_get x =
          BA_.get_pointer x 0
        let sturdy_ref_get_interface x =
          BA_.get_interface x 0
        let sturdy_ref_set x v =
          BA_.set_pointer x 0 (Some v)
        let sturdy_ref_set_reader x v =
          BA_.set_pointer x 0 v
        let sturdy_ref_set_interface x v =
          BA_.set_interface x 0 v
        let of_message x = BA_.get_root_struct ~data_words:0 ~pointer_words:1 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:0 ~pointer_words:1 ()
        let init_pointer ptr =
          BA_.init_struct_pointer ptr ~data_words:0 ~pointer_words:1
      end
      module Save = struct
      end
    end
  end

  module Client = struct
    module Persistent = struct
      type t = [`Persistent_c8cb212fcd9f5691]
      let interface_id = Stdint.Uint64.of_string "0xc8cb212fcd9f5691"
      module Save = struct
        module Params = Builder.Persistent.SaveParams
        module Results = Reader.Persistent.SaveResults
        let method_id : (t, Params.t, Results.t) Capnp.RPC.MethodID.t =
          Capnp.RPC.MethodID.v ~interface_id ~method_id:0
      end
      let method_name = function
        | 0 -> Some "save"
        | _ -> None
      let () = Capnp.RPC.Registry.register ~interface_id ~name:"Persistent" method_name
    end
  end

  module Service = struct
    module Persistent = struct
      type t = [`Persistent_c8cb212fcd9f5691]
      let interface_id = Stdint.Uint64.of_string "0xc8cb212fcd9f5691"
      module Save = struct
        module Params = Reader.Persistent.SaveParams
        module Results = Builder.Persistent.SaveResults
      end
      class virtual service = object (self)
        method release = ()
        method dispatch ~interface_id:i ~method_id =
          if i <> interface_id then MessageWrapper.Untyped.unknown_interface ~interface_id:i
          else match method_id with
          | 0 -> MessageWrapper.Untyped.abstract_method self#save_impl
          | x -> MessageWrapper.Untyped.unknown_method ~interface_id ~method_id
        method pp f = Format.pp_print_string f "Persistent"
        method virtual save_impl : (Save.Params.t, Save.Results.t) MessageWrapper.Service.method_t
      end
      let local (service:#service) =
        MessageWrapper.Untyped.local service
    end
  end
  module MessageWrapper = MessageWrapper
end

module Make(M:Capnp.MessageSig.S) = MakeRPC(Capnp.RPC.None(M))
