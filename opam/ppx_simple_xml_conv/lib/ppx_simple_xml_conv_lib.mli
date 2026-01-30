open! Core

(** This is the runtime library for ppx_simple_xml_conv. *)

(** Re-exposes the module [Simple_xml] from [Ppx_simple_xml_conv_lib] to make sure there
    are no name conflicts. *)
module Xml = Simple_xml

module Of_xml : sig
  module Namespace : sig
    type t =
      | Do_not_care
      | Assert_no_namespace
      | Assert_equals of string
    [@@deriving sexp_of, compare]

    val namespace_matches : t -> string -> bool
  end

  (** This is a element parser that specifies both the tag, and how to parse every
      element. *)
  type 'a element =
    { tag : string
    ; namespace : Namespace.t
    ; parse : ?path_rev:Xml.Tag.t list @ local -> Xml.element -> 'a
    }

  (** This defines a parser for a data type from an XML element. The parser can be for
      just one tag, but can also be for a collection of tags. *)
  type 'a t =
    | Element of 'a element
    | Variant of 'a element list lazy_t

  module Element_count : sig
    type (_, _) t =
      | Required : ('a, 'a) t
      | Option : ('a, 'a option) t
      | List : ('a, 'a list) t
      | Default : 'a -> ('a, 'a) t
  end

  module Attribute_count : sig
    type (_, _) t =
      | Required : ('a, 'a) t
      | Option : ('a, 'a option) t
      | Default : 'a -> ('a, 'a) t
  end

  (** This is used by the PPX to generate the parser for variants, where the generated
      code for:
      {v
    type t =
      | A of A.t
      | B of B.t
    [@@deriving of_xml]
      v}
      Would look something like this:
      {v
    flatten_variants
      [ Parser_and_constructor { parse = A.t_of_xml_description; construct = (fun x -> A x) }
      ; Parser_and_constructor { parse = B.t_of_xml_description; construct = (fun x -> B x) }
      ]
      v} *)
  type 'output parser_and_constructor =
    | Parser_and_constructor :
        { parse : 'a t
        ; construct : 'a -> 'output
        }
        -> 'output parser_and_constructor

  module Element_container : sig
    type 'a t
  end

  type 'a inlined =
    ?path_rev:Xml.Tag.t list @ local
    -> Xml.element Element_container.t
    -> Xml.Attribute.t list
    -> 'a * Xml.element Element_container.t * Xml.Attribute.t list

  val parse_failure
    :  ?path_rev:Xml.Tag.t list @ local
    -> (('a, unit, string, 'b) format4 -> 'a)

  val flatten_variants : 'a parser_and_constructor list -> 'a t

  val check_no_extra_children
    :  ?path_rev:Xml.Tag.t list @ local
    -> Xml.element Element_container.t
    -> unit

  val check_no_extra_attributes
    :  ?path_rev:Xml.Tag.t list @ local
    -> Xml.Attribute.t list
    -> unit

  val elements_only : Xml.t list -> Xml.element Element_container.t

  val leaf
    :  ?ignore_attributes:bool
    -> ?preserve_space:bool
    -> ?namespace:Namespace.t
    -> string
    -> of_string:(string -> 'a)
    -> 'a t

  val empty_element
    :  ?ignore_attributes:bool
    -> ?ignore_children:bool
    -> ?namespace:Namespace.t
    -> string
    -> unit t

  val extract_text
    :  ?path_rev:Xml.Tag.t list @ local
    -> ?preserve_space:bool
    -> tag:string
    -> Xml.t list
    -> string

  (** Extracts the required elements from the given element list that fit the given
      parser, and returns the unused elements. The number of matching elements must
      respect the given element count, and the parser would fail otherwise. *)
  val element
    :  ?path_rev:Xml.Tag.t list @ local
    -> ('input, 'output) Element_count.t
    -> Xml.element Element_container.t
    -> 'input t
    -> 'output * Xml.element Element_container.t

  (** Extracts the required attribute from the given attribute list that fit the given
      key, and returns the unused attributes. The number of matching attributes (0 or 1)
      must respect the given attribute count, and the parser would fail otherwise. *)
  val attribute
    :  ?path_rev:Xml.Tag.t list @ local
    -> ('input, 'output) Attribute_count.t
    -> Xml.Attribute.t list
    -> of_string:(string -> 'input)
    -> namespace:Namespace.t
    -> key:string
    -> 'output * Xml.Attribute.t list

  val parse : ?path_rev:Xml.Tag.t list @ local -> 'a t -> Xml.element -> 'a

  val override_parse
    :  'a t
    -> f:
         (namespace:Namespace.t
          -> tag:string
          -> (?path_rev:Xml.Tag.t list @ local -> Xml.element -> 'a)
          -> (?path_rev:Xml.Tag.t list @ local -> Xml.element -> 'b))
    -> 'b t

  include module type of
      Ppx_simple_xml_conv_lib_intf.Signatures.Of_xml
        (struct
          type nonrec 'a t = 'a t
        end)
        (struct
          type 'a t = 'a inlined
        end)

  module Of_xmlable
      (M : S)
      (Target : sig
         type t

         val of_xmlable : M.t -> t
       end) : S with type t := Target.t

  module Of_xmlable1
      (M : S1)
      (Target : sig
         type 'a t

         val of_xmlable : 'a M.t -> 'a t
       end) : S1 with type 'a t := 'a Target.t

  module Of_xmlable2
      (M : S2)
      (Target : sig
         type ('a, 'b) t

         val of_xmlable : ('a, 'b) M.t -> ('a, 'b) t
       end) : S2 with type ('a, 'b) t := ('a, 'b) Target.t

  module Inlined : sig
    include module type of Inlined

    module Of_xmlable
        (M : S)
        (Target : sig
           type t

           val of_xmlable : M.t -> t
         end) : S with type t := Target.t

    module Of_xmlable1
        (M : S1)
        (Target : sig
           type 'a t

           val of_xmlable : 'a M.t -> 'a t
         end) : S1 with type 'a t := 'a Target.t

    module Of_xmlable2
        (M : S2)
        (Target : sig
           type ('a, 'b) t

           val of_xmlable : ('a, 'b) M.t -> ('a, 'b) t
         end) : S2 with type ('a, 'b) t := ('a, 'b) Target.t
  end
end

module To_xml : sig
  module Builder : sig
    val prepend : 'a -> convert:('a -> 'b) -> 'b list -> 'b list
    val prepend_opt : 'a option -> convert:('a -> 'b) -> 'b list -> 'b list
    val prepend_list : 'a list -> convert:('a -> 'b) -> 'b list -> 'b list
    val xmlns_namespace : string
  end

  type 'a inlined = 'a -> Xml.t list * Xml.Attribute.t list

  include module type of Ppx_simple_xml_conv_lib_intf.Signatures.To_xml (struct
      type 'a t = 'a inlined
    end)

  module Of_xmlable
      (M : S)
      (Target : sig
         type t

         val to_xmlable : t -> M.t
       end) : S with type t := Target.t

  module Of_xmlable1
      (M : S1)
      (Target : sig
         type 'a t

         val to_xmlable : 'a t -> 'a M.t
       end) : S1 with type 'a t := 'a Target.t

  module Of_xmlable2
      (M : S2)
      (Target : sig
         type ('a, 'b) t

         val to_xmlable : ('a, 'b) t -> ('a, 'b) M.t
       end) : S2 with type ('a, 'b) t := ('a, 'b) Target.t

  module Inlined : sig
    include module type of Inlined

    module Of_xmlable
        (M : S)
        (Target : sig
           type t

           val to_xmlable : t -> M.t
         end) : S with type t := Target.t

    module Of_xmlable1
        (M : S1)
        (Target : sig
           type 'a t

           val to_xmlable : 'a t -> 'a M.t
         end) : S1 with type 'a t := 'a Target.t

    module Of_xmlable2
        (M : S2)
        (Target : sig
           type ('a, 'b) t

           val to_xmlable : ('a, 'b) t -> ('a, 'b) M.t
         end) : S2 with type ('a, 'b) t := ('a, 'b) Target.t
  end
end

include module type of
    Ppx_simple_xml_conv_lib_intf.Signatures.Make
      (Of_xml)
      (struct
        type 'a t = 'a Of_xml.inlined
      end)
      (struct
        type 'a t = 'a To_xml.inlined
      end)

module Of_xmlable
    (M : S)
    (Target : sig
       type t

       val to_xmlable : t -> M.t
       val of_xmlable : M.t -> t
     end) : S with type t := Target.t

module Of_xmlable1
    (M : S1)
    (Target : sig
       type 'a t

       val to_xmlable : 'a t -> 'a M.t
       val of_xmlable : 'a M.t -> 'a t
     end) : S1 with type 'a t := 'a Target.t

module Of_xmlable2
    (M : S2)
    (Target : sig
       type ('a, 'b) t

       val to_xmlable : ('a, 'b) t -> ('a, 'b) M.t
       val of_xmlable : ('a, 'b) M.t -> ('a, 'b) t
     end) : S2 with type ('a, 'b) t := ('a, 'b) Target.t

module Inlined : sig
  include module type of Inlined

  module Of_xmlable
      (M : S)
      (Target : sig
         type t

         val to_xmlable : t -> M.t
         val of_xmlable : M.t -> t
       end) : S with type t := Target.t

  module Of_xmlable1
      (M : S1)
      (Target : sig
         type 'a t

         val to_xmlable : 'a t -> 'a M.t
         val of_xmlable : 'a M.t -> 'a t
       end) : S1 with type 'a t := 'a Target.t

  module Of_xmlable2
      (M : S2)
      (Target : sig
         type ('a, 'b) t

         val to_xmlable : ('a, 'b) t -> ('a, 'b) M.t
         val of_xmlable : ('a, 'b) M.t -> ('a, 'b) t
       end) : S2 with type ('a, 'b) t := ('a, 'b) Target.t
end

(** This module defines the primitives that might be needed to derive [xml], including
    some string primitives not provided by [Core]. *)
module Primitives : sig
  val string_of_string : string -> string
  val string_to_string : string -> string
  val int_to_string : int -> string
  val float_to_string : float -> string

  val result_of_xml_description
    :  'ok Of_xml.t
    -> 'error Of_xml.t
    -> ('ok, 'error) result Of_xml.t

  val result_of_xml
    :  'ok Of_xml.t
    -> 'error Of_xml.t
    -> Xml.element
    -> ('ok, 'error) result

  val xml_of_result
    :  ('ok -> Xml.element)
    -> ('error -> Xml.element)
    -> ('ok, 'error) result
    -> Xml.element
end
