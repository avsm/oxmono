open! Core

module Signatures = struct
  module Of_xml (Description : T1) (Inlined : T1) = struct
    module type S = sig
      type t

      val t_of_xml_description : t Description.t
      val t_of_xml : Simple_xml.element -> t
    end

    module type S1 = sig
      type 'a t

      val t_of_xml_description : 'a Description.t -> 'a t Description.t
      val t_of_xml : 'a Description.t -> Simple_xml.element -> 'a t
    end

    module type S2 = sig
      type ('a, 'b) t

      val t_of_xml_description
        :  'a Description.t
        -> 'b Description.t
        -> ('a, 'b) t Description.t

      val t_of_xml
        :  'a Description.t
        -> 'b Description.t
        -> Simple_xml.element
        -> ('a, 'b) t
    end

    module Inlined = struct
      module type S = sig
        type t

        val t_of_xml_inlined : t Inlined.t
      end

      module type S1 = sig
        type 'a t

        val t_of_xml_inlined : 'a Description.t -> 'a t Inlined.t
      end

      module type S2 = sig
        type ('a, 'b) t

        val t_of_xml_inlined
          :  'a Description.t
          -> 'b Description.t
          -> ('a, 'b) t Inlined.t
      end
    end
  end

  module To_xml (Inlined : T1) = struct
    module type S = sig
      type t

      val xml_of_t : t -> Simple_xml.element
    end

    module type S1 = sig
      type 'a t

      val xml_of_t : ('a -> Simple_xml.element) -> 'a t -> Simple_xml.element
    end

    module type S2 = sig
      type ('a, 'b) t

      val xml_of_t
        :  ('a -> Simple_xml.element)
        -> ('b -> Simple_xml.element)
        -> ('a, 'b) t
        -> Simple_xml.element
    end

    module Inlined = struct
      module type S = sig
        type t

        val inlined_xml_of_t : t Inlined.t
      end

      module type S1 = sig
        type 'a t

        val inlined_xml_of_t : ('a -> Simple_xml.element) -> 'a t Inlined.t
      end

      module type S2 = sig
        type ('a, 'b) t

        val inlined_xml_of_t
          :  ('a -> Simple_xml.element)
          -> ('b -> Simple_xml.element)
          -> ('a, 'b) t Inlined.t
      end
    end
  end

  module Make (Of_xml_description : T1) (Inlined_of_xml : T1) (Inlined_to_xml : T1) =
  struct
    module type S = sig
      include Of_xml(Of_xml_description)(Inlined_of_xml).S
      include To_xml(Inlined_to_xml).S with type t := t
    end

    module type S1 = sig
      include Of_xml(Of_xml_description)(Inlined_of_xml).S1
      include To_xml(Inlined_to_xml).S1 with type 'a t := 'a t
    end

    module type S2 = sig
      include Of_xml(Of_xml_description)(Inlined_of_xml).S2
      include To_xml(Inlined_to_xml).S2 with type ('a, 'b) t := ('a, 'b) t
    end

    module Inlined = struct
      module type S = sig
        include Of_xml(Of_xml_description)(Inlined_of_xml).Inlined.S
        include To_xml(Inlined_to_xml).Inlined.S with type t := t
      end

      module type S1 = sig
        include Of_xml(Of_xml_description)(Inlined_of_xml).Inlined.S1
        include To_xml(Inlined_to_xml).Inlined.S1 with type 'a t := 'a t
      end

      module type S2 = sig
        include Of_xml(Of_xml_description)(Inlined_of_xml).Inlined.S2
        include To_xml(Inlined_to_xml).Inlined.S2 with type ('a, 'b) t := ('a, 'b) t
      end
    end
  end
end
