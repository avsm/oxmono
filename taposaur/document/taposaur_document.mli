(** Bounded readers for individual document formats. All metadata readers
    return the format-independent [Taposaur.Bibliography.t]. *)
module Tei = Tei
module Elsevier = Elsevier
module S2orc = S2orc
