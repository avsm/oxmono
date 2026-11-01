@@ portable

(** JSON objects read as association lists.

    Several wire shapes are objects whose member names are data rather than a
    fixed vocabulary, such as a power-level table or a MAC per key id. *)

val jsont : 'a Jsont.t -> (string * 'a) list Jsont.t
(** [jsont v] reads an object whose members are read with [v] into an
    association list. Bindings come out sorted by name however the sender
    ordered them. *)
