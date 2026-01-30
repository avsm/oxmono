open Core
open Incr_dom

module Format : sig
  (** [Time_ago] generates a user-friendly string representing the time difference (e.g.
      1h 30s turns into "1 hour ago"), while [Short_string_ago] and [Short_string_until]
      are equivalent to [Time_ns.Span.to_short_string] *)
  type t =
    | Time_ago
    | Short_string_ago
    | Short_string_until
    | Custom of (now:Time_ns.t -> reference:Time_ns.t -> string)
end

(** Time-ago-widget creates a time ago text field, that displays the time remaining to, or
    the time elapsed from a particular reference time. This read-friendly string is
    automatically updated. When the reference time in the model changes, the widget
    automatically updates. *)

val view : ?format:Format.t -> Time_ns.t -> Vdom.Node.t
