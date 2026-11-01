@@ portable

(** Shapes shared by many client-server endpoints.

    These are the request and response fragments that recur across the API and
    have no event type of their own. Each is paired with a [Jsont.t] named
    [jsont] in the module that owns it. *)

module Direction : sig
  (** The direction a paginated endpoint walks the timeline in. *)

  type t =
    | Forward  (** Towards the present. *)
    | Backward  (** Towards the start of the room. *)

  val to_string : t -> string
  (** [to_string t] is ["f"] or ["b"], the form the [dir] query parameter takes.
  *)

  val of_string : string -> (t, [> `Msg of string ]) result
  (** [of_string s] is the direction ["f"] or ["b"] denotes. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same direction. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Visibility : sig
  (** Whether a room appears in the server's public room directory. *)

  type t = Public | Private

  val to_string : t -> string
  (** [to_string t] is ["public"] or ["private"]. *)

  val of_string : string -> (t, [> `Msg of string ]) result
  (** [of_string s] is the visibility [s] denotes. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same visibility. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Page : sig
  (** One batch of a paginated result, with the tokens that reach the batches
      either side of it.

      The endpoints spell these members differently. [/messages] uses [chunk],
      [start] and [end], the relations endpoints use [chunk], [next_batch] and
      [prev_batch], so {!jsont} takes the names as arguments. *)

  type 'a t = {
    chunk : 'a list;  (** The items, in the order the server returned them. *)
    next_batch : string option;
        (** Token for the batch after this one. Absent means there is none. *)
    prev_batch : string option;
        (** Token for the batch before this one. Absent means there is none. *)
  }

  val v : ?next_batch:string -> ?prev_batch:string -> 'a list -> 'a t
  (** [v chunk] is a page holding [chunk]. [next_batch] and [prev_batch] default
      to absent. *)

  val jsont :
    ?chunk:string ->
    ?next_batch:string ->
    ?prev_batch:string ->
    'a Jsont.t ->
    'a t Jsont.t
  (** [jsont item] is the codec for a page of values read with [item]. [chunk]
      names the member holding the items and defaults to ["chunk"]. [next_batch]
      names the forward token and defaults to ["next_batch"]. [prev_batch] names
      the backward token and defaults to ["prev_batch"]. An absent [chunk]
      member decodes to the empty list. *)
end
