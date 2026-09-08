(** A deterministic space graph projected from cached room state.

    A relationship is accepted only when a joined parent has a non-empty
    [m.space.child] link and the joined child has the corresponding non-empty
    [m.space.parent] link. A joined child whose only claimed parents are left,
    unknown or one-sided is therefore a root. *)

type t

val of_state : Base_client.state -> t
(** [of_state state] projects the joined rooms in [state]. Candidate edges are
    considered in [(parent room id, child room id)] order and one which would
    create a cycle is dropped. The result is therefore a deterministic DAG even
    when malicious or stale state contains a cycle. *)

val parents : t -> Matrix_proto.Id.Room_id.t -> Matrix_proto.Id.Room_id.t list
(** [parents graph room] is every accepted joined parent of [room], ordered by
    room ID. *)

val children : t -> Matrix_proto.Id.Room_id.t -> Matrix_proto.Id.Room_id.t list
(** [children graph room] is every accepted joined child. Children with an
    explicit [order] come first; ties use the child event's timestamp and then
    room ID. *)

val roots : t -> Matrix_proto.Id.Room_id.t list
(** [roots graph] is every joined room with no accepted joined parent, ordered
    by room ID. *)

val flattened_subtree :
  t -> Matrix_proto.Id.Room_id.t -> Matrix_proto.Id.Room_id.t list
(** [flattened_subtree graph room] is a depth-first pre-order beginning with
    [room] and following {!children}. A room reachable by multiple paths occurs
    once. An unknown or non-joined [room] gives [[]]. *)
