(** [Poisoned] is the exception raised when trying to access a synchronization or
    communication primitive that has been poisoned. *)
exception Poisoned

(** [Frozen] is the exception raised when trying to access a synchronization or
    communication primitive that has been frozen. *)
exception Frozen

(** [Empty] is the exception raised when the target communication structure was empty. *)
exception Empty

(** [Already_full] is the exception raised when the target communication structure was
    already full. *)
exception Already_full

let () =
  Printexc.Safe.register_printer (function
    | Poisoned -> Some "Poisoned"
    | Frozen -> Some "Frozen"
    | Empty -> Some "Empty"
    | Already_full -> Some "Already_full"
    | _ -> None)
;;
