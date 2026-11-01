let with_lock mutex f = Eio.Mutex.use_rw ~protect:true mutex f

module Value = struct
  type 'a subscription = {
    mutex : Eio.Mutex.t;
    condition : Eio.Condition.t;
    mutable pending : 'a option;
    mutable active : bool;
    mutable detach : unit -> unit;
  }

  type 'a t = {
    mutex : Eio.Mutex.t;
    equal : 'a -> 'a -> bool;
    mutable value : 'a;
    mutable subscribers : 'a subscription list;
  }

  let create ?(equal = ( = )) value =
    { mutex = Eio.Mutex.create (); equal; value; subscribers = [] }

  let get t = Eio.Mutex.use_ro t.mutex (fun () -> t.value)

  let publish_locked t value =
    List.iter
      (fun subscription ->
        if subscription.active then (
          subscription.pending <- Some value;
          Eio.Condition.broadcast subscription.condition))
      t.subscribers

  let set t value =
    with_lock t.mutex (fun () ->
        if not (t.equal t.value value) then (
          t.value <- value;
          publish_locked t value))

  let unsubscribe (subscription : 'a subscription) = subscription.detach ()

  let subscribe ~sw t =
    let subscription =
      {
        mutex = t.mutex;
        condition = Eio.Condition.create ();
        pending = None;
        active = true;
        detach = (fun () -> ());
      }
    in
    subscription.detach <-
      (fun () ->
        with_lock t.mutex (fun () ->
            if subscription.active then (
              subscription.active <- false;
              subscription.pending <- None;
              t.subscribers <-
                List.filter
                  (fun candidate -> candidate != subscription)
                  t.subscribers;
              Eio.Condition.broadcast subscription.condition)));
    let value =
      with_lock t.mutex (fun () ->
          t.subscribers <- subscription :: t.subscribers;
          t.value)
    in
    Eio.Switch.on_release sw (fun () -> unsubscribe subscription);
    (value, subscription)

  let next (subscription : 'a subscription) =
    Eio.Mutex.lock subscription.mutex;
    Fun.protect
      ~finally:(fun () -> Eio.Mutex.unlock subscription.mutex)
      (fun () ->
        let rec await () =
          if not subscription.active then None
          else
            match subscription.pending with
            | Some value ->
                subscription.pending <- None;
                Some value
            | None ->
                Eio.Condition.await subscription.condition subscription.mutex;
                await ()
        in
        await ())
end

module List = struct
  type 'a diff =
    | Insert of { index : int; value : 'a }
    | Remove of { index : int }
    | Set of { index : int; value : 'a }
    | Move of { from : int; to_ : int }
    | Truncate of { length : int }
    | Reset of 'a array

  type 'a subscription = {
    mutex : Eio.Mutex.t;
    condition : Eio.Condition.t;
    mutable pending : 'a diff list;
    mutable active : bool;
    mutable detach : unit -> unit;
  }

  type 'a t = {
    mutex : Eio.Mutex.t;
    max_pending_diffs : int;
    mutable values : 'a array;
    mutable subscribers : 'a subscription list;
  }

  let create ?(max_pending_diffs = 256) values =
    if max_pending_diffs < 1 then invalid_arg "Observable.List.create";
    {
      mutex = Eio.Mutex.create ();
      max_pending_diffs;
      values = Array.of_list values;
      subscribers = [];
    }

  let snapshot t = Eio.Mutex.use_ro t.mutex (fun () -> Array.copy t.values)
  let length t = Eio.Mutex.use_ro t.mutex (fun () -> Array.length t.values)
  let get t index = Eio.Mutex.use_ro t.mutex (fun () -> t.values.(index))

  let publish_locked t diffs =
    if diffs <> [] then
      List.iter
        (fun subscription ->
          if subscription.active then (
            let count = List.length subscription.pending + List.length diffs in
            subscription.pending <-
              (if count > t.max_pending_diffs then
                 [ Reset (Array.copy t.values) ]
               else subscription.pending @ diffs);
            Eio.Condition.broadcast subscription.condition))
        t.subscribers

  let unsubscribe (subscription : 'a subscription) = subscription.detach ()

  let subscribe ~sw t =
    let subscription =
      {
        mutex = t.mutex;
        condition = Eio.Condition.create ();
        pending = [];
        active = true;
        detach = (fun () -> ());
      }
    in
    subscription.detach <-
      (fun () ->
        with_lock t.mutex (fun () ->
            if subscription.active then (
              subscription.active <- false;
              subscription.pending <- [];
              t.subscribers <-
                List.filter
                  (fun candidate -> candidate != subscription)
                  t.subscribers;
              Eio.Condition.broadcast subscription.condition)));
    let values =
      with_lock t.mutex (fun () ->
          t.subscribers <- subscription :: t.subscribers;
          Array.copy t.values)
    in
    Eio.Switch.on_release sw (fun () -> unsubscribe subscription);
    (values, subscription)

  let next (subscription : 'a subscription) =
    Eio.Mutex.lock subscription.mutex;
    Fun.protect
      ~finally:(fun () -> Eio.Mutex.unlock subscription.mutex)
      (fun () ->
        let rec await () =
          if not subscription.active then None
          else
            match subscription.pending with
            | _ :: _ as diffs ->
                subscription.pending <- [];
                Some diffs
            | [] ->
                Eio.Condition.await subscription.condition subscription.mutex;
                await ()
        in
        await ())

  let check_insert values index =
    if index < 0 || index > Array.length values then
      invalid_arg "Observable.List.insert"

  let check_index name values index =
    if index < 0 || index >= Array.length values then invalid_arg name

  let array_insert values index value =
    check_insert values index;
    let length = Array.length values in
    Array.init (length + 1) (fun i ->
        if i < index then values.(i)
        else if i = index then value
        else values.(i - 1))

  let array_remove values index =
    check_index "Observable.List.remove" values index;
    let length = Array.length values in
    Array.init (length - 1) (fun i ->
        if i < index then values.(i) else values.(i + 1))

  let array_move values from to_ =
    check_index "Observable.List.apply(Move)" values from;
    check_index "Observable.List.apply(Move)" values to_;
    if from = to_ then Array.copy values
    else
      let value = values.(from) in
      let without = array_remove values from in
      array_insert without to_ value

  let mutate t f =
    with_lock t.mutex (fun () ->
        let values, diffs = f t.values in
        t.values <- values;
        publish_locked t diffs)

  let insert t ~index value =
    mutate t (fun values ->
        (array_insert values index value, [ Insert { index; value } ]))

  (* The index is read under the same lock as the write, so that a second
     fiber appending at the same time cannot make this one land short. *)
  let append t value =
    mutate t (fun values ->
        let index = Array.length values in
        (array_insert values index value, [ Insert { index; value } ]))

  let remove t ~index =
    mutate t (fun values -> (array_remove values index, [ Remove { index } ]))

  let set t ~index value =
    mutate t (fun values ->
        check_index "Observable.List.set" values index;
        let next = Array.copy values in
        next.(index) <- value;
        (next, [ Set { index; value } ]))

  let apply values = function
    | Insert { index; value } -> array_insert values index value
    | Remove { index } -> array_remove values index
    | Set { index; value } ->
        check_index "Observable.List.apply(Set)" values index;
        let values = Array.copy values in
        values.(index) <- value;
        values
    | Move { from; to_ } -> array_move values from to_
    | Truncate { length } ->
        if length < 0 then invalid_arg "Observable.List.apply(Truncate)";
        if length >= Array.length values then Array.copy values
        else Array.sub values 0 length
    | Reset values -> Array.copy values

  let apply_all values diffs = List.fold_left apply values diffs

  (* Replaying a long sequence of moves copies the array for every move and
     can turn a pathological reorder into quadratic work.  Subscribers already
     support [Reset], so keep granular diffs for ordinary updates but cap the
     number of array-copying changes made by one reconciliation. *)
  let max_reconcile_diffs = 256

  let reconcile_by ?(key_equal = ( = )) ~key ~equal t target =
    let target = Array.of_list target in
    mutate t (fun current ->
        let reset () = (Array.copy target, [ Reset (Array.copy target) ]) in
        let exception Reconcile_reset in
        try
          let values = ref current in
          let diffs = ref [] in
          let diff_count = ref 0 in
          let emit diff =
            incr diff_count;
            if !diff_count > max_reconcile_diffs then raise Reconcile_reset;
            values := apply !values diff;
            diffs := diff :: !diffs
          in
          Array.iteri
            (fun index target_value ->
              if
                index < Array.length !values
                && key_equal (key !values.(index)) (key target_value)
              then (
                if not (equal !values.(index) target_value) then
                  emit (Set { index; value = target_value }))
              else
                let found = ref None in
                let candidate = ref (index + 1) in
                while
                  Option.is_none !found && !candidate < Array.length !values
                do
                  if key_equal (key !values.(!candidate)) (key target_value)
                  then found := Some !candidate;
                  incr candidate
                done;
                match !found with
                | Some from ->
                    emit (Move { from; to_ = index });
                    if not (equal !values.(index) target_value) then
                      emit (Set { index; value = target_value })
                | None -> emit (Insert { index; value = target_value }))
            target;
          let target_length = Array.length target in
          if Array.length !values > target_length then
            emit (Truncate { length = target_length });
          (!values, List.rev !diffs)
        with Reconcile_reset -> reset ())
end
