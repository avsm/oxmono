exception Busy of string

type entry = { held : bool Atomic.t; mutable users : int }

let mutex = Mutex.create ()
let registry = Hashtbl.create 16

let synchronized f =
  Mutex.lock mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock mutex) f

let with_lock ?wait ~device ~inode ~name f =
  let key = (device, inode, name) in
  let entry =
    synchronized (fun () ->
        let entry =
          match Hashtbl.find_opt registry key with
          | Some entry -> entry
          | None ->
              let entry = { held = Atomic.make false; users = 0 } in
              Hashtbl.add registry key entry;
              entry
        in
        entry.users <- entry.users + 1;
        entry)
  in
  Fun.protect
    ~finally:(fun () ->
      synchronized (fun () ->
          entry.users <- entry.users - 1;
          if entry.users = 0 then Hashtbl.remove registry key))
    (fun () ->
      let rec acquire () =
        if not (Atomic.compare_and_set entry.held false true) then (
          (match wait with Some wait -> wait () | None -> raise (Busy name));
          acquire ())
      in
      acquire ();
      Fun.protect ~finally:(fun () -> Atomic.set entry.held false) f)
