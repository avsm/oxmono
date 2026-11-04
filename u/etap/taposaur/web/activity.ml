type entry = {
  seq : int; time : float; page : string; query : (string * string) list;
  status : int; elapsed_ms : float;
}
type t = { capacity : int; mutable entries : entry list; mutable next : int }
let create ?(capacity = 500) () =
  if capacity < 1 then invalid_arg "capacity must be positive";
  { capacity; entries = []; next = 1 }
let record t ~time ~page ~query ~status ~elapsed_ms =
  let e = { seq = t.next; time; page; query; status; elapsed_ms } in
  t.next <- t.next + 1;
  t.entries <- List.filteri (fun i _ -> i < t.capacity) (e :: t.entries)
let since t seq = List.rev (List.filter (fun e -> e.seq > seq) t.entries)
let cursor t = t.next - 1
