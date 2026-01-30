open! Core

(* Use /proc/PID/exe to get the currently running executable.
   - argv[0] might have been deleted
   - `cp /proc/PID/exe dst` works as expected while `cp /proc/self/exe dst` does not *)
let the_path = ref (lazy [%string "/proc/%{(Core_unix.getpid ())#Pid}/exe"])
let get_path () = Lazy.force !the_path

module Expert = struct
  let set_path_getter get_path = the_path := lazy (get_path ())
  let set_path path = set_path_getter (fun () -> path)
end
