(** Execution and logs for one independently cancellable workflow. *)

type t = {
  job : Job.t;
  mutable status : string;
  mutable started : string option;
  mutable finished : string option;
  mutable error : string option;
  mutable events : Jsont.json list;
  mutable log_bytes : int;
  mutable cancel : (unit -> unit) option;
  mutable cancelled : bool;
}

type env = {
  system : Eio_unix.Stdenv.base;
  directory : Eio.Fs.dir_ty Eio.Path.t;
  slots : Eio.Semaphore.t;
}

type input = {
  id : string;
  repo : string;
  source : string;
  commit : string;
  metadata : Jsont.json;
}

val now : unit -> string
val optional : string -> ('a -> 'b) -> 'a option -> (string * 'b) list
val terminal : t -> bool
val v : Job.t -> t
val view : t -> Jsont.json
val snapshot : t -> Jsont.json
val restore : Jsont.json -> t

val execute : env -> input -> persist:(unit -> unit) -> t -> unit
(** [execute env input ~persist workflow] waits for a worker slot, checks out
    the source and executes trusted steps. Each transition calls [persist]. *)

val cancel : persist:(unit -> unit) -> t -> unit
