val run :
  Support.proc ->
  clock:D10.Config.clk ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  Runner.config ->
  owner:string ->
  prepared:bool ->
  profile:string ->
  output:string ->
  unit
(** [run proc ... ~owner ~output] builds an isolated owner workspace and writes
    a relocatable Dune installation and inventories to a new directory.
    [prepared] uses the caller's existing compiler environment; otherwise
    dependencies come from the locked day10 configuration. *)
