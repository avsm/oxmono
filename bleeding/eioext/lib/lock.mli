exception Busy of string

val with_lock :
  ?wait:(unit -> unit) ->
  device:int64 ->
  inode:int64 ->
  name:string ->
  (unit -> 'a) ->
  'a
