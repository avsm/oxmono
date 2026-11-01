type t = { name : string; papers : int }
val suggest : ?limit:int -> 'a Db.t -> text:string -> t list
