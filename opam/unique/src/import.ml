type nonrec 'a or_null = 'a or_null [@@or_null_reexport]

let failwith = Basement.Stdlib_shim.failwith
