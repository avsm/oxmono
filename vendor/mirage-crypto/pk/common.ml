let rec until p f = let r = f () in if p r then r else until p f

let guard : _ @ portable = fun p err -> if p then Ok () else Error err

let ( let* ) = Result.bind
