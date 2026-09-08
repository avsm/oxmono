type state = Matrix_client.Thread_paginator.state =
  | Start
  | Loading
  | Next of string
  | End
  | Failed of Matrix_client.Error.t

type t = Matrix_client.Thread_paginator.t

let create ~client ~room_id () =
  Matrix_client.Thread_paginator.create ~client:(Client.base client) ~room_id ()

let set_filter = Matrix_client.Thread_paginator.set_filter
let reset = Matrix_client.Thread_paginator.reset
let state = Matrix_client.Thread_paginator.state
let roots = Matrix_client.Thread_paginator.roots
let loaded_pages = Matrix_client.Thread_paginator.loaded_pages
let is_at_last_page = Matrix_client.Thread_paginator.is_at_last_page
let subscribe = Matrix_client.Thread_paginator.subscribe

let next_page t ?limit () =
  Error.unwrap ~context:"loading next thread page"
    (Matrix_client.Thread_paginator.next_page t ?limit ())
