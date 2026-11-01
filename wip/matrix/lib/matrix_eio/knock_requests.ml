type t = Matrix_client.Knock_requests.t

let list = Matrix_client.Knock_requests.list
let all = Matrix_client.Knock_requests.all

let mark_seen store request =
  Error.unwrap ~context:"marking knock request seen"
    (Matrix_client.Knock_requests.mark_seen store request)

let accept client request =
  Error.unwrap ~context:"accepting knock request"
    (Matrix_client.Knock_requests.accept (Client.base client) request)

let decline client request ?reason () =
  Error.unwrap ~context:"declining knock request"
    (Matrix_client.Knock_requests.decline (Client.base client) request ?reason
       ())

let decline_and_ban client request ?reason () =
  Error.unwrap ~context:"declining and banning knock request"
    (Matrix_client.Knock_requests.decline_and_ban (Client.base client) request
       ?reason ())
