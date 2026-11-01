module R = Matrix_client.Retention

type policy = R.policy
type lifetime_limits = R.lifetime_limits
type limits = R.limits
type configuration = R.configuration

let get_configuration client =
  Error.unwrap ~context:"getting retention configuration"
    (R.get_configuration (Client.base client))

let get_room_policy client ~room_id =
  Error.unwrap ~context:"getting room retention policy"
    (R.get_room_policy (Client.base client) ~room_id)

let set_room_policy client ~room_id policy =
  Error.unwrap ~context:"setting room retention policy"
    (R.set_room_policy (Client.base client) ~room_id policy)

let effective client ~room_id =
  Error.unwrap ~context:"getting effective retention policy"
    (R.effective (Client.base client) ~room_id)
