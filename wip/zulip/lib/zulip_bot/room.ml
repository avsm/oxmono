type id = Channel of Zulip.Id.Channel.t | Direct of Zulip.Id.Recipient.t

type t = {
  context : Context.t;
  destination : Zulip.Message.destination;
  id : id;
}

let of_destination context destination =
  let id =
    match destination with
    | Zulip.Message.Channel { channel_id; _ } -> Channel channel_id
    | Zulip.Message.Direct { recipient_id; _ } -> Direct recipient_id
  in
  { context; destination; id }

let id t = t.id
let destination t = t.destination

let key = function
  | { id = Channel id; _ } ->
      "channel:" ^ string_of_int (Zulip.Id.Channel.to_int id)
  | { id = Direct id; _ } ->
      "direct:" ^ string_of_int (Zulip.Id.Recipient.to_int id)

let topic t =
  match t.destination with
  | Zulip.Message.Channel { topic; _ } -> Some topic
  | Direct _ -> None

let is_direct t = match t.id with Direct _ -> true | Channel _ -> false

let participants t =
  match t.destination with
  | Zulip.Message.Direct { participants; _ } -> participants
  | Channel _ -> []

let send_text t content =
  Context.enqueue t.context ~destination:t.destination ~content

let compare_id a b =
  match (a, b) with
  | Channel a, Channel b -> Zulip.Id.Channel.compare a b
  | Direct a, Direct b -> Zulip.Id.Recipient.compare a b
  | Channel _, Direct _ -> -1
  | Direct _, Channel _ -> 1

let equal_id a b = compare_id a b = 0

let pp_id ppf = function
  | Channel id -> Format.fprintf ppf "channel:%a" Zulip.Id.Channel.pp id
  | Direct id -> Format.fprintf ppf "direct:%a" Zulip.Id.Recipient.pp id
