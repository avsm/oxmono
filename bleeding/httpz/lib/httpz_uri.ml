include Uriz

module Scanner = Uriz_scanner

type host_kind = [ `Reg_name | `Ipv4 | `Ipv6 | `Ipvfuture ]
let host_kind t =
  match Uriz.host_kind t with
  | `None -> Null
  | (`Reg_name | `Ipv4 | `Ipv6 | `Ipvfuture) as kind -> This kind

type component =
  [ `Userinfo | `Host | `Path | `Path_segment | `Query | `Query_value | `Fragment | `Unreserved ]
let percent_encode ~component s =
  let component = match component with
    | `Path_segment -> `Segment
    | (`Userinfo | `Host | `Path | `Query | `Query_value | `Fragment | `Unreserved) as c -> c
  in
  pct_encode ~component s

let percent_decode = pct_decode
let encoded_userinfo = userinfo
let encoded_host = host
let encoded_path = path
let encoded_query = query
let encoded_fragment = fragment
let decoded_path = path_decoded
let decoded_fragment = fragment_decoded
let decoded_userinfo = userinfo_decoded
let iter_query_params = query_iter
let find_query_param = find_query

let make_encoded = make
let make_encoded__local = make__local
let with_encoded_userinfo = with_userinfo
let with_encoded_userinfo__local = with_userinfo__local
let with_encoded_host = with_host
let with_encoded_host__local = with_host__local
let with_encoded_path = with_path
let with_encoded_path__local = with_path__local
let with_encoded_query = with_query
let with_encoded_query__local = with_query__local
let with_encoded_fragment = with_fragment
let with_encoded_fragment__local = with_fragment__local

module Raw = Uriz.Raw
