module J = Mail_json
module U = Fetch.Middleware.Url

let url s = match U.of_string s with Ok u -> u | Error _ -> J.invalid ()

let patch args =
  J.only [ "accountId"; "ifInState"; "update" ] args;
  ignore (J.string (J.get "ifInState" args));
  match J.fields (J.get "update" args) with
  | [ (id, changes) ] ->
      ignore (J.id id);
      let entries =
        match Jmap.Proto.Patch.of_json changes with
        | Ok p -> Jmap.Proto.Patch.to_list p
        | Error _ -> J.invalid ()
      in
      entries <> []
      && List.length entries <= 100
      && List.for_all
           (fun (key, value) ->
             String.starts_with ~prefix:"mailboxIds/" key
             && String.length key > 11
             && (not
                   (String.contains
                      (String.sub key 11 (String.length key - 11))
                      '/'))
             &&
             match value with
             | None | Some (Jsont.Bool (true, _)) -> true
             | _ -> false)
           entries
  | _ -> false

let allowed ~writable ~account body =
  try
    if String.length body > 65536 then false
    else
      let request = Codec.decode_exn ~max_depth:32 Jsont.json body in
      J.only [ "using"; "methodCalls" ] request;
      let using = List.map J.string (J.array (J.get "using" request)) in
      if
        List.sort_uniq String.compare using
        <> List.sort String.compare
             [ Jmap.Proto.Capability.core; Jmap.Proto.Capability.mail ]
      then false
      else
        match J.array (J.get "methodCalls" request) with
        | [
         Jsont.Array ([ Jsont.String (name, _); args; Jsont.String (_, _) ], _);
        ] -> (
            J.string (J.get "accountId" args) = account
            &&
            match name with
            | "Email/get" ->
                J.only
                  [
                    "accountId";
                    "ids";
                    "properties";
                    "fetchTextBodyValues";
                    "fetchHTMLBodyValues";
                    "maxBodyValueBytes";
                  ]
                  args;
                let ids = J.ids (J.get "ids" args) in
                List.length ids <= 10
            | "Thread/get" ->
                J.only [ "accountId"; "ids" ] args;
                List.length (J.ids (J.get "ids" args)) = 1
            | "Mailbox/get" ->
                J.only [ "accountId"; "ids"; "properties" ] args;
                true
            | "Email/query" ->
                J.only
                  [
                    "accountId";
                    "filter";
                    "sort";
                    "position";
                    "limit";
                    "calculateTotal";
                    "collapseThreads";
                  ]
                  args;
                let n = J.int (J.get "limit" args) in
                n > 0 && n <= 50
            | "Email/changes" ->
                J.only [ "accountId"; "sinceState"; "maxChanges" ] args;
                ignore (J.string (J.get "sinceState" args));
                let n = J.int (J.get "maxChanges" args) in
                n > 0 && n <= 1000
            | "Email/set" -> writable && patch args
            | _ -> false)
        | _ -> false
  with _ -> false

let create ~url:session_url ~writable fetch =
  let base = url session_url in
  if U.scheme base <> `Https || U.has_query base || U.has_fragment base then
    invalid_arg
      "Mail requires an HTTPS session URL without a query or fragment.";
  let session_url = U.to_string base in
  let destination = ref None in
  let bind ~account ~api_url =
    let api =
      match U.resolve ~base api_url with
      | Ok api -> api
      | Error _ ->
          invalid_arg "The JMAP session advertised an invalid mail API URL."
    in
    if U.scheme api <> `Https then
      invalid_arg "The JMAP session advertised a non-HTTPS mail API URL.";
    if U.has_fragment api then
      invalid_arg "The JMAP session advertised a mail API URL with a fragment.";
    let value = (J.id account, U.to_string api) in
    match !destination with
    | Some previous when previous <> value ->
        invalid_arg "JMAP mail binding changed."
    | _ -> destination := Some value
  in
  let filter (r : Fetch.Middleware.request) =
    let headers =
      Http.Header.to_list r.headers
      |> List.for_all (fun (name, _) ->
             List.mem
               (String.lowercase_ascii name)
               [ "authorization"; "accept"; "content-type" ])
    in
    let valid =
      match (r.meth, r.body, !destination) with
      | `GET, Empty, _ -> U.to_string r.url = session_url
      | `POST, String body, Some (account, api) ->
          U.to_string r.url = api && allowed ~writable ~account body
      | _ -> false
    in
    if valid && headers && not (U.has_fragment r.url) then `Allow
    else `Reject "JMAP mail capability rejected the request"
  in
  (* The authenticated session may advertise an API on another HTTPS origin.
     The filter pins its complete URL, including its query, and applies to
     every request, including redirect hops. *)
  (Fetch.restrict ~methods:[ `GET; `POST ] ~filter fetch, bind)
