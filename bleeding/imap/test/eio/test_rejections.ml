module C = Imap_eio.Client
module S = Imap_eio.Selected
module E = Imap_eio.Error
module R = Imap.Response

let ok = function Ok x -> x | Error e -> failwith (C.error_to_string e)
let contains text fragment =
  let rec loop i =
    i + String.length fragment <= String.length text &&
    (String.sub text i (String.length fragment)=fragment || loop (i+1)) in
  loop 0
let tag n=Printf.sprintf "A%08d" n
let done_ n=tag n ^ " OK done\r\n"
let status_text = function `No -> "NO" | `Bad -> "BAD"
let rejected ?(text="refused") ~status ~code ~tag:expected_tag = function
  | Error (E.Rejected actual as error) ->
      if actual.tag<>expected_tag || actual.status<>status ||
         actual.code<>code || actual.text<>text then
        failwith ("rejection details changed: " ^ C.error_to_string error);
      error
  | Error error -> failwith ("wrong rejection: " ^ C.error_to_string error)
  | Ok _ -> failwith "rejected command succeeded"

let with_client replies f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "typed-rejection" in
  let caps="IMAP4rev1 UNSELECT IDLE" in
  Eio_mock.Flow.on_read flow ([
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\n" ^ done_ 1);
    `Return (done_ 2);
    `Return ("* CAPABILITY " ^ caps ^ "\r\n" ^ done_ 3)] @ replies);
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"password"
    ~mechanism:`Login ~allow_insecure_transport:true () in
  let client=ok (C.of_flow ~sw ~auth flow) in
  Fun.protect ~finally:(fun () -> C.close client) (fun () -> f client)

let test_ordinary_rejections () =
  List.iter (fun status ->
    List.iter (fun (wire,code) ->
      with_client [`Return (tag 4 ^ " " ^ status_text status ^ " " ^ wire ^ "refused\r\n")]
        (fun client ->
          let error=rejected ~tag:(tag 4) ~status ~code
            (C.create_mailbox client ~mailbox:"Archive") in
          let formatted=C.error_to_string error in
          (match code with
           | Some (R.Other_code _) when contains formatted "X-SERVER" ->
               failwith "formatter emitted an unknown raw response code"
           | Some code ->
               (match R.response_code_name code with
                | Some name when not (contains formatted ("[" ^ name ^ "]")) ->
                    failwith "formatter omitted the safe code label"
                | _ -> ())
           | None -> ())))
      ["[NOPERM] ",Some R.Noperm;
       "[X-SERVER opaque-parameter] ",Some (R.Other_code "X-SERVER opaque-parameter");
       "",None]) [`No;`Bad]

let test_append_rejections () =
  List.iter (fun after_literal ->
    let code,wire=if after_literal then R.Overquota,"OVERQUOTA"
      else R.Trycreate,"TRYCREATE" in
    let prefix=if after_literal then [`Return "+ send bytes\r\n"] else [] in
    with_client (prefix @ [`Return (tag 4 ^ " NO [" ^ wire ^ "] refused\r\n")])
      (fun client ->
        ignore (rejected ~tag:(tag 4) ~status:`No ~code:(Some code)
          (C.append_flow client ~mailbox:"Archive" ~length:3L
            (Eio.Flow.string_source "abc"))))) [false;true]

let test_idle_rejections () =
  List.iter (fun after_done ->
    let status,code,wire=if after_done then `Bad,R.Inuse,"INUSE"
      else `No,R.Unavailable,"UNAVAILABLE" in
    let prefix=[`Return ("* 0 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n" ^
      "* OK [UIDNEXT 1] next\r\n" ^ done_ 4)] @
      if after_done then [`Return "+ idling\r\n";`Return "* 1 EXISTS\r\n"] else [] in
    with_client (prefix @ [`Return (tag 5 ^ " " ^ status_text status ^
      " [" ^ wire ^ "] refused\r\n")]) (fun client ->
      ignore (rejected ~tag:(tag 5) ~status ~code:(Some code)
        (C.with_mailbox client ~mode:`Read_only "INBOX" S.wait_for_change))))
    [false;true]

let secret="synthetic-auth-secret"
let username="synthetic-auth-user"
let check_auth ~mechanism ~initial ~wire ~code =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "auth-rejection" in
  let auth,caps,challenge=match mechanism with
    | `Login ->
        Imap_eio.Auth.password ~username ~password:secret ~mechanism:`Login
          ~allow_insecure_transport:true (),"",[]
    | `Cram_md5 ->
        Imap_eio.Auth.password ~username ~password:secret ~mechanism:`Cram_md5
          ~allow_insecure_transport:true (),"AUTH=CRAM-MD5",
        (if initial then [] else [`Return "+ Y2hhbGxlbmdl\r\n"])
    | `Plain ->
        Imap_eio.Auth.password ~username ~password:secret ~mechanism:`Plain
          ~allow_insecure_transport:true (),"AUTH=PLAIN",
        (if initial then [] else [`Return "+ \r\n"])
    | `Oauthbearer ->
        Imap_eio.Auth.bearer ~username ~token:secret ~allow_insecure_transport:true (),
        "AUTH=OAUTHBEARER SASL-IR",
        (if initial then [] else
          [`Return ("+ " ^ Base64.encode_string ("{\"error\":\"" ^ secret ^ "\"}") ^ "\r\n")]) in
  let status=if initial then `Bad else `No in
  Eio_mock.Flow.on_read flow ([
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY IMAP4rev1 " ^ caps ^ "\r\n" ^ done_ 1)] @
    challenge @ [`Return (tag 2 ^ " " ^ status_text status ^ " " ^ wire ^
      "credential " ^ secret ^ " for " ^ username ^ " refused\r\n")]);
  let error=rejected ~text:"authentication rejected" ~tag:(tag 2)
    ~status ~code (C.of_flow ~sw ~auth flow) in
  let formatted=C.error_to_string error in
  List.iter (fun value ->
    if contains formatted value then failwith "authentication formatter leaked credentials")
    [secret;username;Base64.encode_string secret];
  match error with
  | E.Rejected {text;code=actual;_} ->
      if contains text secret || contains text username || actual<>code then
        failwith "authentication diagnostic retained secret material"
  | _ -> assert false

let test_auth_rejections () =
  List.iter (fun mechanism ->
    List.iter (fun initial ->
      List.iter (fun (wire,code) -> check_auth ~mechanism ~initial ~wire ~code)
        ["[AUTHENTICATIONFAILED] ",Some R.Authenticationfailed;
         "[EXPIRED] ",Some R.Expired;
         "[X-AUTH " ^ secret ^ "] ",None;
         "[PERMANENTFLAGS (" ^ secret ^ ")] ",None;
         "[APPENDUID 1 2] ",None;
         "",None]) [false;true]) [`Login;`Cram_md5;`Plain;`Oauthbearer];
  List.iter (fun (name,code) ->
    check_auth ~mechanism:`Login ~initial:false ~wire:("[" ^ name ^ "] ") ~code:(Some code))
    ["UNAVAILABLE",R.Unavailable;"AUTHORIZATIONFAILED",R.Authorizationfailed;
     "PRIVACYREQUIRED",R.Privacyrequired;"CONTACTADMIN",R.Contactadmin;
     "NOPERM",R.Noperm;"INUSE",R.Inuse;"SERVERBUG",R.Serverbug;
     "CLIENTBUG",R.Clientbug;"CANNOT",R.Cannot;"LIMIT",R.Limit]

let () =
  test_ordinary_rejections (); test_append_rejections ();
  test_idle_rejections (); test_auth_rejections ()
