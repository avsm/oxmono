(* Opt-in, one fresh process per transport/size. No complete message buffer. *)
let ok = function Ok x -> x | Error e -> failwith (Imap_eio.Client.error_to_string e)
let env = Sys.getenv
let setting name default allowed =
  let value=Option.value ~default (Sys.getenv_opt name) in
  if not (List.mem value allowed) then invalid_arg name;
  value
let gc_mode=setting "IMAP_MEMORY_GC" "retained" ["retained";"normal"]
let payload=setting "IMAP_MEMORY_PAYLOAD" "repeat" ["repeat";"entropy"]
let heap () = (Gc.quick_stat ()).heap_words * (Sys.word_size / 8)
let mib = 1024 * 1024
let rss () =
  try In_channel.with_open_bin "/proc/self/status" (fun input ->
    let rec loop () = match input_line input with
      | line when String.starts_with ~prefix:"VmRSS:" line ->
          Scanf.sscanf line "VmRSS: %d kB" (fun n -> n * 1024)
      | _ -> loop () in loop ())
  with Sys_error _ | End_of_file -> 0
let live () =
  Gc.full_major ();
  (Gc.stat ()).live_words * (Sys.word_size / 8)
type measurement = {
  baseline_live:int; baseline_rss:int; baseline_heap:int; mutable peak_heap:int;
  mutable peak_live:int; mutable peak_rss:int; mutable next:int;
}
let measurement () =
  let baseline_live=live () in
  let baseline_rss=rss () in
  let baseline_heap=heap () in
  {baseline_live;baseline_rss;baseline_heap;peak_heap=baseline_heap;peak_live=baseline_live;
   peak_rss=baseline_rss;next=0}
let sample m count =
  if count >= m.next then (
    (* RSS before forced collection; live heap after collection. *)
    m.peak_rss <- max m.peak_rss (rss ());
    m.peak_heap <- max m.peak_heap (heap ());
    if gc_mode="retained" then m.peak_live <- max m.peak_live (live ());
    m.next <- if count > max_int-mib then max_int else count + mib)
let report mode size phase m =
  let delta=max 0 (m.peak_live-m.baseline_live) in
  Printf.printf "%s,%s,%s,%d,%s,%d,%d,%d,%d,%d,%d\n%!" gc_mode payload mode size phase
    m.baseline_live (if gc_mode="retained" then delta else -1)
    m.baseline_rss (max 0 (m.peak_rss-m.baseline_rss))
    m.baseline_heap (max 0 (m.peak_heap-m.baseline_heap));
  if gc_mode="retained" && delta >= 8*mib then
    failwith "additional sampled live heap reached 8 MiB"

let block =
  "From: memory@example.test\r\nSubject: streaming memory\r\n\r\n" ^
  String.concat "" (List.init 512 (fun n ->
    String.init 76 (fun i -> Char.chr (33 + (n*37+i*17) mod 90)) ^ "\r\n"))
module Source = struct
  type t = {length:int; mutable count:int; m:measurement;
            mutable hash:Digestif.SHA256.ctx; random:Random.State.t}
  let read_methods=[]
  let single_read t (buffer @ local) =
    let buffer=Cstruct.globalize buffer in
    if t.count=t.length then raise End_of_file;
    sample t.m t.count;
    let n=min (Cstruct.length buffer) (t.length-t.count) in
    let rec fill offset =
      if offset<n then (
        let from=(t.count+offset) mod String.length block in
        let len=min (n-offset) (String.length block-from) in
        Cstruct.blit_from_string block from buffer offset len;
        fill (offset+len)) in
    if payload="repeat" then fill 0 else (
      let header="From: memory@example.test\r\nSubject: random ASCII\r\n\r\n" in
      for offset=0 to n-1 do
        let position=t.count+offset in
        let byte=if position<String.length header then header.[position]
          else match (position-String.length header) mod 78 with
            | 76 -> '\r' | 77 -> '\n'
            | _ -> Char.chr (33 + Random.State.int t.random 94) in
        Cstruct.set_char buffer offset byte
      done);

    t.hash <- Digestif.SHA256.feed_string t.hash
      (Cstruct.to_string (Cstruct.sub buffer 0 n));
    t.count <- t.count+n;
    n
end
module Sink = struct
  type t = {mutable count:int; m:measurement; mutable hash:Digestif.SHA256.ctx}
  let single_write t (buffers @ local) =
    let buffers=Cstruct.globalize_list buffers in
    List.iter (fun chunk ->
      t.hash <- Digestif.SHA256.feed_string t.hash (Cstruct.to_string chunk);
      t.count <- t.count+Cstruct.length chunk;
      sample t.m t.count) buffers;
    Cstruct.lenv buffers
  let copy t ~src=Eio.Flow.Pi.simple_copy ~single_write t ~src
end
let source_handler=Eio.Flow.Pi.source (module Source)
let sink_handler=Eio.Flow.Pi.sink (module Sink)
let run mode size = Eio_main.run (fun io ->
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock io) 300. (fun () ->
  Eio.Switch.run (fun sw ->
    let tls,compressed=match mode with
      | "plain" -> `Plain,false | "tls" -> `Implicit,false
      | "starttls" -> `Required_starttls,false
      | "starttls-deflate" -> `Required_starttls,true
      | "deflate" -> `Plain,true | "tls-deflate" -> `Implicit,true
      | _ -> invalid_arg "unknown transport mode" in
    let pem=In_channel.with_open_bin (env "IMAP_DOVECOT_CA_CERT") In_channel.input_all in
    let ca=match X509.Certificate.decode_pem pem with
      | Ok ca -> ca | Error (`Msg e) -> failwith e in
    let authenticator=X509.Authenticator.chain_of_trust_no_crl
      ~time:(fun () -> Some (Ptime_clock.now ())) [ca] in
    let transport=Imap_eio.Transport.v ~net:(Eio.Stdenv.net io)
      ~host:(env "IMAP_DOVECOT_HOST")
      ~port:(int_of_string (env (if tls=`Implicit then "IMAP_DOVECOT_TLS_PORT"
                                else "IMAP_DOVECOT_PORT")))
      ~tls ~authenticator () in
    let auth=Imap_eio.Auth.password ~username:(env "IMAP_DOVECOT_USER")
      ~password:(env "IMAP_DOVECOT_PASSWORD") ~mechanism:`Cram_md5 () in
    let client=ok (Imap_eio.Client.connect ~sw ~auth transport) in
    let mailbox=Printf.sprintf "Oxmono-memory-%d-%s-%d" (Unix.getpid ()) mode size in
    Fun.protect ~finally:(fun () -> Eio.Cancel.protect (fun () ->
      ignore (Imap_eio.Client.delete_mailbox client mailbox);
      Imap_eio.Client.close client)) (fun () ->
      if compressed then ok (Imap_eio.Client.compress_deflate client);
      ok (Imap_eio.Client.create_mailbox client mailbox);
      let length=size*mib in
      let append=measurement () in
      let source={Source.length;count=0;m=append;hash=Digestif.SHA256.empty;
                  random=Random.State.make [|0x1a2b;42|]} in
      let receipt=match ok (Imap_eio.Client.append_flow_receipt client ~mailbox
        ~length:(Int64.of_int length) (Eio.Resource.T (source,source_handler))) with
        | Some receipt -> receipt | None -> failwith "missing APPENDUID" in
      sample append max_int;
      report mode size "append" append;
      ok (Imap_eio.Client.with_mailbox client ~mode:`Read_only mailbox (fun selected ->
        let fetch=measurement () in
        let sink={Sink.count=0;m=fetch;hash=Digestif.SHA256.empty} in
        ok (Imap_eio.Selected.fetch_to selected
          ~uid:(Imap.Uid.to_int64 receipt.uid)
          (Eio.Resource.T (sink,sink_handler)));
        sample fetch max_int;
        report mode size "fetch" fetch;
        if source.count<>length || sink.count<>length ||
           Digestif.SHA256.(to_hex (get source.hash) <> to_hex (get sink.hash)) then
          failwith "streamed message digest/length mismatch";
        Ok ()))))))
let () =
  if Array.length Sys.argv<>3 then invalid_arg "body_memory MODE SIZE_MIB";
  let size=int_of_string Sys.argv.(2) in
  if not (List.mem size [1;10;100]) then invalid_arg "size must be 1, 10 or 100 MiB";
  run Sys.argv.(1) size
