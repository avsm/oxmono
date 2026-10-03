(* A prompt is the pair of flows the program was handed, plus the buffer that
   remembers where reading got to. Both halves are Eio capabilities: asking
   suspends the fiber instead of parking the domain, and a test drives the same
   code from a string source. *)

type t = {
  interactive : bool;
  input : Eio.Buf_read.t;
  output : Eio.Flow.sink_ty Eio.Resource.t;
}

(* A typed answer is one line from a human. The cap keeps a source that never
   sends a terminator from growing the buffer without bound; crossing it is an
   [Error] like any other unanswerable question. *)
let max_answer = 4096

(* The one genuine Unix boundary: whether there is a terminal on the other end
   of the input is a property of its descriptor, and a flow with no descriptor
   -- a string source, a pipe from a mock -- has nobody to ask. *)
let is_a_terminal stdin =
  match Eio_unix.Resource.fd_opt stdin with
  | None -> false
  | Some fd -> Eio_unix.Fd.use_exn "isatty" fd Unix.isatty

let v ?interactive ~stdin ~stdout () =
  let interactive =
    match interactive with Some i -> i | None -> is_a_terminal stdin
  in
  {
    interactive;
    input = Eio.Buf_read.of_flow ~max_size:max_answer stdin;
    output = (stdout :> Eio.Flow.sink_ty Eio.Resource.t);
  }

let interactive t = t.interactive

let line t question =
  if not t.interactive then Error (`Msg "no terminal to ask on")
  else begin
    (* The question goes out first, so it is on the terminal even when the
       answer never comes. *)
    Eio.Flow.copy_string question t.output;
    match Eio.Buf_read.line t.input with
    | answer -> Ok answer
    | exception End_of_file -> Error (`Msg "no answer: the input ended")
    | exception Eio.Buf_read.Buffer_limit_exceeded ->
        Error (`Msg "no answer: the line is too long")
  end

let confirm t label =
  match line t (label ^ " [y/N] ") with
  | Error (`Msg _ as e) -> Error e
  | Ok answer -> (
      match String.lowercase_ascii (String.trim answer) with
      | "y" | "yes" -> Ok true
      | _ -> Ok false)

let expect t question word =
  match line t question with
  | Error (`Msg _ as e) -> Error e
  | Ok answer ->
      (* The refusal says that the answer was wrong and stops there: spelling
         the word out would hand over the answer the gate exists to ask for. *)
      if String.equal answer word then Ok ()
      else Error (`Msg "the answer does not match what was asked for")

(* Reading a secret is the one question that does not go through the flows a
   [t] was handed. Echo is the terminal's own line discipline, so masking what
   is typed means taking that terminal over with termios; that is a local
   terminal and a Unix descriptor, which is why it lives here in the driver
   rather than in the freestanding half of the library. *)

let err_msg fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let rec write_all fd s off =
  if off < String.length s then
    match Unix.write_substring fd s off (String.length s - off) with
    | 0 -> raise End_of_file
    | n -> write_all fd s (off + n)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_all fd s off

let write fd s = write_all fd s 0

let masked_attributes attr =
  {
    attr with
    Unix.c_icanon = false;
    c_echo = false;
    c_echoe = false;
    c_echok = false;
    c_echonl = false;
    c_vmin = 1;
    c_vtime = 0;
  }

let secret ?(prompt = "") () =
  if not (Unix.isatty Unix.stdin) then
    Error (`Msg "cannot prompt for a secret because stdin is not a terminal")
  else
    try
      let original = Unix.tcgetattr Unix.stdin in
      let restore () = Unix.tcsetattr Unix.stdin Unix.TCSANOW original in
      write Unix.stderr prompt;
      Terminal.on_interrupt restore (fun () ->
          Fun.protect ~finally:restore (fun () ->
              Unix.tcsetattr Unix.stdin Unix.TCSANOW
                (masked_attributes original);
              let input = Console.Input.v ~mask:"*" () in
              let buf = Bytes.create 64 in
              let rec loop () =
                match Unix.read Unix.stdin buf 0 (Bytes.length buf) with
                | 0 -> Error (`Msg "terminal reached end of input")
                | n -> (
                    let echo, lines =
                      Console.Input.feed input (Bytes.sub_string buf 0 n)
                    in
                    write Unix.stderr echo;
                    match lines with line :: _ -> Ok line | [] -> loop ())
                | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
              in
              loop ()))
    with
    | Unix.Unix_error (e, fn, arg) ->
        err_msg "%s(%s): %s" fn arg (Unix.error_message e)
    | End_of_file -> Error (`Msg "terminal output closed while prompting")
