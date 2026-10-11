type t = {
  name : string;
  jmap_profile : string;
  account : string option;
  sortal_roots : string list;
  body_limit : int;
  max_steps : int;
  chat_model : string option;
  embedding_url : string option;
  embedding_model : string option;
  embedding_dimensions : int;
}

let codec =
  let make name jmap_profile account sortal_roots body_limit max_steps
      chat_model embedding_url embedding_model embedding_dimensions =
    {
      name;
      jmap_profile;
      account;
      sortal_roots;
      body_limit;
      max_steps;
      chat_model;
      embedding_url;
      embedding_model;
      embedding_dimensions;
    }
  in
  Jsont.Object.map make
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun x -> x.name)
  |> Jsont.Object.mem "jmapProfile" Jsont.string ~enc:(fun x -> x.jmap_profile)
  |> Jsont.Object.mem "account" (Jsont.option Jsont.string) ~enc:(fun x ->
         x.account)
  |> Jsont.Object.mem "sortalRoots" (Jsont.list Jsont.string) ~enc:(fun x ->
         x.sortal_roots)
  |> Jsont.Object.mem "bodyLimit" Jsont.int ~enc:(fun x -> x.body_limit)
  |> Jsont.Object.mem "maxSteps" Jsont.int ~enc:(fun x -> x.max_steps)
  |> Jsont.Object.mem "chatModel"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:(fun x -> x.chat_model)
  |> Jsont.Object.mem "embeddingUrl"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:(fun x -> x.embedding_url)
  |> Jsont.Object.mem "embeddingModel"
       (Jsont.option Jsont.string)
       ~dec_absent:(fun () -> None)
       ~enc:(fun x -> x.embedding_model)
  |> Jsont.Object.mem "embeddingDimensions" Jsont.int
       ~dec_absent:(fun () -> 0)
       ~enc:(fun x -> x.embedding_dimensions)
  |> Jsont.Object.finish

let valid name =
  if not (Jmap_eio.Profile.valid_name name) then
    invalid_arg "Invalid profile name"

let xdg var fallback =
  match Sys.getenv_opt var with
  | Some s when not (Filename.is_relative s) -> s
  | _ -> (
      match Sys.getenv_opt "HOME" with
      | Some s when not (Filename.is_relative s) -> Filename.concat s fallback
      | _ -> invalid_arg "An absolute HOME or XDG path is required")

let directory var fallback name =
  valid name;
  Filename.concat (Filename.concat (xdg var fallback) "siftbot") name

let rec mkdir dir =
  if not (Sys.file_exists dir) then (
    mkdir (Filename.dirname dir);
    try Unix.mkdir dir 0o700 with Unix.Unix_error (EEXIST, _, _) -> ())

let config name =
  Filename.concat (directory "XDG_CONFIG_HOME" ".config" name) "profile.json"

let data_path name =
  let dir = directory "XDG_DATA_HOME" ".local/share" name in
  mkdir dir;
  if (Unix.stat dir).Unix.st_perm land 0o077 <> 0 then
    invalid_arg "Siftbot profile directory must have mode 0700";
  Filename.concat dir "corpus.sqlite3"

let defaults ~name ~jmap_profile =
  valid name;
  valid jmap_profile;
  {
    name;
    jmap_profile;
    account = None;
    sortal_roots = [];
    body_limit = 65536;
    max_steps = 100;
    chat_model = None;
    embedding_url = None;
    embedding_model = None;
    embedding_dimensions = 0;
  }

let write ~exclusive p =
  let path = config p.name in
  mkdir (Filename.dirname path);
  let text = Jmap_eio.Codec.encode_exn codec p ^ "\n" in
  if String.length text > 65536 then invalid_arg "Profile is too large";
  let tmp, oc =
    Filename.open_temp_file ~perms:0o600 ~temp_dir:(Filename.dirname path)
      ".profile-" ".tmp"
  in
  Fun.protect
    ~finally:(fun () ->
      close_out_noerr oc;
      if Sys.file_exists tmp then Sys.remove tmp)
    (fun () ->
      output_string oc text;
      flush oc;
      Unix.fsync (Unix.descr_of_out_channel oc);
      close_out oc;
      if exclusive then Unix.link tmp path else Unix.rename tmp path)

let init ~name ~jmap_profile =
  write ~exclusive:true (defaults ~name ~jmap_profile)

let exists name = Sys.file_exists (config name)

let names () =
  let dir = Filename.concat (xdg "XDG_CONFIG_HOME" ".config") "siftbot" in
  if not (Sys.file_exists dir) then []
  else
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun name -> Jmap_eio.Profile.valid_name name && exists name)
    |> List.sort String.compare

let load name =
  valid name;
  let path = config name in
  if (Unix.stat path).Unix.st_size > 65536 then
    invalid_arg "Profile is too large";
  let ic = open_in_bin path in
  let text =
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  let p = Jmap_eio.Codec.decode_exn codec text in
  if
    p.name <> name || p.body_limit < 1 || p.body_limit > 1048576
    || p.max_steps < 1 || p.max_steps > 10000
  then invalid_arg "Invalid profile limits";
  valid p.jmap_profile;
  p

let connect ~sw env p =
  let checked = function
    | Ok x -> x
    | Error e -> failwith (Jmap_eio.Profile.error_to_string e)
  in
  let store = checked (Jmap_eio.Profile.xdg_store env) in
  let jp = checked (Jmap_eio.Profile.load store p.jmap_profile) in
  let token =
    match Jmap_eio.Profile.credential jp with
    | Bearer token -> token
    | Basic _ ->
        invalid_arg "Siftbot requires a shared bearer-token JMAP profile"
  in
  let service = Jmap_eio.Profile.session_url jp in
  let reader =
    Jmap_eio.Mail.connect_read_only ~sw ~fetch:(Fetch_curl.v ~sw ())
      ~clock:(Eio.Stdenv.clock env) ~token ?account_id:p.account service
  in
  Import.remote ~service ~body_limit:p.body_limit reader

let save p =
  valid p.jmap_profile;
  write ~exclusive:false p
