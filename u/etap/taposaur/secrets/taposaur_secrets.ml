let ( / ) = Eio.Path.( / )
type credential =
  | Elsevier_api_key | Elsevier_inst_token | Springer_api_key
  | Wiley_tokens | Core_api_token
let credentials = [Elsevier_api_key; Elsevier_inst_token; Springer_api_key;
  Wiley_tokens; Core_api_token]
let name = function
  | Elsevier_api_key -> "taposaur/elsevier/api-key"
  | Elsevier_inst_token -> "taposaur/elsevier/inst-token"
  | Springer_api_key -> "taposaur/springer/api-key"
  | Wiley_tokens -> "taposaur/wiley/tokens"
  | Core_api_token -> "taposaur/core/token"
let variable = function
  | Elsevier_api_key -> "ELSEVIER_API_KEY"
  | Elsevier_inst_token -> "ELSEVIER_INST_TOKEN"
  | Springer_api_key -> "SPRINGER_API_KEY"
  | Wiley_tokens -> "WILEY_TOKENS"
  | Core_api_token -> "CORE_API_TOKEN"
let of_name s = List.find_opt (fun c -> name c = s) credentials

type origin = Keychain | Environment
type t = { store : Sekrit_eio.Store.t option; env : string -> string option }

let nonblank = function
  | Some s when String.trim s <> "" -> Some s
  | _ -> None
let open_ ?(env = Sys.getenv_opt) ~sw ~fs ?root ?key_file () =
  let root = match root with
    | Some root -> (root :> Eio.Fs.dir_ty Eio.Path.t)
    | None -> (fs / Sekrit.Paths.root () :> Eio.Fs.dir_ty Eio.Path.t) in
  let key_file = match key_file with
    | Some key_file -> (key_file :> Eio.Fs.dir_ty Eio.Path.t)
    | None -> (fs / Sekrit.Paths.key_file () :> Eio.Fs.dir_ty Eio.Path.t) in
  let store = if Eio.Path.is_directory root
    then Some (Sekrit_eio.Store.open_ ~sw ~root ~key_file) else None in
  { store; env }
let keychain t = t.store <> None
let stored t c = Option.bind t.store (fun store ->
  Sekrit_eio.Store.find store (name c))
let find t c = match stored t c with
  | Some secret -> Some secret
  | None -> Option.map Sekrit.Secret.of_string
      (nonblank (t.env (variable c)))
let origin t c = match stored t c with
  | Some _ -> Some Keychain
  | None -> Option.map (fun _ -> Environment) (nonblank (t.env (variable c)))
let value t c = Option.map (fun _ () -> match find t c with
  | Some secret -> Sekrit.Secret.with_value secret Fun.id
  | None -> failwith ("Credential was removed: " ^ name c)) (find t c)
