(** Apple's on-device Foundation Models framework.

    [Apple_fm] provides Eio-native sessions, streamed and structured responses,
    typed tools, transcripts, token accounting, and multimodal prompts for
    Apple's system language model.

    {[
      open Apple_fm

      let weather =
        let arguments =
          Codec.Invoke.map "weather" Fun.id
          |> Codec.Invoke.param ~enc:Fun.id "city" Codec.string
               ~description:"city whose weather is required"
          |> Codec.Invoke.seal
        in
        Tool.v ~description:"Return the current weather." arguments (fun city ->
            city ^ ": 18 C")

      let () =
        Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
        let session =
          Session.create ~sw ~instructions:"Answer briefly." [ weather ]
        in
        ignore
          (Session.respond_stream session ~output:(Eio.Stdenv.stdout env)
             "What is the weather in London?")
    ]}

    A session keeps its transcript between responses and belongs to an
    [Eio.Switch.t]. Tool handlers run in the response fiber and may use Eio
    operations directly. Only one response uses a session at a time; concurrent
    callers wait on an Eio mutex without blocking the domain.

    Cancelling a response cancels generation and closes the session. Create a
    new session before sending another prompt after cancellation. Framework
    failures are contextual [Eio.Io] exceptions carrying {!Error.E}.

    The package requires macOS 26 or later, Apple silicon, an SDK containing
    [FoundationModels.framework], and Apple Intelligence enabled. It is
    available in native-code builds only.

    See Apple's guides to
    {{:https://developer.apple.com/documentation/foundationmodels/prompting-an-on-device-foundation-model}
      prompting the on-device model},
    {{:https://developer.apple.com/documentation/foundationmodels/expanding-generation-with-tool-calling}
      tool calling},
    and
    {{:https://developer.apple.com/documentation/foundationmodels/generating-swift-data-structures-with-guided-generation}
      guided generation}. *)

module Availability = Availability
(** Check whether the system language model can accept requests. *)

module Schema = Schema
(** Describe JSON values for Apple's guided generation. *)

module Codec = Codec
(** Pair generation schemas with typed jsont encoders and decoders. *)

module Tool = Tool
(** Define typed functions that the model may call. *)

module Transcript = Transcript
(** Store and restore complete Foundation Models conversations. *)

module Prompt = Prompt
(** Construct text and image prompts. *)

module Model = Model
(** Select a system model and inspect or count its inputs. *)

module Context = Context
(** Configure per-response context and inspect token usage. *)

module Generation = Generation
(** Configure sampling, response length, and tool calling. *)

module Error = Error
(** Inspect failures carried by contextual Eio exceptions. *)

module Session = Session
(** Run persistent, streaming, tool-using model conversations. *)
