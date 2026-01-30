(** Wave mode is used to communicate to each individual harness which waveform format to
    use, since they require slightly different setup per type of simulator (i.e. VCD uses
    a streaming file while hardcamlwaveform serializes at the end of the simulation). *)

type t =
  | None
  | Hardcamlwaveform
  (** This option stores and returns the waves in the hardcaml internal waveform format.
      It can be used for any non-streaming waveform serialization. *)
  | Vcd of { out_channel : Out_channel.t }
