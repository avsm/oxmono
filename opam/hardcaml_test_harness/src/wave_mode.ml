type t =
  | None
  | Hardcamlwaveform
  | Vcd of { out_channel : Out_channel.t }
