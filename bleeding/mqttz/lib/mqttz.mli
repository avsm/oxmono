module Slice = Slice
(** MQTT 3.1.1 and 5.0 wire codecs for OxCaml.

    Decode exactly one bounded frame with [V3.Packet.decode] or
    [V5.Packet.decode]. Use [Frame.length] while assembling a stream. PUBLISH
    payloads borrow their source bytes. [Packet.encode] returns separate header
    and payload views. [Slice.copy] transfers a borrowed payload into
    independent storage.

    The packet codecs validate wire syntax and packet-local rules. Session
    rules, negotiated limits and packet direction are enforced by the Eio
    client. *)


module Frame = Frame
module Qos = Shared.Qos
module Protocol_version = Shared.Protocol_version
module Credentials = Shared.Credentials
module Will = Shared.Will
module Packet_id = Shared.Packet_id
module Topic = Shared.Topic
module V3 = V3
module V5 = V5
