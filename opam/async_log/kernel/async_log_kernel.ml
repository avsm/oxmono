module Global = Global
module Level = Level
module Log = Log
module Message = Message
module Message_event = Message_event

module Output = struct
  include Output
  module Format = Output_format

  module Private = struct
    include Private
    module Name = Output_name

    let write = write
    let flush = flush
  end
end

module Ppx_log_syntax = struct
  module Ppx_log_syntax = Ppx_log_syntax
end

module For_testing = struct
  module Mutable_outputs = Mutable_outputs
end
