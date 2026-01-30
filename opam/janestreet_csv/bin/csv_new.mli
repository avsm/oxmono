open! Core

module Merge : sig
  val command : Command.t
end

module Sort : sig
  val command : Command.t
end

module Transpose : sig
  val command : Command.t
end

module Grep : sig
  val command : Command.t
end

module Grid : sig
  val command : Command.t
end

module To_ascii_table : sig
  val command : Command.t
end

module To_html_table : sig
  val command : Command.t
end

module To_pipe_table : sig
  val command : Command.t
end

module Sum : sig
  val command : Command.t
end

module Sum_group : sig
  val command : Command.t
end

module Id : sig
  val command : Command.t
end

module Validate : sig
  val command : Command.t
end

module Header : sig
  val command : Command.t
end

module Count_rows : sig
  val command : Command.t
end

module Add_column : sig
  val command : Command.t
end
