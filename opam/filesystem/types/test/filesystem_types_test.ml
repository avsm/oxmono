open! Core

(* Make sure we've tested the whole library. *)
module _ : Filesystem_types.S = struct
  module File_kind = Test_file_kind
  module File_permissions = Test_file_permissions
  module File_stats = Test_file_stats
  module On_cleanup_error = Test_on_cleanup_error
end
