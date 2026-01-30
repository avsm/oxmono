open! Core

module type S = sig
  module File_kind : File_kind.S
  module File_permissions : File_permissions.S

  module File_stats :
    File_stats.S
    with module File_kind := File_kind
     and module File_permissions := File_permissions

  module On_cleanup_error : On_cleanup_error.S
end

module type Filesystem_types = sig
  module type S = S

  include
    S
    with module File_kind = File_kind
     and module File_permissions = File_permissions
     and module File_stats = File_stats
     and module On_cleanup_error = On_cleanup_error
end
