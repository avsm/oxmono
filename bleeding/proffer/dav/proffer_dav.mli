(** WebDAV client integration for Proffer applications.
    All types and exceptions are shared with {!Fetch_dav}; link [proffer.dav]. *)
include module type of struct include Fetch_dav end
