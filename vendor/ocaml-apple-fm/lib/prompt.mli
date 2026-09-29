(** Text and image prompts. *)

type image = Apple_fm_base.Prompt.image
(** An image read by Foundation Models when a request starts. *)

val image : ?label:string -> string -> image
(** [image path] attaches the image at absolute filesystem path [path]. Image
    prompts require macOS 27 and a vision-capable model. *)

type t = Apple_fm_base.Prompt.t
(** Text with zero or more image attachments. *)

val v : ?images:image list -> string -> t
(** [v ~images text] constructs a prompt. *)

val text : string -> t
(** [text value] constructs a text-only prompt. *)

val pp : Format.formatter -> t -> unit
(** [pp formatter prompt] prints a summary without reading images. *)
