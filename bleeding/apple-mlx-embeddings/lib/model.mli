(** Selection and acquisition of supported Qwen3 embedding checkpoints. *)

type t = private {
  name : string;
  repo : string;
  revision : string;
  dimensions : int;
  files : (string * int64) list;
}
(** A checkpoint pinned to a Hugging Face commit. [files] records the required
    filenames and byte lengths. [dimensions] is the full output width. *)

val all : t list
(** [all] is the supported model catalog, ordered by increasing size. *)

val default : t
(** [default] is the 4-bit Qwen3-Embedding-4B checkpoint. *)

val find : string -> t option
(** [find name] is the model with the short name or repository ID [name]. *)

val vision_all : t list
(** [vision_all] is the supported text-and-image embedding model catalog. *)

val vision_default : t
(** [vision_default] is the 4-bit Qwen3-VL-Embedding-2B checkpoint. *)

val find_vision : string -> t option
(** [find_vision name] is the vision model with the short name or repository ID
    [name]. Vision models require {!Apple_mlx_embeddings.Vision}. *)

val cache_dir : unit -> string
(** [cache_dir ()] is the absolute Hugging Face cache directory. It uses
    [HF_HUB_CACHE], [HUGGINGFACE_HUB_CACHE], [HF_HOME/hub],
    [XDG_CACHE_HOME/huggingface/hub], then [HOME/.cache/huggingface/hub], in
    that order. Empty environment variables are ignored. *)

val dir : ?cache_dir:string -> t -> string
(** [dir model] is the absolute snapshot directory for [model]. [cache_dir]
    overrides {!cache_dir}. Relative paths resolve against the working directory
    and a leading [~/] expands to [HOME]. *)

val present : ?cache_dir:string -> t -> bool
(** [present model] reports whether every required file is regular and has its
    expected byte length. Hugging Face blob symlinks are followed. This checks
    completeness, not content hashes, and performs no network access. *)

val ensure :
  proc:_ Eio_unix.Process.mgr ->
  stderr:_ Eio.Flow.sink ->
  ?cache_dir:string ->
  t ->
  string
(** [ensure ~proc ~stderr model] is the local snapshot directory for [model],
    downloading missing files with [hf download] or [uvx hf download]. A
    complete snapshot needs neither executable nor network access. Downloads pin
    the catalog revision and use Hugging Face's resumable cache. Files with
    incorrect lengths are removed from the snapshot and downloaded again. Other
    complete files are reused. Non-file entries are refused.

    Both subprocess output streams go to [stderr]. Authentication uses the
    inherited Hugging Face environment or login. Cancellation terminates the
    downloader process group and propagates without retrying. [Failure] reports
    a failed or incomplete download. This function requires an Eio scheduler. *)
