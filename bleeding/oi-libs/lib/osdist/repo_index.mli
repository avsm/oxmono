(** Pure text emitters for the published repo tree.

    These are all strings; the orchestrator ([Osdist_cmd.Repo]) writes them to
    disk, runs [reprepro] / [createrepo_c] in throwaway docker containers, and
    signs with [gpg] host-side. *)

type config = {
  baseurl : string;
      (** Public URL the repo will be served at — baked into the installer
          snippets and dnf [.repo] files. *)
  origin : string;
      (** APT [Origin:] field. Typically the project / org name. *)
  label : string;  (** APT [Label:] field. *)
  description : string;  (** Free-form [Description:] (one line). *)
  gpg_key_id : string;
      (** Short or full fingerprint of the signing key. Surfaced in INSTALL.md
          so users can verify the key out-of-band. *)
  pubkey_filename : string;
      (** Basename of the exported armored public key in the repo root, e.g.
          ["oi.asc"]. *)
}

val apt_distributions : config -> deb_targets:Target.t list -> string
(** [apt_distributions cfg ~deb_targets] is the [conf/distributions] file for
    [reprepro]: one stanza per unique [codename] across [deb_targets], with the
    arches set to [amd64]. *)

val dnf_repo_file : config -> Target.t -> pkg:string -> string
(** [dnf_repo_file cfg t ~pkg] is the [/etc/yum.repos.d/<pkg>.repo] body for a
    single rpm target, including [baseurl], [gpgkey] and the [repo_gpgcheck=1]
    flag. *)

val install_sh : config -> Spec.t -> targets:Target.t list -> string
(** [install_sh cfg s ~targets] is a one-shot installer baked with [cfg.baseurl]
    and [s.package]: detects the host distro from [/etc/os-release], wires up
    the matching signed apt/dnf repo, then [apt|dnf install <pkg>]. Falls back
    to the static-binary tarball download. *)

val install_md : config -> Spec.t -> targets:Target.t list -> string
(** [install_md cfg s ~targets] is a human-readable [INSTALL.md] with copy-paste
    snippets per family. *)
