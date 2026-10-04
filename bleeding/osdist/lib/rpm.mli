(** Fedora/RHEL packaging generators.

    Pure string / {!Dockerfile.t} emitters — same shape as {!Deb}. *)

val spec :
  Spec.t -> Target.t -> overlay_depexts:string list -> date_rpm:string -> string
(** [spec] renders the RPM specfile. [date_rpm] is the [%%changelog] entry's
    date in the rpm-conventional form ([Wed May 21 2026]). *)

val dockerfile :
  Spec.t -> Target.t -> overlay_depexts:string list -> Dockerfile.t

val filename : Spec.t -> Target.t -> string
(** [filename s t] is the conventional binary [.rpm] filename for [s] on [t],
    e.g. [oi-0.13.5-1.fc44.x86_64.rpm]. *)
