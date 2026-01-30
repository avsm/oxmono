open! Core

val extension
  :  name:string
  -> runtime_kind:Runtime_kind.t
  -> experimental_features_allowed:bool
  -> Ppxlib.Extension.t
