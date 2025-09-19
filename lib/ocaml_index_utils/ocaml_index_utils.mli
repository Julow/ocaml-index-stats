open Ocaml_parsing

val occurrences :
  dune_build_dir:string ->
  cmts:Ocaml_shape_utils.cmt list ->
  ((string * string) * Longident.t Location.loc) list
(** Find all the occurrences of declarations from modules loaded in [cmts] in
    the [.ocaml-index] files found in the [dune_build_dir] directory (expected
    to be a path to Dune's [_build] directory). *)
