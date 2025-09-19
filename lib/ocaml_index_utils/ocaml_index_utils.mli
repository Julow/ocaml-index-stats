open Ocaml_parsing

val occurrences :
  dune_build_dir:string ->
  cmts:Ocaml_shape_utils.cmt list ->
  units:(string -> bool) ->
  ((string * string) * Longident.t Location.loc) list
(** Find all the occurrences of values from module matched by [units] of
    packages matching the ocamlfind query specified with [package_query] in the
    [.ocaml-index] files found in Dune's [_build] directory. *)
