open Ocaml_parsing

val merlin_lid :
  Merlin_index_format.Index_format.Lid.t -> Longident.t Location.loc

val sub_locs_of_ident : Longident.t -> Location.t list
