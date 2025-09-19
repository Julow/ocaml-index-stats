open Ocaml_parsing
open Ocaml_typing

val merlin_lid :
  Merlin_index_format.Index_format.Lid.t -> Longident.t Location.loc

(* [tpat_alias_ident tpat] returns [Some ident] if [tpat] is [Tpat_alias _],
   [None] otherwise. *)
val tpat_alias_ident : Typedtree.value Typedtree.pattern_desc -> Ident.t option
val sub_locs_of_ident : Longident.t -> Location.t list
