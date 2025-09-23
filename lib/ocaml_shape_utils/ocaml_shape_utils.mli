open Ocaml_typing

module Decl : sig
  type t = Shape.Uid.t * Ident.t option * Typedtree.item_declaration

  val decl_kind_to_string : Typedtree.item_declaration -> string
  val pp : Format.formatter -> t -> unit
end

type cmt = { unit_name : string; path : Fpath.t; decls : Decl.t list }

val cmts_of_packages :
  packages:string list -> units:(string -> bool) -> cmt list
(** Path to the [.cmt] in the given packages. Cmts for which [units "Unit_name"]
    return [false] are not collected. Uses [ocamlfind]. *)

val cmt_of_path : Fpath.t -> cmt option
val pp : Format.formatter -> cmt -> unit
