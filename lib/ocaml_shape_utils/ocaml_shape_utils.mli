open Ocaml_typing

type cmt

val cmts_of_packages :
  packages:string list -> units:(string -> bool) -> cmt list
(** Path to the [.cmt] in the given packages. Cmts for which [units "Unit_name"]
    return [false] are not collected. Uses [ocamlfind]. *)

val cmt_of_path : Fpath.t -> cmt option

module Decl : sig
  type t = Shape.Uid.t * Ident.t option * Typedtree.item_declaration
end

val unit_name : cmt -> string
val declarations : cmt -> Decl.t list
