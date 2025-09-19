let merlin_lid x = x

let tpat_alias_ident = function
  | Ocaml_typing.Typedtree.Tpat_alias (_, ident, _, _) -> Some ident
  | _ -> None

let sub_locs_of_ident _ = []
