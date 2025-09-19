let merlin_lid =
  let open Merlin_index_format.Index_format in
  Lid.to_list

let tpat_alias_ident = function
  | Ocaml_typing.Typedtree.Tpat_alias (_, ident, _, _, _) -> Some ident
  | _ -> None

let sub_locs_of_ident =
  let rec collect acc = function
    | Longident.Lident _ -> []
    | Ldot (lhs, rhs) -> collect (lhs.loc :: rhs.loc :: acc) lhs.txt
    | Lapply (lhs, rhs) ->
        collect (lhs.loc :: rhs.loc :: collect acc rhs.txt) lhs.txt
  in
  collect []
