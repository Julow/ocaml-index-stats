open! Ocaml_parsing
open! Ocaml_typing

module Per_declaration = struct
  module Kind = struct
    type t =
      [ `Value | `Type | `Typext | `Module | `Modtype | `Class | `Class_type ]

    let to_string = function
      | `Value -> "value"
      | `Type -> "type"
      | `Typext -> "typext"
      | `Module -> "module"
      | `Modtype -> "modtype"
      | `Class -> "class"
      | `Class_type -> "class_type"
  end

  type decl = {
    d_ident : string;
    d_occur : (int * Fpath.Set.t) option;
    d_kind : Kind.t;
    d_subdecls : decl list; (* For modules, module types, etc.. *)
  }

  type module_ = { m_name : string; m_path : string; m_decls : decl list }
  type t = module_ list

  let fname_of_lid lid = lid.Location.loc.loc_start.pos_fname

  (** Remove occurrences to that are from the same module. *)
  let remove_self_occurrences ~cmt =
    List.filter (fun lid -> cmt.Ocaml_shape_utils.path <> fname_of_lid lid)

  let compute_occurrences ~cmt index uid =
    let occs =
      Ocaml_index_utils.lookup_occurrences index uid
      |> remove_self_occurrences ~cmt
    in
    let paths =
      List.fold_left
        (fun acc lid -> Fpath.Set.add (Fpath.v (fname_of_lid lid)) acc)
        Fpath.Set.empty occs
    in
    (List.length occs, paths)

  let rec decl_of_sig_item ~cmt path index item =
    let open Ocaml_shape_utils.Shap in
    let mk d_kind ident path d_subdecls =
      let d_ident = Ident.name ident in
      let uid = Ocaml_shape_utils.Shap.reduce path in
      let d_occur = Option.map (compute_occurrences ~cmt index) uid in
      Some { d_ident; d_occur; d_kind; d_subdecls }
    in
    match item with
    | _ when Types.item_visibility item = Hidden -> None
    | Types.Sig_value (ident, _, _) -> mk `Value ident (value path ident) []
    | Sig_type (ident, _, _, _) -> mk `Type ident (type_ path ident) []
    | Sig_typext (ident, _, _, _) ->
        mk `Typext ident (extension_constructor path ident) []
    | Sig_module (ident, _, d, _, _) ->
        (* Ignore [Mp_absent]. *)
        let path = module_ path ident in
        let subdecls = decls_of_module_type ~cmt path index d.md_type in
        mk `Module ident path subdecls
    | Sig_modtype (ident, d, _) ->
        let path = module_type path ident in
        let subdecls =
          match d.mtd_type with
          | Some mt -> decls_of_module_type ~cmt path index mt
          | None -> []
        in
        mk `Modtype ident path subdecls
    | Sig_class (ident, _, _, _) -> mk `Class ident (class_ path ident) []
    | Sig_class_type (ident, _, _, _) ->
        mk `Class_type ident (class_type path ident) []

  and decls_of_module_type ~cmt prefix index = function
    | Types.Mty_signature sg -> decls_of_signature ~cmt prefix index sg
    | Mty_functor (_, mt) -> decls_of_module_type ~cmt prefix index mt
    | _ -> []

  and decls_of_signature ~cmt prefix index sg =
    List.filter_map (decl_of_sig_item ~cmt prefix index) sg

  let compute_module index
      ({ Ocaml_shape_utils.unit_name; path; intf; shape; _ } as cmt) =
    let m_decls =
      match intf with
      | Some intf ->
          decls_of_signature ~cmt shape index intf.Cmi_format.cmi_sign
      | None -> []
    in
    { m_name = unit_name; m_path = path; m_decls }

  let compute (cmts : Ocaml_shape_utils.cmt list) (index : Ocaml_index_utils.t)
      =
    List.map (compute_module index) cmts

  let pf ppf fmt = Format.fprintf ppf fmt

  let pp_occur_modules ppf paths =
    if Fpath.Set.cardinal paths >= 5 then pf ppf ".."
    else
      let pp_sep ppf () = pf ppf ",@ " in
      Format.pp_print_list ~pp_sep Fpath.pp ppf (Fpath.Set.elements paths)

  let pp_occurrences ppf = function
    | Some (n_occurs, path_occurs) ->
        let n_paths = Fpath.Set.cardinal path_occurs in
        pf ppf "%d occurrences in %d modules:@ @[<hov> %a@]" n_occurs n_paths
          pp_occur_modules path_occurs
    | None -> pf ppf "no occurrences found"

  let rec pp_decl ~max_width ppf d =
    pf ppf "@ @[<v 2>@[<hv 4>%-6s %-*s %a@]%a@]" (Kind.to_string d.d_kind)
      max_width d.d_ident pp_occurrences d.d_occur pp_decls d.d_subdecls

  and pp_decls ppf ds =
    let max_width =
      List.fold_left (fun acc d -> max acc (String.length d.d_ident)) 0 ds
    in
    List.iter (pp_decl ~max_width ppf) ds

  let pp ppf t =
    List.iter
      (fun m ->
        pf ppf "@[<v 2>module %s (at %s)%a@]@\n" m.m_name m.m_path pp_decls
          m.m_decls)
      t
end
