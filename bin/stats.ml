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
    d_occur : [ `Occur of int * Fpath.Set.t | `No_uid | `No_occur ];
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

  let rec decl_of_sig_item ~cmt index item =
    let mk d_kind ident uid d_subdecls =
      let d_ident = Ident.name ident in
      let d_occur =
        match
          Ocaml_shape_utils.Def_to_decl.find uid
            cmt.Ocaml_shape_utils.def_to_decl
        with
        | Some uid ->
            let n, p = compute_occurrences ~cmt index uid in
            if n = 0 then `No_occur else `Occur (n, p)
        | None ->
            Format.eprintf "%a -/> .@\n" Shape.Uid.print uid;
            `No_uid
      in
      Some { d_ident; d_occur; d_kind; d_subdecls }
    in
    match item with
    | _ when Types.item_visibility item = Hidden -> None
    | Types.Sig_value (ident, d, _) -> mk `Value ident d.val_uid []
    | Sig_type (ident, d, _, _) -> mk `Type ident d.type_uid []
    | Sig_typext (ident, d, _, _) -> mk `Typext ident d.ext_uid []
    | Sig_module (ident, _, d, _, _) ->
        (* Ignore [Mp_absent]. *)
        let subdecls = decls_of_module_type ~cmt index d.md_type in
        mk `Module ident d.md_uid subdecls
    | Sig_modtype (ident, d, _) ->
        let subdecls =
          match d.mtd_type with
          | Some mt -> decls_of_module_type ~cmt index mt
          | None -> []
        in
        mk `Modtype ident d.mtd_uid subdecls
    | Sig_class (ident, d, _, _) -> mk `Class ident d.cty_uid []
    | Sig_class_type (ident, d, _, _) -> mk `Class_type ident d.clty_uid []

  and decls_of_module_type ~cmt index = function
    | Types.Mty_signature sg -> decls_of_signature ~cmt index sg
    | Mty_functor (_, mt) -> decls_of_module_type ~cmt index mt
    | _ -> []

  and decls_of_signature ~cmt index sg =
    List.filter_map (decl_of_sig_item ~cmt index) sg

  let filter_module = function
    | Some m ->
        List.filter (function
          | { d_ident; d_kind = `Module; _ } -> d_ident = m
          | _ -> false)
    | None -> fun d -> d

  let compute_module index
      (({ Ocaml_shape_utils.unit_name; path; intf; _ } as cmt), module_) =
    let m_decls =
      match intf with
      | Some intf ->
          decls_of_signature ~cmt index intf.Cmi_format.cmi_sign
          |> filter_module module_
      | None -> []
    in
    { m_name = unit_name; m_path = path; m_decls }

  let compute (cmts : (Ocaml_shape_utils.cmt * string option) list)
      (index : Ocaml_index_utils.t) =
    List.map (compute_module index) cmts

  let pf ppf fmt = Format.fprintf ppf fmt

  let pp_occurrences ppf = function
    | `Occur (n_occurs, path_occurs) ->
        let n_paths = Fpath.Set.cardinal path_occurs in
        pf ppf "%d occurrences in %d modules" n_occurs n_paths;
        if n_paths > 4 then pf ppf ": .."
        else if n_paths > 0 then
          let pp_sep ppf () = pf ppf ",@ " in
          pf ppf ":@ @[<hov>%a@]"
            (Format.pp_print_list ~pp_sep Fpath.pp)
            (Fpath.Set.elements path_occurs)
    | `No_occur -> pf ppf "no occurrences found"
    | `No_uid -> pf ppf "no definition found"

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
