open! Ocaml_parsing
open! Ocaml_typing

type conf = {
  occpaths : [ `None | `Some | `All ];
  diroccurs : bool;
  skip_summary : bool;
  only_values : bool;
      (** Module and module types are kept but with occurrences data removed. *)
}

let conf ~occpaths ~diroccurs ~skip_summary ~only_values =
  { occpaths; diroccurs; skip_summary; only_values }

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

  type occurs = int * (Fpath.t * int) list * (Fpath.t * int) list
  (** Number of occurrences, occurrences per modules, occurrences per
      directories. *)

  type decl = {
    d_ident : string;
    d_occur :
      [ `Occur of occurs | `No_uid | `No_occur of Shape.Uid.t | `Occur_removed ];
    d_kind : Kind.t;
    d_subdecls : decl list;  (** For modules, module types, etc.. *)
    d_summary : occurs;  (** Aggregate occurrences of all the sub decls. *)
  }

  type module_ = {
    m_name : string;
    m_path : string;
    m_decls : decl list;
    m_summary : occurs;
  }

  type t = module_ list

  let fname_of_lid lid = Fpath.v lid.Location.loc.loc_start.pos_fname

  let occmap_incr incr map key =
    Fpath.Map.update key
      (function None -> Some incr | Some n -> Some (n + incr))
      map

  let occmap_to_list map =
    Fpath.Map.to_list map |> List.sort (fun (_, a) (_, b) -> -Int.compare a b)

  (** Count the occurrences per directory. *)
  let compute_directory_stats paths =
    let rec incr_parent acc p n =
      let p = Fpath.parent p in
      if Fpath.is_current_dir p then acc
      else incr_parent (occmap_incr n acc p) p n
    in
    List.fold_left (fun acc (p, n) -> incr_parent acc p n) Fpath.Map.empty paths
    |> occmap_to_list

  (** Count the occurrences per modules. *)
  let compute_path_stats occs =
    List.fold_left (occmap_incr 1) Fpath.Map.empty occs |> occmap_to_list

  (** Construct the summary for a decl containing other decls. *)
  let aggregate_summary decls =
    let module M = Fpath.Map in
    let union = M.union (fun _p a b -> Some (a + b)) in
    let n, paths =
      List.fold_left
        (fun (n, paths) decl ->
          let occ_n, occ_paths, _ =
            match decl.d_occur with `Occur occ -> occ | _ -> (0, [], [])
          in
          let sum_n, sum_paths, _ = decl.d_summary in
          ( occ_n + sum_n + n,
            union paths (union (M.of_list sum_paths) (M.of_list occ_paths)) ))
        (0, M.empty) decls
    in
    let paths = occmap_to_list paths in
    (n, paths, compute_directory_stats paths)

  (** Remove occurrences to that are from the same module. *)
  let remove_self_occurrences ~cmt =
    let cmt_path = Fpath.(rem_ext ~multi:true (v cmt.Ocaml_shape_utils.path)) in
    List.filter (fun p -> cmt_path <> Fpath.rem_ext ~multi:true p)

  let compute_occurrences ~cmt index uids =
    let occs =
      List.concat_map
        (fun uid ->
          Ocaml_index_utils.lookup_occurrences index uid
          |> List.map fname_of_lid
          |> remove_self_occurrences ~cmt)
        uids
    in
    let paths = compute_path_stats occs in
    let dirs = compute_directory_stats paths in
    (List.length occs, paths, dirs)

  let lookup_decl cmt uid =
    uid
    :: Ocaml_shape_utils.Def_to_decl.find uid cmt.Ocaml_shape_utils.def_to_decl

  let rec decl_of_sig_item conf ~cmt index item =
    let mk d_kind ident uid d_subdecls =
      let d_ident = Ident.name ident in
      let d_subdecls =
        if conf.only_values then
          List.filter
            (fun d ->
              match d.d_kind with
              | `Value | `Module | `Modtype -> true
              | _ -> false)
            d_subdecls
        else d_subdecls
      in
      let d_occur =
        if conf.only_values && d_kind <> `Value then `Occur_removed
        else
          match lookup_decl cmt uid with
          | [] -> `No_uid
          | uids ->
              let occ = compute_occurrences ~cmt index uids in
              let n, _, _ = occ in
              if n = 0 then `No_occur uid else `Occur occ
      in
      let d_summary = aggregate_summary d_subdecls in
      Some { d_ident; d_occur; d_kind; d_subdecls; d_summary }
    in
    match item with
    | _ when Types.item_visibility item = Hidden -> None
    | Types.Sig_value (ident, d, _) -> mk `Value ident d.val_uid []
    | Sig_type (ident, d, _, _) -> mk `Type ident d.type_uid []
    | Sig_typext (ident, d, _, _) -> mk `Typext ident d.ext_uid []
    | Sig_module (ident, _, d, _, _) ->
        (* Ignore [Mp_absent]. *)
        let subdecls = decls_of_module_type conf ~cmt index d.md_type in
        mk `Module ident d.md_uid subdecls
    | Sig_modtype (ident, d, _) ->
        let subdecls =
          match d.mtd_type with
          | Some mt -> decls_of_module_type conf ~cmt index mt
          | None -> []
        in
        mk `Modtype ident d.mtd_uid subdecls
    | Sig_class (ident, d, _, _) -> mk `Class ident d.cty_uid []
    | Sig_class_type (ident, d, _, _) -> mk `Class_type ident d.clty_uid []

  and decls_of_module_type conf ~cmt index = function
    | Types.Mty_signature sg -> decls_of_signature conf ~cmt index sg
    | Mty_functor (_, mt) -> decls_of_module_type conf ~cmt index mt
    | _ -> []

  and decls_of_signature conf ~cmt index sg =
    List.filter_map (decl_of_sig_item conf ~cmt index) sg

  let filter_module = function
    | Some m ->
        List.filter (function
          | { d_ident; d_kind = `Module; _ } -> d_ident = m
          | _ -> false)
    | None -> fun d -> d

  let compute_module conf index
      (({ Ocaml_shape_utils.unit_name; path; intf; _ } as cmt), module_) =
    let m_decls =
      match intf with
      | Some intf ->
          decls_of_signature conf ~cmt index intf.Cmi_format.cmi_sign
          |> filter_module module_
      | None -> []
    in
    let m_summary = aggregate_summary m_decls in
    { m_name = unit_name; m_path = path; m_decls; m_summary }

  let compute conf (cmts : (Ocaml_shape_utils.cmt * string option) list)
      (index : Ocaml_index_utils.t) =
    List.map (compute_module conf index) cmts

  let pf ppf fmt = Format.fprintf ppf fmt

  let rec pp_occurrences_paths max_depth ppf = function
    | [] -> ()
    | _ :: _ when max_depth <= 0 -> pf ppf ".."
    | (path, n_occ) :: tl ->
        pf ppf "%a (%d)" Fpath.pp path n_occ;
        if not (List.is_empty tl) then pf ppf ",@ ";
        pp_occurrences_paths (max_depth - 1) ppf tl

  let pp_occurrences conf ppf = function
    | `Occur (n_occurs, path_occurs, dir_occurs) ->
        let n_paths = List.length path_occurs in
        pf ppf "%d occurrences in %d modules" n_occurs n_paths;
        let max_depth =
          match conf.occpaths with
          | `None -> 0
          | `Some -> 8
          | `All -> Int.max_int
        in
        if n_paths > 0 && max_depth > 0 then
          pf ppf ":@;<1 2>@[<hov>%a@]"
            (pp_occurrences_paths max_depth)
            path_occurs;
        if conf.diroccurs then
          pf ppf "@ Occurs in directories:@;<1 2>@[<hov>%a@]"
            (pp_occurrences_paths max_depth)
            dir_occurs
    | `No_occur _uid -> pf ppf "no occurrences found"
    | `No_uid -> pf ppf "no definition found"
    | `Occur_removed -> ()

  let pp_summary conf ppf (n_occ, paths, dirs) =
    if (not conf.skip_summary) && n_occ > 0 then
      pf ppf
        "@;\
         <1 -2>@[<hv 0>Items of this module are used %d times in %d modules:@;\
         <1 2>@[<v>%a@]@ in these directories:@;\
         <1 2>@[<v>%a@]@]"
        n_occ (List.length paths)
        (pp_occurrences_paths Int.max_int)
        paths
        (pp_occurrences_paths Int.max_int)
        dirs
    else ()

  let rec pp_decl ~max_width conf ppf d =
    pf ppf "@ @[<v 2>@[<hv 4>%-6s %-*s %a@]%a%a@]" (Kind.to_string d.d_kind)
      max_width d.d_ident (pp_occurrences conf) d.d_occur (pp_decls conf)
      d.d_subdecls (pp_summary conf) d.d_summary

  and pp_decls conf ppf ds =
    let max_width =
      List.fold_left (fun acc d -> max acc (String.length d.d_ident)) 0 ds
    in
    List.iter (pp_decl ~max_width conf ppf) ds

  let pp conf ppf t =
    List.iter
      (fun m ->
        pf ppf "@[<v 2>module %s (at %s)%a%a@]@\n" m.m_name m.m_path
          (pp_decls conf) m.m_decls (pp_summary conf) m.m_summary)
      t
end
