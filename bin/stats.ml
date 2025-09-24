open Ocaml_parsing
open Ocaml_typing
module SM = Map.Make (String)

module Per_declaration = struct
  module Occ = struct
    type t = string * string

    let hash = Hashtbl.hash
    let equal = Stdlib.( = )
  end

  module OccTbl = Hashtbl.Make (Occ)

  type decl = {
    d_ident : string;
    d_uid : Shape.Uid.t;
    d_occur : (int * Fpath.Set.t) option;
    d_decl : Typedtree.item_declaration;
  }

  type module_ = {
    m_name : string;
    m_path : Fpath.t;
    m_decls : decl list;
    m_intf : Ocaml_shape_utils.cmt;
  }

  type t = module_ list

  let compute_decl unit_name occ_numbers (uid, ident_opt, tdecl) =
    let d_ident =
      match ident_opt with
      | Some id -> Ident.name id
      | None -> Format.asprintf "%a" Shape.Uid.print uid
    in
    let d_occur = OccTbl.find_opt occ_numbers (unit_name, d_ident) in
    { d_ident; d_uid = uid; d_occur; d_decl = tdecl }

  let compute_module cmtis occ_numbers
      ({ Ocaml_shape_utils.unit_name; path; decls } as cmt) =
    let m_decls = List.map (compute_decl unit_name occ_numbers) decls in
    let m_intf = Option.value ~default:cmt (Fpath.Map.find path cmtis) in
    { m_name = unit_name; m_path = path; m_decls; m_intf }

  let compute_occurs_number occs =
    let occ_numbers = OccTbl.create 64 in
    List.iter
      (fun (k, loc) ->
        let n, paths =
          try OccTbl.find occ_numbers k with Not_found -> (0, Fpath.Set.empty)
        in
        let p = Fpath.v loc.Location.loc.loc_start.pos_fname in
        OccTbl.replace occ_numbers k (n + 1, Fpath.Set.add p paths))
      occs;
    occ_numbers

  let compute (cmts : Ocaml_shape_utils.cmt list)
      (occs : Ocaml_index_utils.occurrences) =
    let occ_numbers = compute_occurs_number occs in
    List.map (compute_module occ_numbers) cmts

  let pf ppf fmt = Format.fprintf ppf fmt
  let pp_noop ppf _ = pf ppf ".."

  let pp_occurrences ppf = function
    | Some (n_occurs, path_occurs) ->
        let n_paths = Fpath.Set.cardinal path_occurs in
        pf ppf "%d occurrences in %d modules: %a" n_occurs n_paths
          (* (if n_paths < 5 then Fpath.Set.dump else pp_noop) *)
          pp_noop path_occurs
    | None -> pf ppf "no occurrences found"

  let pp_decl ~max_width ppf d =
    pf ppf "@[<hv 2>%6s %-*s @[<hov>(uid %a) %a@]@]"
      (Ocaml_shape_utils.Decl.decl_kind_to_string d.d_decl)
      max_width d.d_ident Shape.Uid.print d.d_uid pp_occurrences d.d_occur

  let pp ppf t =
    List.iter
      (fun m ->
        let max_width =
          List.fold_left
            (fun acc d -> max acc (String.length d.d_ident))
            0 m.m_decls
        in
        pf ppf "@[<v 2>module %s (at %a)@ %a@]@," m.m_name Fpath.pp m.m_path
          (Format.pp_print_list (pp_decl ~max_width))
          m.m_decls;
        pf ppf "@[<v 2>Intf:@ %a@]@," Ocaml_shape_utils.pp m.m_intf;
      ())
      t
end
