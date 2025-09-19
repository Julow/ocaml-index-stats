(* open Ocaml_parsing *)

type cmt = Fpath.t

let fail fmt = Printf.ksprintf failwith fmt

(* Find all the [.ocaml-index] files. *)
let scan_dune_build_path ~dune_build_dir =
  match Sys.is_directory dune_build_dir with
  | (exception Sys_error _) | false ->
      fail "Directory %S not found." dune_build_dir
  | true -> (
      let collect_index_files acc path =
        if Filename.extension path = ".ocaml-index" then path :: acc else acc
      in
      match Fs_utils.scan_dir collect_index_files [] dune_build_dir with
      | [] -> failwith "No index found. Please run 'dune build @ocaml-index'."
      | p -> p)

(* Query a package's lib path using [ocamlfind]. *)
let package_lib_paths packages =
  let cmd = Filename.quote_command "ocamlfind" ("query" :: packages) in
  let ic = Unix.open_process_in cmd in
  let lib_paths = String.trim (In_channel.input_all ic) in
  match Unix.close_process_in ic with
  | WEXITED 0 -> List.map Fpath.v (String.split_on_char '\n' lib_paths)
  | _ -> fail "Command %S failed." cmd

(* [.cmt] files in packages with names [packages]. Uses [ocamlfind]. *)
let cmts_of_packages ~packages =
  let acc_matching_cmts acc fname =
    if Filename.extension fname = ".cmt" then Fpath.v fname :: acc else acc
  in
  let cmts =
    package_lib_paths packages
    |> List.fold_left
         (fun acc lib_path ->
           Fs_utils.list_dir (Fpath.to_string lib_path)
           |> Array.fold_left acc_matching_cmts acc)
         []
  in
  if cmts = [] then
    fail "Found no [.cmt] in packages: %s" (String.concat ", " packages);
  cmts

let filter_units ~units cmts =
  List.fold_left
    (fun acc cmt ->
      let basename = Fpath.basename cmt in
      let unit_name =
        String.capitalize_ascii (Filename.remove_extension basename)
      in
      if units unit_name then (unit_name, cmt) :: acc else acc)
    [] cmts

(* Lookup the uid_map for a unit. *)
let uid_map_of_unit ~cmts =
  let open Ocaml_typing in
  let ident_of_decl ~unit_name = function
    | Typedtree.Value { val_id = ident; _ }
    | Type { typ_id = ident; _ }
    | Value_binding { vb_pat = { pat_desc = Tpat_var (ident, _, _); _ }; _ }
    | Constructor { cd_id = ident; _ }
    | Extension_constructor { ext_id = ident; _ }
    | Module { md_id = Some ident; _ }
    | Module_substitution { ms_id = ident; _ }
    | Module_binding { mb_id = Some ident; _ }
    | Module_type { mtd_id = ident; _ }
    | Class { ci_id_class = ident; _ }
    | Class_type { ci_id_class = ident; _ }
    | Label { ld_id = ident; _ } ->
        `Found (unit_name, Ident.name ident)
    | Value_binding { vb_pat = { pat_desc; _ }; _ } -> (
        match Compat.tpat_alias_ident pat_desc with
        | Some ident -> `Found (unit_name, Ident.name ident)
        | None -> `Ignore)
    | Module { md_id = None; _ } | Module_binding { mb_id = None; _ } -> `Ignore
  in
  let module Tbl = Shape.Uid.Tbl in
  let tbl = Tbl.create 256 in
  List.iter
    (fun (unit_name, cmt) ->
      let cmt = Cmt_format.read_cmt (Fpath.to_string cmt) in
      Tbl.replace tbl
        (Shape.Uid.of_compilation_unit_id (Ident.create_persistent unit_name))
        (`Found (unit_name, ""));
      Tbl.iter
        (fun uid decl -> Tbl.replace tbl uid (ident_of_decl ~unit_name decl))
        cmt.cmt_uid_to_decl)
    cmts;
  fun uid ->
    match Tbl.find_opt tbl uid with
    | Some ((`Found _ | `Ignore) as r) -> r
    | None -> `Not_found

(* let pp_ocaml_lid ppf lid = *)
(*   let { Location.txt; loc } = Compat.merlin_lid lid in *)
(* Format.fprintf ppf "@[<hv 2>%a:@ %a@]" Pprintast.longident txt
   Location.print_loc loc *)

(* Read the index files and extract all occurrences of [units]. *)
let extract_occurrences_of_unit ~units ~lookup_ident paths =
  let open Merlin_index_format.Index_format in
  if paths = [] then failwith "Found no index files in '_build'.";
  List.fold_left
    (fun acc file ->
      match read ~file with
      | Index index ->
          Uid_map.fold
            (fun uid locs acc ->
              match lookup_ident uid with
              | `Found ident ->
                  Lid_set.elements locs
                  |> List.fold_left
                       (fun acc lid -> (ident, Compat.merlin_lid lid) :: acc)
                       acc
              | `Ignore -> (* Not a value *) acc
              | `Not_found -> (
                  match uid with
                  | Item it when units it.comp_unit ->
                      Format.eprintf
                        "@[<hv 2>Warning:@ No ident for uid %s.%d:@ %d \
                         occurrences@]@\n\
                         %!"
                        it.comp_unit it.id (Lid_set.cardinal locs)
                      (* (Format.pp_print_list pp_ocaml_lid) *)
                      (* (Lid_set.elements locs) *);
                      acc
                  | _ -> acc))
            index.defs acc
      | _ -> acc)
    [] paths

let occurrences ~dune_build_dir ~cmts ~units =
  let paths = scan_dune_build_path ~dune_build_dir in
  let cmts = filter_units ~units cmts in
  let lookup_ident = uid_map_of_unit ~cmts in
  extract_occurrences_of_unit ~units ~lookup_ident paths
