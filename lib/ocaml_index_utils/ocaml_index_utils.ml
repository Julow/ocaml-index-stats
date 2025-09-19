(* open Ocaml_parsing *)

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

let uid_of_unit_name u =
  let open Ocaml_typing in
  Shape.Uid.of_compilation_unit_id (Ident.create_persistent u)

(** For a UID, lookup the corresponding unit and identifier. *)
let lookup_ident ~cmts =
  let open Ocaml_typing in
  let module Tbl = Shape.Uid.Tbl in
  let tbl = Tbl.create 256 in
  List.iter
    (fun cmt ->
      let unit_name = Ocaml_shape_utils.unit_name cmt in
      Tbl.replace tbl (uid_of_unit_name unit_name) (`Found (unit_name, ""));
      List.iter
        (fun (uid, ident, _decl) ->
          let res =
            match ident with
            | Some ident -> `Found (unit_name, Ident.name ident)
            | None -> `Ignore
          in
          Tbl.replace tbl uid res)
        (Ocaml_shape_utils.declarations cmt))
    cmts;
  fun uid ->
    match Tbl.find_opt tbl uid with
    | Some ((`Found _ | `Ignore) as r) -> r
    | None -> `Not_found

(* let pp_ocaml_lid ppf lid = *)
(*   let { Location.txt; loc } = Compat.merlin_lid lid in *)
(* Format.fprintf ppf "@[<hv 2>%a:@ %a@]" Pprintast.longident txt
   Location.print_loc loc *)

(* Read the index files in [paths] and extract occurrences informations for
   declarations that match [lookup_ident]. *)
let extract_occurrences_of_unit ~lookup_ident paths =
  let open Merlin_index_format.Index_format in
  let is_unit_we_care_about u =
    match lookup_ident (uid_of_unit_name u) with `Found _ -> true | _ -> false
  in
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
                  | Item it when is_unit_we_care_about it.comp_unit ->
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

let occurrences ~dune_build_dir ~cmts =
  let paths = scan_dune_build_path ~dune_build_dir in
  let lookup_ident = lookup_ident ~cmts in
  extract_occurrences_of_unit ~lookup_ident paths
