open Ocaml_typing

let fail fmt = Format.kasprintf failwith fmt

module Decl = struct
  type t = Shape.Uid.t * Ident.t option * Typedtree.item_declaration

  let ident_of_decl = function
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
        Some ident
    | Value_binding { vb_pat = { pat_desc; _ }; _ } -> (
        match Compat.tpat_alias_ident pat_desc with
        | Some ident -> Some ident
        | None -> None)
    | Module { md_id = None; _ } | Module_binding { mb_id = None; _ } -> None

  let of_ocaml_decl uid d : t = (uid, ident_of_decl d, d)
end

type cmt = { unit_name : string; path : Fpath.t; decls : Decl.t list }

let read_cmt cmt : Decl.t list =
  let module Tbl = Shape.Uid.Tbl in
  let cmt = Cmt_format.read_cmt cmt in
  Tbl.fold
    (fun uid decl acc -> Decl.of_ocaml_decl uid decl :: acc)
    cmt.cmt_uid_to_decl []

(* Query a package's lib path using [ocamlfind]. *)
let package_lib_paths packages =
  let cmd = Filename.quote_command "ocamlfind" ("query" :: packages) in
  let ic = Unix.open_process_in cmd in
  let lib_paths = String.trim (In_channel.input_all ic) in
  match Unix.close_process_in ic with
  | WEXITED 0 -> List.map Fpath.v (String.split_on_char '\n' lib_paths)
  | _ -> fail "Command %S failed." cmd

let unit_name_of_path p =
  Fpath.basename p |> Filename.remove_extension |> String.capitalize_ascii

(* [.cmt] files in packages with names [packages]. Uses [ocamlfind]. *)
let cmts_of_packages ~packages ~units : cmt list =
  let acc_matching_cmts acc fname =
    let path = Fpath.v fname in
    let unit_name = unit_name_of_path path in
    if Filename.extension fname = ".cmt" && units unit_name then
      { unit_name; path; decls = read_cmt fname } :: acc
    else acc
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

let cmt_of_path path =
  let unit_name = unit_name_of_path path in
  try Some { unit_name; path; decls = read_cmt (Fpath.to_string path) }
  with _ -> None
