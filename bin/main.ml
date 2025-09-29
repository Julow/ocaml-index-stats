let fail = Format.kasprintf failwith
let root_dir = "."

(** Scan and load all the [.cmt] and [.cmti] files in the given directory tree.
    Pass [~read_cmti:true] so that we can use the interface to filter-out
    internal identifiers. *)
let scan_cmts_in_dir ?module_ p =
  let descend_into p =
    match Filename.basename p with
    | ".actions" | ".merlin-conf" | ".formatted" -> false
    | _ -> true
  in
  let read f = Ocaml_shape_utils.cmt_of_path ~read_cmti:true (Fpath.v f) in
  Fs_utils.scan_dir ~descend_into
    (fun acc f ->
      match Filename.extension f with
      | ".cmt" -> (
          match read f with Some cmt -> (cmt, module_) :: acc | _ -> acc)
      | _ -> acc)
    [] (Fpath.to_string p)

let file_exists_and_is_dir p =
  try Sys.is_directory (Fpath.to_string p) with Sys_error _ -> false

(** Guess the path inside Dune's [_build] that correspond to path [p]. *)
let interpret_cli_path ~dune_build_dir ~cwd (p, module_) =
  let p =
    let p = Fpath.normalize p in
    if Fpath.is_abs p then
      match Fpath.rem_prefix cwd p with
      | Some p -> p
      | None -> fail "Path %a is not in the project" Fpath.pp p
    else p
  in
  let profile = Fpath.( / ) dune_build_dir "default" in
  if file_exists_and_is_dir p then
    scan_cmts_in_dir ?module_ (Fpath.( // ) profile p)
  else
    let p_str = Fpath.to_string p in
    scan_cmts_in_dir ?module_ Fpath.(profile // parent p)
    |> List.filter (fun (cmt, _) -> cmt.Ocaml_shape_utils.path = p_str)

(** Interpret the paths given on the command-line. *)
let interpret_cli_paths ~dune_build_dir paths =
  let dune_build_dir = Fpath.v dune_build_dir in
  let cwd = Fpath.v (Sys.getcwd ()) in
  if paths = [] then scan_cmts_in_dir dune_build_dir
  else List.concat_map (interpret_cli_path ~dune_build_dir ~cwd) paths

let main cli_paths =
  let dune_build_dir = Filename.concat root_dir "_build" in
  let cmts = interpret_cli_paths ~dune_build_dir cli_paths in
  let index = Ocaml_index_utils.scan_dune_build_dir ~dune_build_dir in
  let stats = Stats.Per_declaration.compute cmts index in
  Stats.Per_declaration.pp Format.std_formatter stats

open Cmdliner

let arg_paths =
  let path_module_conv =
    let parse s =
      let p, m =
        match String.index_opt s ':' with
        | Some i ->
            ( String.sub s 0 i,
              Some (String.sub s (i + 1) (String.length s - i - 1)) )
        | None -> (s, None)
      in
      Result.map (fun p -> (p, m)) (Fpath.of_string p)
    in
    let print ppf (p, m) =
      Format.(
        fprintf ppf "%a:%a" Fpath.pp p (pp_print_option pp_print_string) m)
    in
    Arg.conv ~docv:"PATH:MODULE" (parse, print)
  in
  let doc =
    "Paths to modules or to directory to look occurrences for. You can specify \
     a submodule with the syntax 'path/to/module.ml:Submodule'."
  in
  Arg.(value & pos_all path_module_conv [] & info ~doc [])

let cmd =
  let term = Term.(const main $ arg_paths) in
  let doc =
    "Print occurrences of every items in signatures. Make sure the indexes are \
     available with 'dune build @ocaml-index' first. Must be run with the \
     project's root as current directory."
  in
  let info = Cmd.info "ocaml-index-stats" ~version:"%%VERSION%%" ~doc in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
