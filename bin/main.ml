let root_dir = "."

(** Scan and load all the [.cmt] and [.cmti] files in Dune's [_build] dir. Pass
    [~read_cmti:true] so that we can use the interface to filter-out internal
    identifiers. *)
let scan_local_cmts ~filter_paths ~dune_build_dir =
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
          match read f with
          | Some cmt when filter_paths (Fpath.v cmt.path) -> cmt :: acc
          | _ -> acc)
      | _ -> acc)
    [] dune_build_dir

(* Process modules paths from the CLI. Turn .mli into .ml. *)
let module_path_of_string p =
  let p = Fpath.v p in
  if Fpath.has_ext ".mli" p then Fpath.set_ext ".ml" p else p

let main filter_paths =
  let dune_build_dir = Filename.concat root_dir "_build" in
  let filter_paths =
    Filter_paths.make (List.map module_path_of_string filter_paths)
  in
  let cmts = scan_local_cmts ~filter_paths ~dune_build_dir in
  let index = Ocaml_index_utils.scan_dune_build_dir ~dune_build_dir in
  let stats = Stats.Per_declaration.compute cmts index in
  Stats.Per_declaration.pp Format.std_formatter stats

open Cmdliner

let arg_paths =
  let doc = "Paths to modules or to directory to look occurrences for." in
  Arg.(value & pos_all file [] & info ~doc ~docv:"PATHS.." [])

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
