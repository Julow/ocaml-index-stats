let root_dir = "."

(** Scan and load all the [.cmt] and [.cmti] files in Dune's [_build] dir.
    Returns the list of loaded [.cmt] files and mappings from each [.cmt]'s path
    to the corresponding loaded [.cmti]. A [.cmti] with no corresponding [.cmt]
    is treated as a [.cmt] because it contain both declarations and definitions.
*)
let scan_local_cmts ~dune_build_dir =
  let descend_into p =
    match Filename.basename p with
    | ".actions" | ".merlin-conf" | ".formatted" -> false
    | _ -> true
  in
  let corresponding_cmt cmts cmti_f =
    match cmts with
    | prev :: _
      when Filename.remove_extension
             (Fpath.to_string prev.Ocaml_shape_utils.path)
           = Filename.remove_extension cmti_f ->
        Some prev
    | _ -> None
  in
  let read f = Ocaml_shape_utils.cmt_of_path (Fpath.v f) in
  Fs_utils.scan_dir ~descend_into
    (fun ((cmts, cmtis) as acc) f ->
      match Filename.extension f with
      | ".cmti" -> (
          (* Directories are scanned in lexicographically sorted order to avoid
             non reproducible output. We take advantage of this to group cmtis
             and cmts. *)
          match (read f, corresponding_cmt cmts f) with
          | Some cmti, Some corresp_cmt ->
              (cmts, Fpath.Map.add corresp_cmt.path cmti cmtis)
          | Some cmti, None -> (cmti :: cmts, Fpath.Map.add cmti.path cmti cmtis)
          | None, _ -> acc)
      | ".cmt" -> (
          match read f with Some cmt -> (cmt :: cmts, cmtis) | None -> acc)
      | _ -> acc)
    ([], Fpath.Map.empty) dune_build_dir

let () =
  let dune_build_dir = Filename.concat root_dir "_build" in
  let cmts, cmtis = scan_local_cmts ~dune_build_dir in
  let occs = Ocaml_index_utils.occurrences ~dune_build_dir ~cmts in
  let stats = Stats.Per_declaration.compute cmts cmtis occs in
  Format.printf "%d occurrences in %d modules@\n@\n%a" (List.length occs)
    (List.length cmts) Stats.Per_declaration.pp stats
