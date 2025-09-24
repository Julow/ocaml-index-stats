let root_dir = "."

(** Scan and load all the [.cmt] and [.cmti] files in Dune's [_build] dir. Pass
    [~read_cmti:true] so that we can use the interface to filter-out internal
    identifiers. *)
let scan_local_cmts ~dune_build_dir =
  let descend_into p =
    match Filename.basename p with
    | ".actions" | ".merlin-conf" | ".formatted" -> false
    | _ -> true
  in
  let read f = Ocaml_shape_utils.cmt_of_path ~read_cmti:true (Fpath.v f) in
  Fs_utils.scan_dir ~descend_into
    (fun acc f ->
      match Filename.extension f with
      | ".cmt" -> ( match read f with Some cmt -> cmt :: acc | None -> acc)
      | _ -> acc)
    [] dune_build_dir

let () =
  let dune_build_dir = Filename.concat root_dir "_build" in
  let cmts = scan_local_cmts ~dune_build_dir in
  let occs = Ocaml_index_utils.occurrences ~dune_build_dir ~cmts in
  let stats = Stats.Per_declaration.compute cmts occs in
  Format.printf "%d occurrences in %d modules@\n@\n%a" (List.length occs)
    (List.length cmts) Stats.Per_declaration.pp stats
