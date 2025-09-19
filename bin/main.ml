let root_dir = "."

let scan_local_cmts ~dune_build_dir =
  let descend_into p =
    match Filename.basename p with
    | ".actions" | ".merlin-conf" | ".formatted" -> false
    | _ -> true
  in
  Fs_utils.scan_dir ~descend_into
    (fun acc f ->
      if Filename.extension f = ".cmt" then Fpath.v f :: acc else acc)
    [] dune_build_dir

let () =
  (* let open Merlin_index_format.Index_format in *)
  let dune_build_dir = Filename.concat root_dir "_build" in
  let cmts = scan_local_cmts ~dune_build_dir in
  let units _ = true in
  let occs = Ocaml_index_utils.occurrences ~dune_build_dir ~cmts ~units in
  Format.printf "%d occurrences of %d units\n%!" (List.length occs)
    (List.length cmts);
  Stats.Per_function_per_module.(compute occs |> pp Format.std_formatter)
