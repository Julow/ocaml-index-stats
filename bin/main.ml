let root_dir = "."

(* Prefer [.cmti]s over [.cmt]s because we care about the declarations that
   appear in the interface. *)
let scan_local_cmtis ~dune_build_dir =
  let descend_into p =
    match Filename.basename p with
    | ".actions" | ".merlin-conf" | ".formatted" -> false
    | _ -> true
  in
  let remove_cmt_of_cmti acc f =
    match acc with
    | prev :: tl
      when Filename.remove_extension
             (Fpath.to_string prev.Ocaml_shape_utils.path)
           = Filename.remove_extension f ->
        tl
    | _ -> acc
  in
  let read acc f =
    match Ocaml_shape_utils.cmt_of_path (Fpath.v f) with
    | Some cmt -> cmt :: acc
    | None -> acc
  in
  Fs_utils.scan_dir ~descend_into
    (fun acc f ->
      match Filename.extension f with
      | ".cmti" ->
          (* Directories are scanned in lexicographically sorted order to avoid
             non reproducible output. We take advantage of this to make cmtis
             take precedence over cmts. *)
          read (remove_cmt_of_cmti acc f) f
      | ".cmt" -> read acc f
      | _ -> acc)
    [] dune_build_dir

let () =
  let dune_build_dir = Filename.concat root_dir "_build" in
  let cmts = scan_local_cmtis ~dune_build_dir in
  let occs = Ocaml_index_utils.occurrences ~dune_build_dir ~cmts in
  Format.printf "%d occurrences of %d units\n%!" (List.length occs)
    (List.length cmts);
  Stats.Per_declaration.(compute cmts occs |> pp Format.std_formatter)
