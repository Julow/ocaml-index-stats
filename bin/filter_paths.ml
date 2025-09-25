type t = Fpath.t -> bool

let fail = Format.kasprintf failwith

let make paths =
  let cwd = Fpath.v (Sys.getcwd ()) in
  let norm p =
    if Fpath.is_abs p then
      match Fpath.relativize ~root:cwd p with
      | Some p -> p
      | None -> fail "Path %a is not in the project" Fpath.pp p
    else Fpath.normalize p
  in
  let paths =
    List.fold_left
      (fun acc p -> Fpath.Set.add (norm p) acc)
      Fpath.Set.empty paths
  in
  let rec query_path p =
    Fpath.Set.mem p paths
    || (not (Fpath.is_current_dir ~prefix:true p))
       && query_path (Fpath.parent p)
  in
  let query p =
    query_path p || Fpath.Set.mem (Fpath.set_ext ~multi:true ".ml" p) paths
    (* Match extensions like ".pp.ml". *)
  in
  query

let make paths = if paths = [] then fun _ -> true else make paths
