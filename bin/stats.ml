open Ocaml_parsing

type occs = ((string * string) * Longident.t Location.loc) list
(** See {!Ocaml_index_utils}. *)

module SM = Map.Make (String)

module Per_function_per_module = struct
  type t = (int * Fpath.Set.t) SM.t SM.t

  let get_fun_map mod_map unit =
    try SM.find unit mod_map with Not_found -> SM.empty

  let get_fun fun_map fun_name =
    try SM.find fun_name fun_map with Not_found -> (0, Fpath.Set.empty)

  let compute (occs : occs) : t =
    List.fold_left
      (fun mod_map ((unit, fun_name), { Location.txt = _; loc }) ->
        let fun_map = get_fun_map mod_map unit in
        let n, occ_l = get_fun fun_map fun_name in
        let fun_map =
          SM.add fun_name
            ( n + 1,
              Fpath.Set.add (Fpath.v loc.Location.loc_start.pos_fname) occ_l )
            fun_map
        in
        SM.add unit fun_map mod_map)
      SM.empty occs

  let pp ppf t =
    SM.iter
      (fun mod_name fun_map ->
        Format.fprintf ppf "@[<v 2>Occurrences of module %s:" mod_name;
        SM.iter
          (fun fun_name (n, occ_l) ->
            let occ_n = Fpath.Set.cardinal occ_l in
            Format.fprintf ppf "@ @[<hv 2>- %-24s %d occurrences in %d module"
              fun_name n occ_n;
            if occ_n < 5 then Format.fprintf ppf "@ %a" Fpath.Set.dump occ_l;
            Format.fprintf ppf "@]")
          fun_map;
        Format.fprintf ppf "@]@ ")
      t
end
