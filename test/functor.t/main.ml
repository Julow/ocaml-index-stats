open Functor

module M = Map.Make (struct
  type t = int
end)

let of_list = List.fold_left (fun acc (k, v) -> M.add k v acc) M.empty
let _ = of_list [ (1, 2) ]
