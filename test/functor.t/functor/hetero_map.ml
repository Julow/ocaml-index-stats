module type S = sig
  type t
  type 'a key
  type 'a data

  val empty : t
  val add : 'a key -> 'a data -> t -> t
end

module type K = sig
  type 'a key
  type 'a data
end

module Make (K : K) = struct
  type 'a key = 'a K.key
  type 'a data = 'a K.data
  type pair = Pair : 'a key * 'a data -> pair
  type t = pair list

  let empty = []
  let add k v m = Pair (k, v) :: m
end
