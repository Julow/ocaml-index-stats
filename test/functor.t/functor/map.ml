module type S = sig
  type 'a t
  type key

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
end

module Make (K : sig
  type t
end) =
struct
  type 'a t = (K.t * 'a) list
  type key = K.t

  let empty = []
  let add k v m = (k, v) :: m
end
