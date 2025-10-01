module type S = sig
  type 'a t
  type key

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
end

module Make (K : sig
  type t
end) : S with type key = K.t
