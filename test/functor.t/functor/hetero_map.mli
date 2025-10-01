(** Heterogeneous map top of [Map]. *)

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

module Make (K : K) : S with type 'a key = 'a K.key and type 'a data = 'a K.data
