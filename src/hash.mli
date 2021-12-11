type t

module Set : Set.S with type elt = t

val phash : ('a, 'b, 'c) Image.t -> t
val equal : t -> t -> bool
val to_string : t -> string
val to_int64 : t -> int64
