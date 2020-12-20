open Bigarray

type ('a, 'b) t = ('a, 'b, c_layout) Array1.t
(** Data type *)

val ty : ('a, 'b) t -> ('a, 'b) Type.t
(** Get the [Bigarray.ty] *)

val of_array : ('a, 'b) Type.t -> 'a array -> ('a, 'b) t
(** Converts an array to a [Data.t] of the given ty *)

val to_array : ('a, 'b) t -> 'a array
(** Converts a [Data.t] to an array *)

val v : ('a, 'b) Type.t -> int -> ('a, 'b) t
(** Create a new [Data.t] with the given length. *)

val random : ('a, 'b) Type.t -> int -> ('a, 'b) t

val length : ('a, 'b) t -> int
(** Returns the number of elements in a [Data.t] *)

val fold2 : ('a -> 'd -> 'c -> 'c) -> ('a, 'b) t -> ('d, 'e) t -> 'c -> 'c
(** Reduce over two [Data.t] *)

val fold : ('a -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
(** Reduce over a single [Data.t] *)

val fill : ('a, 'b) t -> 'a -> unit
(** [fill d x] sets each value of [d] to [x] *)

val map_inplace : ('a -> 'a) -> ('a, 'b) t -> ('a, 'b) t
(** [map_inplace f data] runs [f] over each value of [data] *)

val map2_inplace : ('a -> 'c -> 'a) -> ('a, 'b) t -> ('c, 'd) t -> ('a, 'b) t
(** [map2_inplace f data1 data2] runs [f] over each value of [data1] and [data2] *)

val slice : offs:int -> length:int -> ('a, 'b) t -> ('a, 'b) t
(** [slice ~offs ~length data] extracts a section of [data] of [length]
      values starting at index [offs] *)

val copy_to : dest:('a, 'b) t -> ('a, 'b) t -> unit
(** [copy_to ~dest src] copies each value from [src] to [dest] *)

val copy : ('a, 'b) t -> ('a, 'b) t
(** Create a new copy of [Data.t] *)

val convert : ('c, 'd) Type.t -> ('a -> 'c) -> ('a, 'b) t -> ('c, 'd) t
(** Convert between [Data.t] types *)

val convert_to : ('a -> 'c) -> dest:('c, 'd) t -> ('a, 'b) t -> unit
(** Convert between [Data.t] types with an existing destination image *)

val hash : ('a, 'b) t -> int
(** Default hash function *)

val compare : ('a, 'b) t -> ('a, 'b) t -> int
(** Default comparison function *)

val equal : ('a, 'b) t -> ('a, 'b) t -> bool
(** Default equality function *)
