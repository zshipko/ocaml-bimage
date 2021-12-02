type t = Image.any array

type index = int

val empty : t

val append : t -> ('a, 'b, 'c) Image.t -> t

val or_default : index option -> index
(** Returns the provided index, if not [None] or the default index *)

val get : t -> index -> Image.any
(** Get an image from the input, raising [Error.Exc (`Invalid_input index)]
      if the provided index is out of bounds. *)

val shape : t -> int * int * int
(** Get input shape *)

val of_image : ('a, 'b, 'c) Image.t -> t
