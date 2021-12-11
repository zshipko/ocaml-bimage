type t = float array array

val v : rows:int -> cols:int -> t
(** [v ~rows ~cols] makes a new Kernel with the given dimensions *)

val rows : t -> int
(** Returns the number of rows in a kernel *)

val cols : t -> int
(** Returns the number of columns in a kernel *)

val combine : (float -> float -> float) -> t -> t -> t
(** Joins two kernels using the given operation *)

val of_array : ?norm:bool -> float array array -> t
(** Create a kernel from an existing 2-dimensional float array. When [norm] is true,
      the kernel will be normalized *)

val to_array : t -> float array array
(** Convert a kernel to a 2-dimensional float array *)

val get : t -> int -> int -> float
(** [get kernel y x] gets the value at (x, y) *)

val set : t -> int -> int -> float -> unit
(** [set kernel y x v] sets the value at (x, y) *)

val sum : t -> float
(** Get the sum of each value of a kernel *)

val normalize : t -> t
(** [normalize kernel] returns a kernel where each element has been divided by the sum of all elements *)

val sobel_x : t
(** Sobel kernel in the X direction onlu *)

val sobel_y : t
(** Sobel kernel in the Y direction only *)

val gaussian : ?std:float -> int -> t
(** [gassian n] generates a new [n]x[n] gaussian kernel *)

module Infix : sig
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
end
