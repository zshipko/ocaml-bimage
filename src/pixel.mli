open Color

type 'a t

val empty : 'a Color.t -> 'a t
(** Create a new pixel with all channels set to 0 *)

val v : 'a Color.t -> float list -> 'a t
(** Create a new pixel filled using the values from a list *)

val fill : 'a t -> float -> unit

val length : 'a t -> int
(** Get the number of channels in a pixel *)

val get : 'a t -> int -> float
(** Get value at index *)

val set : 'a t -> int -> float -> unit
(** Set value at index *)

val compare : 'a t -> 'a t -> int
(** Compare two pixels *)

val equal : 'a t -> 'a t -> bool
(** Returns true when two pixels are equal *)

val of_data : 'c Color.t -> ('a, 'b) Data.t -> 'c t
(** Create a new pixel from existing image data *)

val to_data : dest:('a, 'b) Data.t -> 'c t -> unit
(** Copy pixel data to existing image data *)

val data : 'a t -> floatarray
(** Returns the underlying pixel data *)

val color : 'a t -> 'a Color.t
(** Get pixel color *)

val to_rgb : 'a t -> rgb t
(** Convert pixel to RGB *)

val of_rgb : 'a Color.t -> rgb t -> 'a t
(** Convert pixel from RGB *)

val iter : (int -> float -> unit) -> 'a t -> unit
(** Iterate over pixel values *)

val map : ?ignore_alpha:bool -> (float -> float) -> 'a t -> 'a t
(** [map f x] executes [f] for each value in [x], returning a new Pixel.t *)

val map_inplace : ?ignore_alpha:bool -> (float -> float) -> 'a t -> 'a t
(** [map_inplace f x] executes [f] for each value in [x], assigning the new value to the same
   *  index *)

val map2_inplace :
  ?ignore_alpha:bool -> (float -> float -> float) -> 'a t -> 'a t -> 'a t
(** Executes a function over each item in two pixels *)

val map2 :
  ?ignore_alpha:bool -> (float -> float -> float) -> 'a t -> 'a t -> 'a t
(** [map2 f x y] executes [f] for each value in [x] and [y], returning a new Pixel.t *)

val fold : ?ignore_alpha:bool -> (float -> 'a -> 'a) -> 'b t -> 'a -> 'a
(** Reduction over a pixel *)

val clamp : 'a t -> 'a t

val pp : Format.formatter -> 'a t -> unit

module Infix : sig
  val ( + ) : 'a t -> 'a t -> 'a t

  val ( - ) : 'a t -> 'a t -> 'a t

  val ( * ) : 'a t -> 'a t -> 'a t

  val ( / ) : 'a t -> 'a t -> 'a t

  val ( +@ ) : 'a t -> float -> 'a t

  val ( -@ ) : 'a t -> float -> 'a t

  val ( *@ ) : 'a t -> float -> 'a t

  val ( /@ ) : 'a t -> float -> 'a t
end
