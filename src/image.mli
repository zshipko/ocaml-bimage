open Type

type ('a, 'b, 'c) t = {
  width : int;
  height : int;
  color : 'c Color.t;
  ty : ('a, 'b) Type.t;
  data : ('a, 'b) Data.t;
}
(** Image type *)

type any =
  | Any : ('a, 'b, 'c) t -> any
      (** Generic image type, used for input parameters *)

val any : ('a, 'b, 'c) t -> any
(** Make an image generic *)

val v : ('a, 'b) Type.t -> 'c Color.t -> int -> int -> ('a, 'b, 'c) t
(** [v ty color width height] makes a new image with the given [ty], [color] and dimensions *)

val random : ('a, 'b) Type.t -> 'c Color.t -> int -> int -> ('a, 'b, 'c) t
val compare : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> int
val equal : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> bool

val data : ('a, 'b, 'c) t -> ('a, 'b) Data.t
(** Get image data *)

val of_data : 'c Color.t -> int -> int -> ('a, 'b) Data.t -> ('a, 'b, 'c) t
(** [of_data color width height data] makes a new image from existing image data with the given [ty], [color], and dimensions *)

val like : ('a, 'b, 'c) t -> ('a, 'b, 'c) t
(** [like img] creates a new image with the same dimensions, color and ty as [img] *)

val like_with_size : ('a, 'b, 'c) t -> int -> int -> ('a, 'b, 'c) t

val like_with_color : 'd Color.t -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t
(** Create an image with the same dimensions and type *)

val like_with_ty : ('d, 'e) Type.t -> ('a, 'b, 'c) t -> ('d, 'e, 'c) t
(** Create an image with the name dimensions and color *)

val copy : ('a, 'b, 'c) t -> ('a, 'b, 'c) t
(** Makes a copy of an image and underlying image data *)

val copy_to : dest:('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit
(** Copy pixels from one image to another *)

val channels : ('a, 'b, 'c) t -> int
(** Returns the number of channels in an image *)

val length : ('a, 'b, 'c) t -> int
(** Returns the number of values contained in an image *)

val ty : ('a, 'b, 'c) t -> ('a, 'b) Type.t
(** Returns the image ty *)

val color : ('a, 'b, 'c) t -> 'c Color.t
(** Returns the image color type *)

val shape : ('a, 'b, 'c) t -> int * int * int
(** Returns the width, height and channels *)

val convert_to : dest:('d, 'e, 'f) t -> ('a, 'b, 'c) t -> unit
(** Convert an image to an existing image of another ty *)

val convert : ('d, 'e) Type.t -> 'f Color.t -> ('a, 'b, 'c) t -> ('d, 'e, 'f) t
(** Convert an image to a new image of another ty *)

val get : ('a, 'b, 'c) t -> int -> int -> int -> 'a
(** [get image x y c] returns a the value at (x, y, c) *)

val set : ('a, 'b, 'c) t -> int -> int -> int -> 'a -> unit
(** Set a single channel of the given image at (x, y) *)

val get_f : ('a, 'b, 'c) t -> int -> int -> int -> float
(** [get_f image x y c] returns the normalized float value at (x, y, c) *)

val set_f : ('a, 'b, 'c) t -> int -> int -> int -> float -> unit
(** Set a single channel of the given image at (x, y) using a normalized float value *)

val get_pixel : ('a, 'b, 'c) t -> ?dest:'c Pixel.t -> int -> int -> 'c Pixel.t
(** [get_pixel image x y] returns a pixel representation of [image] data at ([x], [y]) *)

val set_pixel : ('a, 'b, 'c) t -> int -> int -> 'c Pixel.t -> unit
(** [set_pixel image x y px] sets the value of [image] at ([x], [y]) to [px] *)

val get_data :
  ('a, 'b, 'c) t -> ?dest:('a, 'b) Data.t -> int -> int -> ('a, 'b) Data.t
(** [get_data image x y] returns [image] data at ([x], [y]) *)

val set_data : ('a, 'b, 'c) t -> int -> int -> ('a, 'b) Data.t -> unit
(** [set_data image x y px] sets the value of [image] at ([x], [y]) to [px] *)

val for_each :
  (int -> int -> ('a, 'b) Data.t -> unit) ->
  ?x:int ->
  ?y:int ->
  ?width:int ->
  ?height:int ->
  ('a, 'b, 'c) t ->
  unit
(** Iterate over each pixel in an image, or a rectangle segment of an image specified by [x], [y], [width],
      and [height]. The data segment used in the callback is mutable and will write directly to the underlying
      image data. *)

val for_each_pixel :
  (int -> int -> 'c Pixel.t -> unit) ->
  ?x:int ->
  ?y:int ->
  ?width:int ->
  ?height:int ->
  ('a, 'b, 'c) t ->
  unit
(** Iterate over each pixel in an image *)

val avg :
  ?x:int ->
  ?y:int ->
  ?width:int ->
  ?height:int ->
  ('a, 'b, 'c) t ->
  (float, f64) Data.t
(** Get the average pixel of an image or region of an image *)

val crop :
  ('a, 'b, 'c) t -> x:int -> y:int -> width:int -> height:int -> ('a, 'b, 'c) t
(** Extract the sub-image specified by the given dimensions *)

val mean_std : ?channel:int -> ('a, 'b, 'c) t -> float * float
(** Calculate the mean and standard deviation of an image *)

val fold :
  (int -> int -> ('a, 'b) Data.t -> 'd -> 'd) -> ('a, 'b, 'c) t -> 'd -> 'd

val fold2 :
  (int -> int -> ('a, 'b) Data.t -> ('e, 'f) Data.t -> 'd -> 'd) ->
  ('a, 'b, 'c) t ->
  ('e, 'f, 'c) t ->
  'd ->
  'd

val map_inplace : ('a -> 'a) -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
val map : ('a -> 'a) -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t

val map2_inplace :
  ('a -> 'd -> 'a) -> ('a, 'b, 'c) t -> ('d, 'e, 'f) t -> ('a, 'b, 'c) t

val map2 :
  ('a -> 'd -> 'a) -> ('a, 'b, 'c) t -> ('d, 'e, 'f) t -> ('a, 'b, 'c) t

module Diff : sig
  type diff

  val apply : diff -> ('a, 'b, 'c) t -> unit
  val length : diff -> int
end

val diff : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> Diff.diff
