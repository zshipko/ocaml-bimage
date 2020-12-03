(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Image processing library

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

open Bigarray
(** {1 Bimage} *)

module type TYPE = sig
  type t

  type elt

  type kind = (t, elt) Bigarray.kind

  val name : string

  val kind : kind

  val of_float : float -> t

  val to_float : t -> float
end

type ('a, 'b) kind = ('a, 'b) Bigarray.kind

exception Unsupported
(** Raised when attempting to use Bigarray types other than u8, u16, f32, f64, i32, i64*)

module Error : sig
  type t =
    [ `Invalid_shape
    | `Invalid_kernel_shape of int * int
    | `Invalid_input of int
    | `Invalid_color
    | `Msg of string ]

  exception Exc of t

  val exc : t -> 'a
  (** Raises an [Exc] with the provided [Error.t] *)

  val to_string : t -> string
  (** Returns a string representation of an [Error.t] *)

  val unwrap : ('a, t) result -> 'a
  (** A convenience function that returns the [Ok] value of a result if possible, otherwise
      it raises the [Error] value *)

  val string_of_exn : exn -> string
end

(** The Angle type is used instead of a float whenever a function expects an angle
      argument to avoid ambiguity *)
module Angle : sig
  type t

  val of_degrees : float -> t
  (** [of_degrees deg] creates new angle from [deg] degrees *)

  val to_degrees : t -> float
  (** [to_degrees angle] returns the value of the angle in degrees *)

  val of_radians : float -> t
  (** [of_radians rad] creates a new angle from [rad] radians *)

  val to_radians : t -> float
  (** [to_radians angle] returns the value of the angle in radians *)
end

(** Point is a 2 element float tuple used to perform calculations on (x, y) coordinates *)
module Point : sig
  type t = float * float

  val x : t -> float
  (** [x pt] extracts the x coordinate *)

  val y : t -> float
  (** [y pt] extracts the y coordinate *)
end

module type COLOR = sig
  type t

  val t : t

  val name : t -> string

  val channels : t -> int

  val has_alpha : t -> bool

  val to_rgb : t -> floatarray -> floatarray

  val from_rgb : t -> floatarray -> floatarray
end

(** Color contains methods for creating and inspecting color types *)
module Color : sig
  module Rgb : COLOR with type t = [ `Rgb ]

  module Rgba : COLOR with type t = [ `Rgba ]

  module Gray : COLOR with type t = [ `Gray ]

  module Xyz : COLOR with type t = [ `Xyz ]

  module Yuv : COLOR with type t = [ `Yuv ]

  type 'a t = (module COLOR with type t = 'a)
  (** Used to specify the color model of an image *)

  val name : 'a t -> string

  val has_alpha : 'a t -> bool
  (** Returns true if the color has an alpha channel *)

  val channels : 'a t -> int
  (** Returns the number of channels for a color *)
end

type gray = Color.Gray.t
(** 1-channels gray color type *)

type rgb = Color.Rgb.t
(** 3-channel RGB color type *)

type xyz = [ `Xyz ]
(** 3-channel XYZ color type *)

type yuv = [ `Yuv ]
(** 3-channel YUV color type *)

type rgba = Color.Rgba.t
(** 4-channel RGBA image *)

(*type any = [ `Any ]*)
(** Any color image *)

val gray : [ `Gray ] Color.t
(** Gray color *)

val rgb : [ `Rgb ] Color.t
(** RGB color *)

(*val rgb_packed : rgb_packed Color.t
(** RGB packed into a signle channel *)

val xyz : xyz Color.t
(** XYZ color *)

val yuv : yuv Color.t
(** YUV color *)*)

val rgba : [ `Rgba ] Color.t
(** RGBA color *)

(*val color : int -> any Color.t*)
(** Generic color *)

module Type : sig
  type ('a, 'b) t = (module TYPE with type t = 'a and type elt = 'b)

  val kind : ('a, 'b) t -> ('a, 'b) Bigarray.kind
  (** Get Bigarray kind *)

  val name : ('a, 'b) t -> string
  (** [name k] returns the name of a given ty *)

  val depth : ('a, 'b) t -> int
  (** returns the number of bits for a given ty *)

  val max : ('a, 'b) Type.t -> 'a
  (** [max k] returns the maximum normalized value for [k] *)

  val min : ('a, 'b) Type.t -> 'a
  (** [min k] returns the minimum normalized value for [k] *)

  val max_f : ('a, 'b) Type.t -> float
  (** [max k] returns the maximum normalized value for [k] as a float *)

  val min_f : ('a, 'b) Type.t -> float
  (** [min k] returns the minimum normalized value for [k] as a float *)

  val to_float : ('a, 'b) Type.t -> 'a -> float
  (** [to_float k x] converts a value of type [k] to float *)

  val of_float : ('a, 'b) Type.t -> float -> 'a
  (** [of_float k x] converts a float to a value of ty [k] *)

  val clamp : ('a, 'b) Type.t -> float -> float
  (** Converts a float value to a value within the proper range for the given type *)

  val normalize : ('a, 'b) Type.t -> float -> float
  (** Scales a value to the range 0.0-1.0 *)

  val denormalize : ('a, 'b) Type.t -> float -> float
  (** Sclaes a value to the range (type_min-type_max) *)

  val convert : from:('a, 'b) Type.t -> ('c, 'd) Type.t -> 'a -> 'c
end

type u8 = int8_unsigned_elt

type u16 = int16_unsigned_elt

type i32 = int32_elt

type i64 = int64_elt

type f32 = float32_elt

type f64 = float64_elt

val u8 : (int, u8) Type.t

val u16 : (int, u16) Type.t

val i32 : (int32, i32) Type.t

val i64 : (int64, i64) Type.t

val f32 : (float, f32) Type.t

val f64 : (float, f64) Type.t

(** The Data module defines several operations on one dimensional image data *)
module Data : sig
  type ('a, 'b) t = ('a, 'b, c_layout) Array1.t
  (** Data type *)

  val ty : ('a, 'b) t -> ('a, 'b) Type.t
  (** Get the [Bigarray.ty] *)

  val of_array : ('a, 'b) Type.t -> 'a array -> ('a, 'b) t
  (** Converts an array to a [Data.t] of the given ty *)

  val to_array : ('a, 'b) t -> 'a array
  (** Converts a [Data.t] to an array *)

  val create : ('a, 'b) Type.t -> int -> ('a, 'b) t
  (** Create a new [Data.t] with the given length. *)

  val length : ('a, 'b) t -> int
  (** Returns the number of elements in a [Data.t] *)

  val fold2 : ('a -> 'd -> 'c -> 'c) -> ('a, 'b) t -> ('d, 'e) t -> 'c -> 'c
  (** Reduce over two [Data.t] *)

  val fold : ('a -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  (** Reduce over a single [Data.t] *)

  val fill : ('a, 'b) t -> 'a -> unit
  (** [fill d x] sets each value of [d] to [x] *)

  val map_inplace : ('a -> 'a) -> ('a, 'b) t -> unit
  (** [map_inplace f data] runs [f] over each value of [data] *)

  val map2_inplace : ('a -> 'c -> 'a) -> ('a, 'b) t -> ('c, 'd) t -> unit
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
end

(** Pixels are float vectors used to store image data *)
module Pixel : sig
  type 'a t

  val empty : 'a Color.t -> 'a t
  (** Create a new pixel with all channels set to 0 *)

  val length : 'a t -> int
  (** Get the number of channels in a pixel *)

  val get : 'a t -> int -> float

  val set : 'a t -> int -> float -> unit

  val compare : 'a t -> 'a t -> int

  val equal : 'a t -> 'a t -> bool

  val from_data : 'c Color.t -> ('a, 'b) Data.t -> 'c t
  (** Create a new pixel from existing image data *)

  val to_data : dest:('a, 'b) Data.t -> 'c t -> unit
  (** Copy pixel data to existing image data *)

  val data : 'a t -> floatarray
  (** Returns the underlying pixel data *)

  val color : 'a t -> 'a Color.t

  val to_rgb : 'a t -> rgb t

  val from_rgb : 'a Color.t -> rgb t -> 'a t

  (*val rgb_to_xyz : t -> t*)
  (** Convert pixel from RGB to XYZ *)

  (*val rgb_to_yuv : t -> t*)
  (** Convert pixel from RGB to YUV *)

  val map : (float -> float) -> 'a t -> 'a t
  (** [map f x] executes [f] for each value in [x], returning a new Pixel.t *)

  val map_inplace : (float -> float) -> 'a t -> unit
  (** [map_inplace f x] executes [f] for each value in [x], assigning the new value to the same
   *  index *)

  val fold : (float -> 'a -> 'a) -> 'b t -> 'a -> 'a
  (** Reduction over a pixel *)

  val pp : Format.formatter -> 'a t -> unit
end

(** The Image module defines a simple interface for manipulating image data *)
module Image : sig
  type ('a, 'b, 'c) t = {
    width : int;
    height : int;
    color : 'c Color.t;
    ty : ('a, 'b) Type.t;
    data : ('a, 'b) Data.t;
  }
  (** Image type *)

  val create : ('a, 'b) Type.t -> 'c Color.t -> int -> int -> ('a, 'b, 'c) t
  (** [create ty color width height] makes a new image with the given [ty], [color] and dimensions *)

  val compare : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> int

  val equal : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> bool

  val data : ('a, 'b, 'c) t -> ('a, 'b) Data.t
  (** Get image data *)

  val of_data : 'c Color.t -> int -> int -> ('a, 'b) Data.t -> ('a, 'b, 'c) t
  (** [of_data color width height data] makes a new image from existing image data with the given [ty], [color], and dimensions *)

  val like : ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** [like img] creates a new image with the same dimensions, color and ty as [img] *)

  val like_with_color : 'd Color.t -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t

  val like_with_ty : ('d, 'e) Type.t -> ('a, 'b, 'c) t -> ('d, 'e, 'c) t

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

  val convert :
    ('d, 'e) Type.t -> 'f Color.t -> ('a, 'b, 'c) t -> ('d, 'e, 'f) t
  (** Convert an image to a new image of another ty *)

  (*val of_any_color :
    ('a, 'b, any) t -> 'c Color.t -> (('a, 'b, 'c) t, Error.t) result*)
  (** Convert from [any] color to the given color *)

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

  val avg :
    ?x:int ->
    ?y:int ->
    ?width:int ->
    ?height:int ->
    ('a, 'b, 'c) t ->
    (float, f64) Data.t
  (** Get the average pixel of an image or region of an image *)

  val crop :
    ('a, 'b, 'c) t ->
    x:int ->
    y:int ->
    width:int ->
    height:int ->
    ('a, 'b, 'c) t
  (** Extract the sub-image specified by the given dimensions *)

  val rotate_90 : ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Rotate an image 90 degrees *)

  val rotate_180 : ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Rotate an image 180 degrees *)

  val rotate_270 : ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Rotate an image 270 degrees *)

  val resize : int -> int -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Scale an image to the given size *)

  val mean_std : ?channel:int -> ('a, 'b, 'c) t -> float * float
  (** Calculate the mean and standard deviation of an image *)

  val fold : ('a -> 'd -> 'd) -> ('a, 'b, 'c) t -> 'd -> 'd

  val fold2 :
    ('a -> 'e -> 'd -> 'd) -> ('a, 'b, 'c) t -> ('e, 'f, 'c) t -> 'd -> 'd

  val fold_data :
    (int -> int -> ('a, 'b) Data.t -> 'd -> 'd) -> ('a, 'b, 'c) t -> 'd -> 'd

  val fold_data2 :
    (int -> int -> ('a, 'b) Data.t -> ('e, 'f) Data.t -> 'd -> 'd) ->
    ('a, 'b, 'c) t ->
    ('e, 'f, 'c) t ->
    'd ->
    'd

  val map_inplace : ('a -> 'a) -> ('a, 'b, 'c) t -> unit

  val map : ('a -> 'a) -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t

  val map2_inplace :
    ('a -> 'd -> 'a) -> ('a, 'b, 'c) t -> ('d, 'e, 'f) t -> unit

  val map2 :
    ('a -> 'd -> 'a) -> ('a, 'b, 'c) t -> ('d, 'e, 'f) t -> ('a, 'b, 'c) t

  module Diff : sig
    type diff

    val apply : diff -> ('a, 'b, 'c) t -> unit
  end

  val diff : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> Diff.diff
end

(** Kernels are used for filtering images using convolution *)
module Kernel : sig
  type t

  val create : int -> int -> t
  (** [create rows cols] makes a new Kernel with the given dimensions *)

  val rows : t -> int
  (** Returns the number of rows in a kernel *)

  val cols : t -> int
  (** Returns the number of columns in a kernel *)

  val join : (float -> float -> float) -> t -> t -> t
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
end

(** Defines the type used as input to operations *)
module Input : sig
  type input = Input : ('a, 'b, 'c) Image.t -> input

  type t = input array

  type index = private int

  val index : int -> index

  val input : ('a, 'b, 'c) Image.t -> input

  val int_of_index : index -> int

  val get : t -> index -> input
  (** Get an image from the input, raising [Error.Exc (`Invalid_input index)]
      if the provided index is out of bounds. *)

  val shape : t -> int * int * int
end

module Transform : sig
  type t

  val v :
    float ->
    float ->
    float ->
    float ->
    float ->
    float ->
    float ->
    float ->
    float ->
    t

  val neg : t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val mmul : t -> t -> t

  val smul : t -> float -> t

  val transpose : t -> t

  val det : t -> float

  val inv : t -> t

  val translate : float -> float -> t

  val rotate : ?center:Point.t -> Angle.t -> t

  val scale : float -> float -> t

  val transform : t -> Point.t -> Point.t
end

(** Expr implements an operation combinator which can be used to build operations from low-level functions *)
module Expr : sig
  type _ t =
    | Kernel : Input.index * Kernel.t -> float t
    | Input : Input.index * int t * int t * int t -> float t
    | X : int t
    | Y : int t
    | C : int t
    | Int : int -> int t
    | Float : float -> float t
    | Bool : bool -> bool t
    | Float_of_int : int t -> float t
    | Int_of_float : float t -> int t
    | Fadd : float t * float t -> float t
    | Fsub : float t * float t -> float t
    | Fmul : float t * float t -> float t
    | Fdiv : float t * float t -> float t
    | Fpow : float t * float t -> float t
    | Fsqrt : float t -> float t
    | Fsin : float t -> float t
    | Fcos : float t -> float t
    | Ftan : float t -> float t
    | Fmod : float t * float t -> float t
    | Iadd : int t * int t -> int t
    | Isub : int t * int t -> int t
    | Imul : int t * int t -> int t
    | Idiv : int t * int t -> int t
    | Imod : int t * int t -> int t
    | Gt : 'a t * 'a t -> bool t
    | Eq : 'a t * 'a t -> bool t
    | Lt : 'a t * 'a t -> bool t
    | And : bool t * bool t -> bool t
    | Or : bool t * bool t -> bool t
    | Not : bool t -> bool t
    | Cond : bool t * 'a t * 'a t -> 'a t
    | Func : 'b t * (int -> int -> int -> 'b -> 'a t) -> 'a t
    | Pixel : Input.index * int t * int t -> rgb Pixel.t t
    | Value : 'a -> 'a t
    | Pair : 'a t * 'b t -> ('a * 'b) t
    | Type_min : Input.index -> float t
    | Type_max : Input.index -> float t
    | Channels : Input.index -> int t
    | Shape : Input.index -> (int * int * int) t

  val op :
    ?x:int ref ->
    ?y:int ref ->
    ?c:int ref ->
    float t ->
    Input.t ->
    int ->
    int ->
    int ->
    float

  val int : int -> int t
  (** Create an int [Expr] *)

  val float : float -> float t
  (** Create a float [Expr] *)

  val int_of_float : float t -> int t

  val float_of_int : int t -> float t

  val x : int t

  val y : int t

  val c : int t

  val kernel : Input.index -> Kernel.t -> float t
  (** Create a kernel expr from an existing kernel *)

  val join_kernel :
    Input.index -> (float -> float -> float) -> Kernel.t -> Kernel.t -> float t
  (** Create a kernel expession using two kernels combined using the designated operation *)

  val transform : Input.index -> Transform.t -> float t
  (** Apply a transformation *)

  val rotate : Input.index -> ?center:float * float -> float -> float t

  val scale : Input.index -> float -> float -> float t

  val threshold : Input.index -> float array -> float t

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** Create a new Pair expr, used for joining existing expressions *)

  val pixel : Input.index -> int t -> int t -> rgb Pixel.t t
  (** Create a Pixel expr, for extracting pixels *)

  val type_min : Input.index -> float t

  val type_max : Input.index -> float t

  val channels : Input.index -> int t

  val value : 'a -> 'a t
  (** Create a Value expr *)

  val shape : Input.index -> (int * int * int) t

  val func : 'b t -> (int -> int -> int -> 'b -> 'a t) -> 'a t
  (** Create a Func expr *)

  val map : ('b -> 'a t) -> 'b t -> 'a t

  val input : int -> int t -> int t -> int t -> float t
  (** Get input data from the specified index *)

  val fadd : float t -> float t -> float t

  val fsub : float t -> float t -> float t

  val fmul : float t -> float t -> float t

  val fdiv : float t -> float t -> float t

  val iadd : int t -> int t -> int t

  val isub : int t -> int t -> int t

  val imul : int t -> int t -> int t

  val idiv : int t -> int t -> int t

  val pow : float t -> float t -> float t

  val sqrt : float t -> float t

  val sin : float t -> float t

  val cos : float t -> float t

  val tan : float t -> float t

  val pi : unit -> float t

  val and_ : bool t -> bool t -> bool t

  val or_ : bool t -> bool t -> bool t

  val not_ : bool t -> bool t

  val cond : bool t -> 'a t -> 'a t -> 'a t

  val blend : Input.index -> Input.index -> float t
  (** An expression to average two images *)

  val min : Input.index -> Input.index -> float t
  (** An expression to take the lowest value from two images *)

  val max : Input.index -> Input.index -> float t
  (** An expression to take the highest value from two images *)

  val brightness : Input.index -> float t -> float t
  (** Multiply each pixel component *)

  val grayscale : Input.index -> float t

  val color : Input.index -> float t

  val kernel_3x3 : Input.index -> Kernel.t -> float t

  module Infix : sig
    val ( && ) : bool t -> bool t -> bool t

    val ( || ) : bool t -> bool t -> bool t

    val ( + ) : int t -> int t -> int t
    (** Integer addition *)

    val ( - ) : int t -> int t -> int t
    (** Integer subtraction *)

    val ( * ) : int t -> int t -> int t
    (** Integer multiplacation *)

    val ( / ) : int t -> int t -> int t
    (** Integer division *)

    val ( +. ) : float t -> float t -> float t
    (** Float addition *)

    val ( -. ) : float t -> float t -> float t
    (** Float subtraction *)

    val ( *. ) : float t -> float t -> float t
    (** Float multiplication *)

    val ( /. ) : float t -> float t -> float t
    (** Float division *)

    val ( ** ) : float t -> float t -> float t
    (** Pow *)
  end
end

(** Op is used to define pixel-level operations. These operations are performed on normalized floating-point values *)
module Op : sig
  type t = Input.t -> int -> int -> int -> float

  type f = float Expr.t -> t

  val blend : ?input:Input.index -> ?input1:Input.index -> t
  (** Blend two images: [a + b / 2] *)

  val min : ?input:Input.index -> ?input1:Input.index -> t
  (** Minimum pixel value of two images *)

  val max : ?input:Input.index -> ?input1:Input.index -> t
  (** Maximum pixel value of two images *)

  val grayscale : ?input:Input.index -> t
  (** Convert a color image to grayscale *)

  val color : ?input:Input.index -> t
  (** Convert a grayscale image to color *)

  val join : (float -> float -> float) -> t -> t -> t
  (** [join f a b] builds a new operation of [f(a, b)] *)

  val apply : f -> t -> t
  (** [apply f a] builds a new operation of [f(a)] *)

  val scalar : float -> t
  (** Builds an operation returning a single value *)

  val scalar_max : ('a, 'b) Type.t -> t
  (** Builds an operation returning the maximum value for a given ty *)

  val scalar_min : ('a, 'b) Type.t -> t
  (** Builds an operation returning the minimum value for a given ty *)

  val invert : ?input:Input.index -> t
  (** Invert the values in an image *)

  val cond : (Input.t -> int -> int -> int -> bool) -> t -> t -> t
  (** Conditional operation *)

  val sobel_x : ?input:Input.index -> t
  (** Sobel in the X direction *)

  val sobel_y : ?input:Input.index -> t
  (** Sobel in the Y direction *)

  val sobel : ?input:Input.index -> t
  (** Sobel kernel *)

  val gaussian_blur : ?input:Input.index -> ?std:float -> int -> t
  (** Blur using gaussian kernel. The size must be an odd number. *)

  val transform : ?input:Input.index -> Transform.t -> t
  (** Apply a transformation *)

  val rotate : ?input:Input.index -> ?center:float * float -> Angle.t -> t
  (** Rotation operation *)

  val scale : ?input:Input.index -> float -> float -> t
  (** Scale an image by the given amount *)

  val brightness : ?input:Input.index -> float Expr.t -> t
  (** Adjust the brightness of an image. 0.0 will remove all brightness and 1.0 will keep the image as-is. *)

  val threshold : ?input:Input.index -> float array -> t
  (** Per-channel threshold -- each entry in the given array is the threshold for the channel with the same index *)

  module Infix : sig
    val ( &+ ) : t -> t -> t
    (** Infix operator for [join] using addition *)

    val ( &- ) : t -> t -> t
    (** Infix operator for [join] using subtraction *)

    val ( &* ) : t -> t -> t
    (** Infix operator for [join] using multiplication *)

    val ( &/ ) : t -> t -> t
    (** Infix operator for [join] using division *)

    val ( $ ) : t -> f -> t
    (** Infix operator for [map] *)
  end
end

module type FILTER = sig
  type 'a io

  type ('a, 'b, 'c) t = output:('a, 'b, 'c) Image.t -> Input.t -> unit io

  val join : ('a, 'b, 'c) t array -> ('a, 'b, 'c) t

  val make : ?x:int ref -> ?y:int ref -> ?c:int ref -> Op.t -> ('a, 'b, 'c) t

  val of_expr : float Expr.t -> ('a, 'b, 'c) t
end

module Filter : sig
  include FILTER with type 'a io = 'a

  module Make (S : sig
    type 'a io

    val bind : 'a io -> ('a -> 'b io) -> 'b io

    val return : 'a -> 'a io

    val detach : ('a -> 'b) -> 'a -> 'b io
  end) : FILTER with type 'a io = 'a S.io
end

type ('a, 'b, 'c) filter = output:('a, 'b, 'c) Image.t -> Input.t -> unit

module Hash : sig
  type t

  module Set : Set.S with type elt = t

  val phash : ('a, 'b, 'c) Image.t -> t

  val equal : t -> t -> bool

  val to_string : t -> string

  val to_int64 : t -> int64
end

val ( ~@ ) : int -> Input.index

(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
