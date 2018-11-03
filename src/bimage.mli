(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Image processing library

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Bimage} *)
open Bigarray

(** Raised when attempting to use Char, Int8_signed, Int16_signed Bigarray types *)
exception Unsupported

type ('a, 'b) kind = ('a, 'b) Bigarray.kind

type u8 = int8_unsigned_elt

type u16 = int16_unsigned_elt

type i32 = int32_elt

type i64 = int64_elt

type f32 = float32_elt

type f64 = float64_elt

type c32 = complex32_elt

type c64 = complex64_elt

val u8 : (int, u8) kind

val u16 : (int, u16) kind

val i32 : (int32, i32) kind

val i64 : (int64, i64) kind

val f32 : (float, f32) kind

val f64 : (float, f64) kind

val c32 : (Complex.t, c32) kind

val c64 : (Complex.t, c64) kind

module Error : sig
  type t =
    [ `Invalid_shape
    | `Invalid_kernel_shape of int * int
    | `Invalid_input of int
    | `Invalid_layout
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
module Point: sig
  type t = float * float

  val x: t -> float
  (** [x pt] extracts the x coordinate *)

  val y: t -> float
  (** [y pt] extracts the y coordinate *)
end

(** Color contains methods for creating and inspecting color types *)
module Color : sig
  (** Used to specify the color model of an image *)
  type 'a t

  val create : has_alpha:bool -> channels:int -> 'a -> 'a t
  (** Create a new color type *)

  val has_alpha : 'a t -> bool
  (** Returns true if the color has an alpha channel *)

  val channels : 'a t -> int
  (** Returns the number of channels for a color *)

  val t : 'a t -> 'a
  (** Returns the underlying type of a color *)
end

(** 1-channels gray color type *)
type gray = [`Gray]

(** 3-channel RGB color type *)
type rgb = [`Rgb]

(** 3-channel XYZ color type *)
type xyz = [`Xyz]

(** 3-channel YUV color type *)
type yuv = [`Yuv]

(** 4-channel RGBA image *)
type rgba = [`Rgba]

(** Any color image *)
type any = [`Any]

(** Gray color *)
val gray : gray Color.t

(** RGB color *)
val rgb : rgb Color.t

(** XYZ color *)
val xyz : xyz Color.t

(** YUV color *)
val yuv : yuv Color.t

(** RGBA color *)
val rgba : rgba Color.t

(** Generic color *)
val color : int -> any Color.t

module Kind : sig
  val max : ('a, 'b) kind -> 'a
  (** [max k] returns the maximum normalized value for [k] *)

  val min : ('a, 'b) kind -> 'a
  (** [min k] returns the minimum normalized value for [k] *)

  val max_f : ('a, 'b) kind -> float
  (** [max k] returns the maximum normalized value for [k] as a float *)

  val min_f : ('a, 'b) kind -> float
  (** [min k] returns the minimum normalized value for [k] as a float *)

  val to_float : ('a, 'b) kind -> 'a -> float
  (** [to_float k x] converts a value of kind [k] to float *)

  val of_float : ('a, 'b) kind -> float -> 'a
  (** [of_float k x] converts a float to a value of kind [k] *)

  val clamp : ('a, 'b) kind -> float -> float
  (** Converts a float value to a value within the proper range for the given kind *)

  val normalize : ('a, 'b) kind -> float -> float
  (** Scales a value to the range 0.0-1.0 *)

  val denormalize : ('a, 'b) kind -> float -> float
  (** Sclaes a value to the range (kind_min-kind_max) *)

  val convert : from:('a, 'b) kind -> ('c, 'd) kind -> 'a -> 'c
end

(** The Data module defines several operations on one dimensional image data *)
module Data : sig
  (** Data type *)
  type ('a, 'b) t = ('a, 'b, c_layout) Array1.t

  val kind : ('a, 'b) t -> ('a, 'b) kind
  (** Get the [Bigarray.kind] *)

  val of_array : ('a, 'b) kind -> 'a array -> ('a, 'b) t
  (** Converts an array to a [Data.t] of the given kind *)

  val to_array : ('a, 'b) t -> 'a array
  (** Converts a [Data.t] to an array *)

  val create : ('a, 'b) kind -> int -> ('a, 'b) t
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

  val convert : ('c, 'd) kind -> ('a -> 'c) -> ('a, 'b) t -> ('c, 'd) t
  (** Convert between [Data.t] types *)

  val convert_to : ('a -> 'c) -> dest:('c, 'd) t -> ('a, 'b) t -> unit
  (** Convert between [Data.t] types with an existing destination image *)

  val of_float :
    ?dest:('a, 'b) t -> ('a, 'b) kind -> (float, f32) t -> ('a, 'b) t
  (** [of_float ~dest k data] converts a [Data.t] from float to [k], storing the results in
      [dest] if provided. *)

  val to_float : ?dest:(float, f32) t -> ('a, 'b) t -> (float, f32) t
  (** [to_float ~dest data] converts a [Data.t] to float values, storing the results in
      [dest] if provided. *)

  val hash : ('a, 'b) t -> int
  (** Default hash function *)

  val compare : ('a, 'b) t -> ('a, 'b) t -> int
  (** Default comparison function *)

  val equal : ('a, 'b) t -> ('a, 'b) t -> bool
  (** Default equality function *)
end

(** Pixels are float vectors used to store normalized image data *)
module Pixel : sig
  type t

  val empty : int -> t
  (** Create a new pixel with all channels set to 0 *)

  val compare : t -> t -> int
  val equal : t -> t -> bool

  val from_data : ('a, 'b) Data.t -> t
  (** Create a new pixel from existing image data *)

  val to_data : dest:('a, 'b) Data.t -> t -> unit
  (** Copy pixel data to existing image data *)

  val data : t -> (float, f32) Data.t
  (** Returns the underlying pixel data *)

  val rgb_to_xyz : t -> t
  (** Convert pixel from RGB to XYZ *)

  val rgb_to_yuv : t -> t
  (** Convert pixel from RGB to YUV *)

  val map : (float -> float) -> t -> t
  (** [map f x] executes [f] for each value in [x], returning a new Pixel.t *)

  val map_inplace : (float -> float) -> t -> unit
  (** [map_inplace f x] executes [f] for each value in [x], assigning the new value to the same
   *  index *)

  val fold : (float -> 'a -> 'a) -> t -> 'a -> 'a
  (** Reduction over a pixel *)

  val fold2 : (float -> float -> 'a -> 'a) -> t -> t -> 'a -> 'a
  (** Reduction over two pixels *)

  val pp : Format.formatter -> t -> unit
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

(** The Image module defines a simple interface for manipulating image data *)
module Image : sig
  type layout =
    | Planar
    | Interleaved
    (** Image pixel layout. Planar is [RRRGGGBBB] and Interleaved is [RGBRGBRGB] *)

  type ('a, 'b, 'c) t =
    { width : int
    ; height : int
    ; color : 'c Color.t
    ; layout : layout
    ; data : ('a, 'b) Data.t }
  (** Image type *)

  val create :
    ?layout:layout
    -> ('a, 'b) kind
    -> 'c Color.t
    -> int
    -> int
    -> ('a, 'b, 'c) t
  (** [create kind color width height] makes a new image with the given [kind], [color] and dimensions *)

  val compare: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> int
  val equal: ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> bool

  val of_data :
    'c Color.t -> int -> int -> layout -> ('a, 'b) Data.t -> ('a, 'b, 'c) t
  (** [of_data color width height layout data] makes a new image from existing image data with the given [kind], [color], [layout], and dimensions *)

  val like : ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** [like img] creates a new image with the same dimensions, color and kind as [img] *)

  val like_with_color : 'd Color.t -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t

  val like_with_kind : ('d, 'e) kind -> ('a, 'b, 'c) t -> ('d, 'e, 'c) t

  val like_with_layout : layout -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t

  val copy : ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Makes a copy of an image and underlying image data *)

  val copy_to : dest:('a, 'b, 'c) t -> ('a, 'b, 'c) t -> unit
  (** Copy pixels from one image to another *)

  val channels : ('a, 'b, 'c) t -> int
  (** Returns the number of channels in an image *)

  val layout : ('a, 'b, 'c) t -> layout
  (** Returns the image layout type *)

  val length : ('a, 'b, 'c) t -> int
  (** Returns the number of values contained in an image *)

  val kind : ('a, 'b, 'c) t -> ('a, 'b) kind
  (** Returns the image kind *)

  val color : ('a, 'b, 'c) t -> 'c Color.t
  (** Returns the image color type *)

  val shape : ('a, 'b, 'c) t -> int * int * int
  (** Returns the width, height and channels *)

  val convert_to : dest:('d, 'e, 'c) t -> ('a, 'b, 'c) t -> unit
  (** Convert an image to an existing image of another kind *)

  val convert : ('d, 'e) kind -> ('a, 'b, 'c) t -> ('d, 'e, 'c) t
  (** Convert an image to a new image of another kind *)

  val of_any_color :
    ('a, 'b, any) t -> 'c Color.t -> (('a, 'b, 'c) t, Error.t) result
  (** Convert from [any] color to the given color *)

  val get : ('a, 'b, 'c) t -> int -> int -> int -> 'a
  (** [get image x y c] returns a the value at (x, y, c) *)

  val set : ('a, 'b, 'c) t -> int -> int -> int -> 'a -> unit
  (** Set a single channel of the given image at (x, y) *)

  val get_f : ('a, 'b, 'c) t -> int -> int -> int -> float
  (** [get_f image x y c] returns the float value at (x, y, c) *)

  val set_f : ('a, 'b, 'c) t -> int -> int -> int -> float -> unit
  (** Set a single channel of the given image at (x, y) using a float value *)

  val get_n : ('a, 'b, 'c) t -> int -> int -> int -> float
  (** [get_f image x y c] returns the normalized float value at (x, y, c) *)

  val set_n : ('a, 'b, 'c) t -> int -> int -> int -> float -> unit
  (** Set a single channel of the given image at (x, y) using a normalized float value *)

  val get_pixel : ('a, 'b, 'c) t -> ?dest:Pixel.t -> int -> int -> Pixel.t
  (** [get_pixel image x y] returns a pixel representation of [image] data at ([x], [y]) *)

  val set_pixel : ('a, 'b, 'c) t -> int -> int -> Pixel.t -> unit
  (** [set_pixel image x y px] sets the value of [image] at ([x], [y]) to [px] *)

  val get_data :
    ('a, 'b, 'c) t -> ?dest:('a, 'b) Data.t -> int -> int -> ('a, 'b) Data.t
  (** [get_data image x y] returns [image] data at ([x], [y]) *)

  val set_data : ('a, 'b, 'c) t -> int -> int -> ('a, 'b) Data.t -> unit
  (** [set_data image x y px] sets the value of [image] at ([x], [y]) to [px] *)

  val for_each :
    (int -> int -> ('a, 'b) Data.t -> unit)
    -> ?x:int
    -> ?y:int
    -> ?width:int
    -> ?height:int
    -> ('a, 'b, 'c) t
    -> unit
  (** Iterate over each pixel in an image, or a rectangle segment of an image specified by [x], [y], [width],
      and [height]. The data segment used in the callback is mutable and will write directly to the underlying
      image data. *)

  val kernel :
    Kernel.t -> ?output:('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Apply a kernel directly to the provided image. Note that this implementation is much slower
      than `Op.kernel`, it is mostly provided for convenience *)

  val avg :
    ?x:int
    -> ?y:int
    -> ?width:int
    -> ?height:int
    -> ('a, 'b, 'c) t
    -> (float, f32) Data.t
  (** Get the average pixel of an image or region of an image *)

  val convert_layout : layout -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Convert an image to the given layout *)

  val crop :
    ('a, 'b, 'c) t
    -> x:int
    -> y:int
    -> width:int
    -> height:int
    -> ('a, 'b, 'c) t
  (** Extract the sub-image specified by the given dimensions *)

  val rotate_90 : ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Rotate an image 90 degrees *)

  val rotate_180 : ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Rotate an image 180 degrees *)

  val rotate_270 : ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Rotate an image 270 degrees *)

  val resize : int -> int -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Scale an image to the given size *)

  val mean_std: ?channel:int -> ('a, 'b, 'c) t -> float * float
  (** Calculate the mean and standard deviation of an image *)
end

type ('a, 'b, 'c, 'd, 'e, 'f) filter =
  output:('d, 'e, 'f) Image.t -> ('a, 'b, 'c) Image.t array -> unit

module Transform : sig
  type t

  val v:
    float -> float -> float ->
    float -> float -> float ->
    float -> float -> float -> t

  val neg: t -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t
  val mmul: t -> t -> t
  val smul: t -> float -> t
  val transpose: t -> t
  val det: t -> float
  val inv: t -> t

  val translate: float -> float -> t
  val rotate : ?center:Point.t -> Angle.t -> t
  val scale : float -> float -> t
  val transform : t -> Point.t -> Point.t
end

(** Defines the type used as input to operations *)
module Input : sig
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Image.t array

  val get : ('a, 'b, 'c) t -> int -> ('a, 'b, 'c) Image.t
  (** Get an image from the input, raising [Error.Exc (`Invalid_input index)]
      if the provided index is out of bounds. *)

  val make_output :
    ?width:int -> ?height:int -> ('a, 'b, 'c) t -> ('a, 'b, 'c) Image.t
    (** Create an output image width the given width and height if provided, otherwise the generated
        image will match the first input image in size, kind and color *)
end

(** Op is used to define pixel-level operations. These operations are performed on normalized floating-point values *)
module Op : sig
  type ('a, 'b, 'c) t =
    ('a, 'b, 'c) Image.t array -> int -> int -> int -> float

  type ('a, 'b, 'c) f =
    float -> ('a, 'b, 'c) Image.t array -> int -> int -> int -> float

  val blend : ('a, 'b, 'c) t
  (** Blend two images: [a + b / 2] *)

  val min : ('a, 'b, 'c) t
  (** Minimum pixel value of two images *)

  val max : ('a, 'b, 'c) t
  (** Maximum pixel value of two images *)

  val grayscale : ('a, 'b, [< `Rgb | `Rgba]) t
  (** Convert a color image to grayscale *)

  val color : ('a, 'b, [`Gray]) t
  (** Convert a grayscale image to color *)

  val eval :
    ?x:int ref
    -> ?y:int ref
    -> ?c:int ref
    -> ('a, 'b, 'c) t
    -> ('a, 'b, 'c, 'd, 'e, 'f) filter
  (** Evaluate an operation *)

  val join :
    (float -> float -> float)
    -> ('a, 'b, 'c) t
    -> ('a, 'b, 'c) t
    -> ('a, 'b, 'c) t
  (** [join f a b] builds a new operation of [f(a, b)] *)

  val apply : ('a, 'b, 'c) f -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** [map f a] builds a new operation of [f(a)] *)

  val scalar : ('a, 'b, 'c) f
  (** Builds an operation returning a single value *)

  val scalar_max : ('a, 'b) kind -> ('a, 'b, 'c) t
  (** Builds an operation returning the maximum value for a given kind *)

  val scalar_min : ('a, 'b) kind -> ('a, 'b, 'c) t
  (** Builds an operation returning the minimum value for a given kind *)

  val invert_f : ('a, 'b, 'c) f
  (** Invert a single value *)

  val invert : ('a, 'b, 'c) t
  (** Invert the values in an image *)

  val cond :
    (('a, 'b, 'c) Image.t array -> int -> int -> int -> bool)
    -> ('a, 'b, 'c) t
    -> ('a, 'b, 'c) t
    -> ('a, 'b, 'c) t
  (** Conditional operation *)

  val kernel : Kernel.t -> ('a, 'b, 'c) t
  (** Create a kernel operation *)

  val join_kernel :
    (float -> float -> float) -> Kernel.t -> Kernel.t -> ('a, 'b, 'c) t
  (** Create a kernel operation using two kernels combined using the designated operation *)

  val ( &+ ) : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Infix operator for [join] using addition *)

  val ( &- ) : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Infix operator for [join] using subtraction *)

  val ( &* ) : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Infix operator for [join] using multiplication *)

  val ( &/ ) : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Infix operator for [join] using division *)

  val ( %+ ) : Kernel.t -> Kernel.t -> ('a, 'b, 'c) t
  (** Infix operator for [join_kernel] using addition *)

  val ( %- ) : Kernel.t -> Kernel.t -> ('a, 'b, 'c) t
  (** Infix operator for [join_kernel] using subtraction *)

  val ( %* ) : Kernel.t -> Kernel.t -> ('a, 'b, 'c) t
  (** Infix operator for [join_kernel] using multiplication *)

  val ( %/ ) : Kernel.t -> Kernel.t -> ('a, 'b, 'c) t
  (** Infix operator for [join_kernel] using division *)

  val ( $ ) : ('a, 'b, 'c) t -> ('a, 'b, 'c) f -> ('a, 'b, 'c) t
  (** Infix operator for [map] *)

  val sobel_x : ('a, 'b, 'c) t
  (** Sobel in the X direction *)

  val sobel_y : ('a, 'b, 'c) t
  (** Sobel in the Y direction *)

  val sobel : ('a, 'b, 'c) t
  (** Sobel kernel *)

  val gaussian_blur : ?std:float -> int -> ('a, 'b, 'c) t
  (** Blur using gaussian kernel. The size must be an odd number. *)

  val transform : Transform.t -> ('a, 'b, 'c) t
  (** Apply a transformation *)

  val rotate : ?center:float * float -> Angle.t -> ('a, 'b, 'c) t
  (** Rotation operation *)

  val scale : float -> float -> ('a, 'b, 'c) t
  (** Scale an image by the given amount *)

  val brightness : float -> ('a, 'b, 'c) t
  (** Adjust the brightness of an image. 0.0 will remove all brightness and 1.0 will keep the image as-is. *)

  val threshold : float array -> ('a, 'b, 'c) t
  (** Per-channel threshold -- each entry in the given array is the threshold for the channel with the same index *)
end

(** Expr implements an operation combinator which can be used to build operations from low-level functions *)
module Expr : sig
  type _ t =
    | Kernel : Kernel.t -> float t
    | Input : int * int t * int t * int t -> float t
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
    | If : bool t * 'a t * 'a t -> 'a t

  val f :
    ?x:int ref -> ?y:int ref -> ?c:int ref -> float t -> ('a, 'b, 'c) Op.t

  val eval :
    ?x:int ref
    -> ?y:int ref
    -> ?c:int ref
    -> float t
    -> ('a, 'b, 'c, 'd, 'e, 'f) filter
  (** Convert an [Expr] to a filter *)

  val int : int -> int t
  (** Create an int [Expr] *)

  val float : float -> float t
  (** Create a float [Expr] *)

  val int_of_float : float t -> int t

  val float_of_int : int t -> float t

  val x : int t

  val y : int t

  val c : int t

  val kernel : Kernel.t -> float t

  val input : int -> int t -> int t -> int t -> float t

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

  val if_ : bool t -> 'a t -> 'a t -> 'a t

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

module Hash: sig
  type t
  module Set: Set.S with type elt = t
  val phash: ('a, 'b, 'c) Image.t -> t
  val equal: t -> t -> bool
  val to_string: t -> string
  val to_int64: t -> int64
end

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
