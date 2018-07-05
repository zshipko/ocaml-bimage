(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Image processing library

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Bimage} *)
open Bigarray

exception Unsupported
(** Raised when attempting to use Char, Int8_signed, Int16_signed Bigarray types *)

type u8 = int8_unsigned_elt
type u16 = int16_unsigned_elt
type i32 = int32_elt
type i64 = int64_elt
type f32 = float32_elt
type f64 = float64_elt
type c32 = complex32_elt
type c64 = complex64_elt

val u8: (int, u8) kind
val u16: (int, u16) kind
val i32: (int32, i32) kind
val i64: (int64, i64) kind
val f32: (float, f32) kind
val f64: (float, f64) kind
val c32: (Complex.t, c32) kind
val c64: (Complex.t, c64) kind

(** Color contains methods for creating and inspecting color types *)
module Color: sig
  type 'a t
  (** Used to specify the color model of an image *)

  val create: has_alpha:bool -> channels:int -> 'a -> 'a t
  (** Create a new color type *)

  val has_alpha: 'a t -> bool
  (** Returns true if the color has an alpha channel *)

  val channels: 'a t -> int
  (** Returns the number of channels for a color *)

  val t: 'a t -> 'a
  (** Returns the underlying type of a color *)
end

type gray = [`Gray]
(** 1-channels gray color type *)

type rgb = [`Rgb]
(** 3-channel RGB color type *)

type xyz = [`Xyz]
(** 3-channel XYZ color type *)

type yuv = [`Yuv]
(** 3-channel YUV color type *)

type rgba = [`Rgba]
(** 4-channel RGBA image *)

val gray: gray Color.t
val rgb: rgb Color.t
val xyz: xyz Color.t
val yuv: yuv Color.t
val rgba: rgba Color.t

module Kind: sig
  val max: ('a, 'b) kind -> 'a
  (** [max k] returns the maximum normalized value for [k] *)

  val min: ('a, 'b) kind -> 'a
  (** [min k] returns the minimum normalized value for [k] *)

  val max_f: ('a, 'b) kind -> float
  (** [max k] returns the maximum normalized value for [k] as a float *)

  val min_f: ('a, 'b) kind -> float
  (** [min k] returns the minimum normalized value for [k] as a float *)

  val to_float: ('a, 'b) kind -> 'a -> float
  (** [to_float k x] converts a value of kind [k] to float *)

  val of_float: ('a, 'b) kind -> float -> 'a
  (** [of_float k x] converts a float to a value of kind [k] *)

  val clamp: ('a, 'b) kind -> float -> float
  (** Converts a float value to a value within the proper range for the given kind *)
end

(** The Data module defines several operations on one dimensional image data *)
module Data: sig
  type ('a, 'b) t = ('a, 'b, c_layout) Array1.t
  (** Data type *)

  val kind: ('a, 'b) t -> ('a, 'b) kind
  (** Get the [Bigarray.kind] *)

  val of_array: ('a, 'b) kind -> 'a array -> ('a, 'b) t
  (** Converts an array to a [Data.t] of the given kind *)

  val to_array: ('a, 'b) t -> 'a array
  (** Converts a [Data.t] to an array *)

  val create: ('a, 'b) kind -> ?mmap:string -> int -> ('a, 'b) t
  (** Create a new [Data.t] with the given length. If [mmap] is provided then the file
      will be memory mapped to the path specified *)

  val length: ('a, 'b) t -> int
  (** Returns the number of elements in a [Data.t] *)

  val fold2: ('a -> 'd -> 'c -> 'c) -> ('a, 'b) t -> ('d, 'e) t -> 'c -> 'c
  (** Reduce over two [Data.t] *)

  val fold: ('a -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  (** Reduce over a single [Data.t] *)

  val fill: ('a, 'b) t -> 'a -> unit
  (** [fill d x] sets each value of [d] to [x] *)

  val map_inplace: ('a -> 'a) -> ('a, 'b) t -> unit
  (** [map_inplace f data] runs [f] over each value of [data] *)

  val map2_inplace: ('a -> 'c -> 'a) -> ('a, 'b) t -> ('c, 'd) t -> unit
  (** [map2_inplace f data1 data2] runs [f] over each value of [data1] and [data2] *)

  val slice: offs:int -> length:int -> ('a, 'b) t -> ('a, 'b) t
  (** [slice ~offs ~length data] extracts a section of [data] of [length]
      values starting at index [offs] *)

  val copy_to:  dest:('a, 'b) t -> ('a, 'b) t -> unit
  (** [copy_to ~dest src] copies each value from [src] to [dest] *)

  val copy: ('a, 'b) t -> ('a, 'b) t
  (** Create a new copy of [Data.t] *)

  val convert: ('c, 'd) kind -> ('a -> 'c) -> ('a, 'b) t -> ('c, 'd) t
  (** Convert between [Data.t] types *)

  val convert_to: ('c, 'd) kind -> ('a -> 'c) -> dest:('c, 'd) t -> ('a, 'b) t -> unit
  (** Convert between [Data.t] types with an existing destination image *)

  val of_float: ?dest:('a, 'b) t -> ('a, 'b) kind -> (float, f32) t -> ('a, 'b) t
  (** [of_float ~dest k data] converts a [Data.t] from float to [k], storing the results in
      [dest] if provided. *)

  val to_float: ?dest:(float, f32) t -> ('a, 'b) t -> (float, f32) t
  (** [to_float ~dest data] converts a [Data.t] to float values, storing the results in
      [dest] if provided. *)

  val hash: ('a, 'b) t -> int
  (** Default hash function *)

  val compare: ('a, 'b) t -> ('a, 'b) t -> int
  (** Default comparison function *)

  val equal: ('a, 'b) t -> ('a, 'b) t -> bool
  (** Default equality function *)
end

(** Pixels are 3-channel float vectors used to store image data *)
module Pixel: sig
  type t

  val empty: unit -> t
  (** Create a new pixel with all channels set to 0 *)

  val from_data: ('a, 'b) Data.t -> t
  (** Create a new pixel from existing image data *)

  val to_data: dest:('a, 'b) Data.t -> t -> unit
  (** Copy pixel data to existing image data *)

  val to_xyz: t -> t
  (** Convert pixel from RGB to XYZ *)

  val to_yuv: t -> t
  (** Convert pixel from RGB to YUV *)
end

(** Kernels are used for filtering images using convolution *)
module Kernel: sig
  type t

  val create: int -> int -> t
  (** [create rows cols] makes a new Kernel with the given dimensions *)

  val rows: t -> int
  (** Returns the number of rows in a kernel *)

  val cols: t -> int
  (** Returns the number of columns in a kernel *)

  val of_array: ?norm:bool -> float array array -> t
  (** Create a kernel from an existing 2-dimensional float array. When [norm] is true,
      the kernel will be normalized *)

  val to_array: t -> float array array
  (** Convert a kernel to a 2-dimensional float array *)

  val get: t -> int -> int -> float
  (** [get kernel y x] gets the value at (x, y) *)

  val set: t -> int -> int -> float -> unit
  (** [set kernel y x v] sets the value at (x, y) *)

  val sum: t -> float
  (** Get the sum of each value of a kernel *)

  val normalize: t -> t
  (** [normalize kernel] returns a kernel where each element has been divided by the sum of all elements *)
end

(** The Image module defines a simple interface for manipulating image data *)
module Image: sig
  type ('a, 'b, 'c) t = {
    width: int;
    height: int;
    color: 'c Color.t;
    step: int;
    data: ('a, 'b) Data.t;
  }

  val create: ?mmap:string -> ('a, 'b) kind -> 'c Color.t -> int -> int -> ('a, 'b, 'c) t
  (** [create kind color width height] makes a new image with the given [kind], [color] and dimensions *)

  val like: ('a, 'b) kind -> 'c Color.t -> ('d, 'e, 'f) t -> ('a, 'b, 'c) t
  (** [like kind color img] creates a new image with the same dimensions as [img] with the given [kind] and [color] *)

  val channels: ('a, 'b, 'c) t -> int
  (** Returns the number of channels in an image *)

  val kind: ('a, 'b, 'c) t -> ('a, 'b) kind
  (** Returns the image kind *)

  val shape: ('a, 'b, 'c) t -> int * int * int
  (** Returns the width, height and channels *)

  val convert_to: ?scale:float -> ('d, 'e) kind -> dest:('d, 'e, 'c) t -> ('a, 'b, 'c) t -> unit
  (** Convert an image to an existing image of another kind, optionally using a scale factor *)

  val convert: ?scale:float -> ('d, 'e) kind -> ('a, 'b, 'c) t -> ('d, 'e, 'c) t
  (** Convert an image to a new image of another kind, optionally using a scale factor *)

  val at: ('a, 'b, 'c) t -> int -> int -> ('a, 'b) Data.t
  (** [at image x y] returns a [Data.t] with data from (x, y). The data contains one value
      for each channel of the image *)

  val get: ('a, 'b, 'c) t -> int -> int -> int -> float
  (** [get image x y c] returns a float representation of the value at (x, y, c). This is
      equivalent to  [to_float (kind image) (at image x y).{c}] *)

  val set: ('a, 'b, 'c) t -> int -> int -> int -> float -> unit
  (** Set a single channel of the given image at (x, y) *)

  val each_pixel: (int -> int -> ('a, 'b) Data.t -> unit) -> ?x:int -> ?y:int -> ?width:int -> ?height:int -> ('a, 'b, 'c) t -> unit
  (** Iterate over each pixel in an image, or a rectangle segment of an image specified by [x], [y], [width],
      and [height]. The data segment used in the callback is mutable and will write directly to the underlying
      image data. *)

  val filter: Kernel.t -> ?dest:('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Apply a kernel directly to the provided image. Note that this implementation is much slower
      than `Op.filter`, it is mostly provided for convenience *)

  val avg_in_rect: ('a, 'b, 'c) t -> int -> int -> int -> int -> (float, f32) Data.t
  (** Get the average pixel value in a rectangular region of an image *)
end

(** Op is used to define pixel-level operations *)
module Op: sig
  type ('a, 'b, 'c) t = int -> int -> int -> ('a, 'b, 'c) Image.t array -> float

  val blend: ('a, 'b, 'c) t
  (** Blend two images: [a + b / 2] *)

  val min: ('a, 'b, 'c) t
  (** Minimum pixel value of two images *)

  val max: ('a, 'b, 'c) t
  (** Maximum pixel value of two images *)

  val grayscale: ('a, 'b, [< `Rgb | `Rgba]) t
  (** Convert a color image to grayscale *)

  val color: ('a, 'b, [`Gray]) t
  (** Convert a grayscale image to color *)

  val eval: ('a, 'b, 'c) t -> ('d, 'e, 'f) Image.t -> ('a, 'b, 'c) Image.t array -> unit
  (** Evaluate an operation *)

  val join: (float -> float -> float) -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** [join f a b] builds a new operation from [f(a, b)] *)

  val map: ('a, 'b, 'c) t -> (float -> float) -> ('a, 'b, 'c) t
  (** [map a f] builds a new operation from [f(a)] *)

  val scalar: float -> ('a, 'b, 'c) t
  (** Builds an operation returning a single value *)

  val invert_f: ('a, 'b) kind -> float -> float
  (** Invert a single value *)

  val invert: ('a, 'b, 'c) t
  (** Invert the values in an image *)

  val filter_3x3: Kernel.t -> ('a, 'b, 'c) t
  (** Create a filter operation using a 3x3 kernel *)

  val filter: Kernel.t -> ('a, 'b, 'c) t
  (** Create a filter operation *)

  val ( &+ ): ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Infix operator for [join] using addition *)

  val ( &- ): ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Infix operator for [join] using subtraction *)

  val ( &* ): ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Infix operator for [join] using multiplication *)

  val ( &/ ): ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (** Infix operator for [join] using division *)

  val ( $ ): ('a, 'b, 'c) t -> (float -> float) -> ('a, 'b, 'c) t
  (** Infix operator for [map] *)

  val sobel_x: ('a, 'b, 'c) t
  val sobel_y: ('a, 'b, 'c) t
  val sobel: ('a, 'b, 'c) t
  (** Sobel operation *)
end

(** Magick defines image I/O operations using ImageMagick/GraphicsMagick on the
    command-line *)
module Magick: sig
  val read: string -> ('a, 'b) kind -> ([< gray | rgb | rgba] as 'c) Color.t -> ('a, 'b, 'c) Image.t option
  (** [read filename kind color] loads an image from [filename] on disk using the given [kind] and [color] *)

  val write: string -> ('a, 'b, [< gray | rgb | rgba]) Image.t -> unit
  (** [write filename image] saves an image to [filename] *)

  val command: string ref
  (** [command] contains the command used to call out to ImageMagick/GraphicsMagick. For example,
      if you'd like to use GraphicsMagick then set this to "gm convert" *)
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
