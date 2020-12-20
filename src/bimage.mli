(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Image processing library

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

open Bigarray
(** {1 Bimage} *)

module type TYPE = Type.TYPE

exception Unsupported
(** Raised when attempting to use Bigarray types other than u8, u16, f32, f64, i32, i64*)

module Error = Error
module Angle = Util.Angle
module Point = Util.Point

module type COLOR = Color.COLOR

module Color = Color
(** Color contains methods for creating and inspecting color types *)

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

val gray : [ `Gray ] Color.t
(** Gray color *)

val rgb : [ `Rgb ] Color.t
(** RGB color *)

val xyz : xyz Color.t
(** XYZ color *)

val yuv : yuv Color.t
(** YUV color *)

val rgba : [ `Rgba ] Color.t
(** RGBA color *)

module Type = Type

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

module Data = Data
(** The Data module defines several operations on one dimensional image data *)

module Pixel = Pixel
(** The pixel module defines operations on individual pixels *)

module Image = Image
(** The Image module defines an interface for manipulating images *)

type 'c image_u8 = (int, u8, 'c) Image.t

type 'c image_u16 = (int, u16, 'c) Image.t

type 'c image_i32 = (int32, i32, 'c) Image.t

type 'c image_i64 = (int64, i64, 'c) Image.t

type 'c image_f32 = (float, f32, 'c) Image.t

type 'c image_f64 = (float, f64, 'c) Image.t

module Kernel = Kernel
(** Kernels are used for filtering images using convolution *)

module Input = Input
(** Defines the type used as input to operations *)

module Transform = Transform

module Expr = Expr
(** Expr implements an operation combinator which can be used to build operations from low-level functions *)

module type FILTER = Filter.FILTER

module Filter = Filter

type ('a, 'b, 'c) filter = output:('a, 'b, 'c) Image.t -> Input.t -> unit

module Hash = Hash

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
