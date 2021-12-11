module type COLOR = sig
  type t

  val t : t
  (** Color type value *)

  val name : t -> string
  (** Color name *)

  val channels : t -> int
  (** Returns the number of channels *)

  val has_alpha : t -> bool
  (** Returns true when a color has an alpha channel, which will always be the last channel *)

  val to_rgb : t -> floatarray -> floatarray
  (** Convert a color to RGB *)

  val of_rgb : t -> floatarray -> floatarray
  (** Convert a color from RGB *)
end

module Rgb : COLOR with type t = [ `Rgb ]
(** Color contains methods for creating and inspecting color types *)

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

val alpha_channel : 'a t -> int option
(** Returns the index of the alpha channel if available *)

type gray = [ `Gray ]
(** 1-channels gray color type *)

type rgb = [ `Rgb ]
(** 3-channel RGB color type *)

type xyz = [ `Xyz ]
(** 3-channel XYZ color type *)

type yuv = [ `Yuv ]
(** 3-channel YUV color type *)

type rgba = [ `Rgba ]
(** 4-channel RGBA image *)

type hsv = [ `Hsv ]
(** 3-channel HSV color type *)

val gray : [ `Gray ] t
(** Gray color *)

val rgb : [ `Rgb ] t
(** RGB color *)

val xyz : [ `Xyz ] t
(** XYZ color *)

val yuv : [ `Yuv ] t
(** YUV color *)

val rgba : [ `Rgba ] t
(** RGBA color *)

val hsv : [ `Hsv ] t
(** HSV color *)
