module type COLOR = sig
  type t
  val t: t

  val name: t -> string
  val channels: t -> int
  val has_alpha: t -> bool
  val to_rgb: t -> (float, Type.F64.elt) Data.t -> (float, Type.F64.elt) Data.t
  val from_rgb: t -> (float, Type.F64.elt) Data.t -> (float, Type.F64.elt) Data.t
end

module Rgb: COLOR with type t = [`Rgb] = struct
  type t = [`Rgb]
  let t = `Rgb

  let name _ = "rgb"
  let channels _ = 3
  let has_alpha _ = false
  let to_rgb _ x = x
  let from_rgb _ x = x
end

module Rgba: COLOR with type t = [`Rgba] = struct
  type t = [`Rgba]
  let t = `Rgba

  let name _ = "rgba"
  let channels _ = 4
  let has_alpha _ = true
  let to_rgb _ x =
    let alpha = x.{3} in
    x.{0} <- x.{0} *. alpha;
    x.{1} <- x.{1} *. alpha;
    x.{2} <- x.{2} *. alpha;
    x.{3} <- 1.0;
     x

  let from_rgb _ x =
    let dest = Data.create Type.f64 4 in
    dest.{0} <- x.{0};
    dest.{1} <- x.{1};
    dest.{2} <- x.{2};
    dest.{3} <- 1.0;
    dest
end

module Gray: COLOR with type t = [`Gray] = struct
  type t = [`Gray]
  let t = `Gray

  let name _ = "gray"

  let channels _ = 1
  let has_alpha _ = false

  let to_rgb _ ( px) =
    let  dest = Data.create Type.f64 3 in
    dest.{0} <- px.{0};
    dest.{1} <- px.{1};
    dest.{2} <- px.{2};
    dest

  let from_rgb _ ( px) =
    let dest = Data.create Type.f64 1 in
    dest.{0} <- (px.{0} *. 0.21) +. (px.{1} *. 0.72) +. (px.{2} *. 0.07);
    dest
end

type 'a t = (module COLOR with type t = 'a)

type rgb = Rgb.t
type rgba = Rgba.t
type gray = Gray.t

let rgb: rgb t = (module Rgb)
let rgba: rgba t = (module Rgba)
let gray: gray t = (module Gray)

(*type 'a t = { t : 'a; channels : int; has_alpha : bool }

let create ~has_alpha ~channels t = { t; channels; has_alpha }

let has_alpha { has_alpha; _ } = has_alpha

let channels { channels; _ } = channels

let t { t; _ } = t

type gray = [ `Gray ]

type rgb = [ `Rgb ]

type rgb_packed = [ `Rgb_packed ]

type yuv = [ `Yuv ]

type xyz = [ `Xyz ]

type rgba = [ `Rgba ]

type any = [ `Any ]

let gray = create ~has_alpha:false ~channels:1 `Gray

let rgb = create ~has_alpha:false ~channels:3 `Rgb

let yuv = create ~has_alpha:false ~channels:3 `Yuv

let xyz = create ~has_alpha:false ~channels:3 `Xyz

let rgba = create ~has_alpha:true ~channels:4 `Rgba

let rgb_packed = create ~has_alpha:false ~channels:1 `Rgb_packed

let color n = create ~has_alpha:(n = 4) ~channels:n `Any

let[@inline] channels_of_color : type a. a t -> int =
 fun { channels; _ } -> channels*)
