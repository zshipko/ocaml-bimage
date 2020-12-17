module type COLOR = sig
  type t

  val t : t

  val name : t -> string

  val channels : t -> int

  val has_alpha : t -> bool

  val to_rgb : t -> floatarray -> floatarray

  val of_rgb : t -> floatarray -> floatarray
end

open Float.Array

module Rgb : COLOR with type t = [ `Rgb ] = struct
  type t = [ `Rgb ]

  let t = `Rgb

  let name _ = "rgb"

  let channels _ = 3

  let has_alpha _ = false

  let to_rgb _ x = x

  let of_rgb _ x = x
end

module Rgba : COLOR with type t = [ `Rgba ] = struct
  type t = [ `Rgba ]

  let t = `Rgba

  let name _ = "rgba"

  let channels _ = 4

  let has_alpha _ = true

  let to_rgb _ x =
    let alpha = get x 3 in
    set x 0 (get x 0 *. alpha);
    set x 1 (get x 1 *. alpha);
    set x 2 (get x 2 *. alpha);
    set x 3 1.0;
    x

  let of_rgb _ x =
    let dest = Float.Array.create 4 in
    set dest 0 (get x 0);
    set dest 1 (get x 1);
    set dest 2 (get x 2);
    set dest 3 1.0;
    dest
end

module Gray : COLOR with type t = [ `Gray ] = struct
  type t = [ `Gray ]

  let t = `Gray

  let name _ = "gray"

  let channels _ = 1

  let has_alpha _ = false

  let to_rgb _ (px : floatarray) = make 3 (get px 0)

  let of_rgb _ (px : floatarray) =
    make 1 ((get px 0 *. 0.21) +. (get px 1 *. 0.72) +. (get px 2 *. 0.07))
end

module Xyz : COLOR with type t = [ `Xyz ] = struct
  type t = [ `Xyz ]

  let t = `Xyz

  let name _ = "xyz"

  let channels _ = 3

  let has_alpha _ = false

  let to_rgb _ (px : floatarray) =
    let rgb = make 3 0.0 in
    let x = get px 0 /. 100. in
    let y = get px 1 /. 100. in
    let z = get px 2 /. 100. in
    let var_r = (x *. 3.2406) +. (y *. -1.5372) +. (z *. -0.4986) in
    let var_g = (x *. -0.9689) +. (y *. 1.8758) +. (z *. 0.0415) in
    let var_b = (x *. 0.0557) +. (y *. -0.2040) +. (z *. 1.0570) in
    set rgb 0
      ( if var_r > 0.0031308 then
        (1.055 *. Float.pow var_r (1.0 /. 2.4)) -. 0.055
      else 12.92 *. var_r );
    set rgb 1
      ( if var_g > 0.0031308 then
        (1.055 *. Float.pow var_g (1.0 /. 2.4)) -. 0.055
      else 12.92 *. var_g );
    set rgb 2
      ( if var_b > 0.0031308 then
        (1.055 *. Float.pow var_b (1.0 /. 2.4)) -. 0.055
      else 12.92 *. var_b );
    rgb

  let of_rgb _ px =
    let xyz = make 3 0.0 in
    let r = get px 0 in
    let g = get px 1 in
    let b = get px 2 in
    let r =
      if r > 0.04045 then Float.pow ((r +. 0.055) /. 1.055) 2.4 else r /. 12.92
    in
    let g =
      if g > 0.04045 then Float.pow ((g +. 0.055) /. 1.055) 2.4 else g /. 12.92
    in

    let b =
      if b > 0.04045 then Float.pow ((b +. 0.055) /. 1.055) 2.4 else b /. 12.92
    in

    let r = r *. 100. in
    let g = g *. 100. in
    let b = b *. 100. in

    set xyz 0 ((r *. 0.4124) +. (g *. 0.3576) +. (b *. 0.1805));
    set xyz 1 ((r *. 0.2126) +. (g *. 0.7152) +. (b *. 0.0722));
    set xyz 2 ((r *. 0.0193) +. (g *. 0.1192) +. (b *. 0.9505));
    xyz
end

module Yuv : COLOR with type t = [ `Yuv ] = struct
  type t = [ `Yuv ]

  let t = `Yuv

  let name _ = "yuv"

  let channels _ = 3

  let has_alpha _ = false

  let to_rgb _ (px : floatarray) =
    let rgb = make 3 0.0 in
    let y = get px 0 in
    let u = get px 1 in
    let v = get px 2 in
    set rgb 0 (y +. (1.14 *. v));
    set rgb 1 (y -. (0.395 *. u) -. (0.581 *. v));
    set rgb 2 (y +. (2.032 *. u));
    rgb

  let of_rgb _ (px : floatarray) =
    let yuv = make 3 0.0 in
    let r = get px 0 in
    let g = get px 1 in
    let b = get px 2 in
    set yuv 0 ((0.299 *. r) +. (0.587 *. g) +. (0.114 *. b));
    set yuv 1 ((-0.147 *. r) +. 0.289 +. g +. (0.436 *. b));
    set yuv 2 ((0.615 *. r) +. (0.515 *. g) +. (0.1 *. b));
    yuv
end

type 'a t = (module COLOR with type t = 'a)

type rgb = Rgb.t

type rgba = Rgba.t

type gray = Gray.t

type xyz = Xyz.t

type yuv = Yuv.t

let rgb : rgb t = (module Rgb)

let rgba : rgba t = (module Rgba)

let gray : gray t = (module Gray)

let xyz : xyz t = (module Xyz)

let yuv : yuv t = (module Yuv)

let channels (type a) (module C : COLOR with type t = a) = C.channels C.t

let name (type a) (module C : COLOR with type t = a) = C.name C.t

let has_alpha (type a) (module C : COLOR with type t = a) = C.has_alpha C.t

let alpha_channel (type a) (module C : COLOR with type t = a) =
  if C.has_alpha C.t then Some (C.channels C.t - 1) else None
