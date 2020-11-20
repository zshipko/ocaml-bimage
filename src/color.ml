module type COLOR = sig
  type t

  val t : t

  val name : t -> string

  val channels : t -> int

  val has_alpha : t -> bool

  val to_rgb : t -> floatarray -> floatarray

  val from_rgb : t -> floatarray -> floatarray
end

open Float.Array

module Rgb : COLOR with type t = [ `Rgb ] = struct
  type t = [ `Rgb ]

  let t = `Rgb

  let name _ = "rgb"

  let channels _ = 3

  let has_alpha _ = false

  let to_rgb _ x = x

  let from_rgb _ x = x
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

  let from_rgb _ x =
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

  let from_rgb _ (px : floatarray) =
    make 1 ((get px 0 *. 0.21) +. (get px 1 *. 0.72) +. (get px 2 *. 0.07))
end

type 'a t = (module COLOR with type t = 'a)

type rgb = Rgb.t

type rgba = Rgba.t

type gray = Gray.t

let rgb : rgb t = (module Rgb)

let rgba : rgba t = (module Rgba)

let gray : gray t = (module Gray)

let channels (type a) (module C : COLOR with type t = a) = C.channels C.t

let name (type a) (module C : COLOR with type t = a) = C.name C.t
