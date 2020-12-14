open Image

type t = Image.any array -> int -> int -> Expr.pixel

type f = Expr.pixel Expr.t -> t

let blend ?(input = 0) ?(input1 = 1) : t = Expr.op (Expr.blend input input1)

let max ?(input = 0) ?(input1 = 1) : t = Expr.op (Expr.max input input1)

let min ?(input = 0) ?(input1 = 1) : t = Expr.op (Expr.min input input1)

let grayscale ?(input = 0) : t = Expr.op (Expr.grayscale input)

let color ?(input = 0) : t = Expr.op (Expr.color input)

let pixel p = Expr.op (Expr.pixel p)

let cond : (any array -> int -> int -> bool) -> t -> t -> t =
 fun cond a b inputs x y ->
   if cond inputs x y then a inputs x y else b inputs x y

let join f a b inputs x y = f (a inputs x y) (b inputs x y)

let apply (f : f) (a : t) : t =
 fun inputs x y -> f (Expr.pixel @@ a inputs x y) inputs x y

let invert ?(input = 0) : t =
 fun inputs x y ->
   let (Any a) = inputs.(input) in
   Pixel.map (fun x -> 1.0 -. x) (get_pixel a x y |> Pixel.to_rgb)

module Infix = struct
  let ( $ ) a f = apply f a

  let ( &+ ) a b = join Pixel.Infix.( + ) a b

  let ( &- ) a b = join Pixel.Infix.( - ) a b

  let ( &* ) a b = join Pixel.Infix.( * ) a b

  let ( &/ ) a b = join Pixel.Infix.( / ) a b
end

let sobel_x ?(input = 0) : t = Expr.op (Expr.kernel_3x3 input Kernel.sobel_x)

let sobel_y ?(input = 0) : t = Expr.op (Expr.kernel_3x3 input Kernel.sobel_y)

let[@inline] sobel ?(input = 0) =
  Expr.op (Expr.kernel input (Kernel.join ( +. ) Kernel.sobel_x Kernel.sobel_y))

let gaussian_blur ?(input = 0) ?std n =
  Expr.op (Expr.kernel input (Kernel.gaussian ?std n))

let transform ?(input = 0) t = Expr.op (Expr.transform input t)

let rotate ?input ?center angle =
  let r = Transform.rotate ?center angle in
  transform ?input r

let scale ?input x y =
  let s = Transform.scale x y in
  transform ?input s

let brightness ?(input = 0) n inputs x y =
  Expr.op (Expr.brightness input n) inputs x y
