open Image

type t = any array -> int -> int -> int -> float

type f = float Expr.t -> any array -> int -> int -> int -> float

let blend ?(input = 0) ?(input1 = 1) : t = Expr.op (Expr.blend input input1)

let max ?(input = 0) ?(input1 = 1) : t = Expr.op (Expr.max input input1)

let min ?(input = 0) ?(input1 = 1) : t = Expr.op (Expr.min input input1)

let grayscale ?(input = 0) : t = Expr.op (Expr.grayscale input)

let color ?(input = 0) : t = Expr.op (Expr.color input)

let cond : (any array -> int -> int -> int -> bool) -> t -> t -> t =
  fun cond a b inputs x y c ->
  if cond inputs x y c then a inputs x y c else b inputs x y c

let join f a b inputs x y c = f (a inputs x y c) (b inputs x y c)

let apply f a inputs x y c = f (Expr.float @@ a inputs x y c) inputs x y c

let scalar : float -> t = fun f _inputs _x _y _c -> f

let scalar_min : ('a, 'b) Type.t -> t = fun k -> scalar (Type.min_f k)

let scalar_max : ('a, 'b) Type.t -> t = fun k -> scalar (Type.max_f k)

let invert ?(input = 0) : t =
  fun inputs x y c ->
  let (Any a) = inputs.(input) in
  if c = 4 then get_f a x y c else 1.0 -. get_f a x y c

module Infix = struct
  let ( $ ) a f = apply f a

  let ( &+ ) a b = join ( +. ) a b

  let ( &- ) a b = join ( -. ) a b

  let ( &* ) a b = join ( *. ) a b

  let ( &/ ) a b = join ( /. ) a b
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

let brightness ?(input = 0) n inputs x y c =
  Expr.op (Expr.brightness input n) inputs x y c

let threshold ?(input = 0) thresh = Expr.op (Expr.threshold input thresh)
