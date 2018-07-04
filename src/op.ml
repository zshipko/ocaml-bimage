open Type
open Image

type ('a, 'b, 'c) t = int -> int -> int -> ('a, 'b, 'c) Image.t array -> float

let blend: ('a, 'b, 'c) t = fun x y c inputs ->
  let a = inputs.(0) in
  let b = inputs.(1) in
  get a x y c +. get b x y c

let max: ('a, 'b, 'c) t = fun x y c inputs ->
  let a = inputs.(0) in
  let b = inputs.(1) in
  max (get a x y c) (get b x y c)

let min: ('a, 'b, 'c) t = fun x y c inputs ->
  let a = inputs.(0) in
  let b = inputs.(1) in
  min (get a x y c) (get b x y c)

let grayscale: ('a, 'b, [< `Rgb | `Rgba]) t = fun x y c inputs ->
  let a = inputs.(0) in
  (get a x y 0 *. 0.21)
  +. (get a x y 1 *. 0.72)
  +. (get a x y 2 *. 0.07)

let color: ('a, 'b, [`Gray]) t = fun x y c inputs ->
  let a = inputs.(0) in
  get a x y 0

let eval op output (inputs: ('a, 'b, 'c) Image.t array) =
  let channels = channels output in
  let kind = kind output in
  for j = 0 to output.height - 1 do
    for i = 0 to output.width - 1 do
      let p = at output i j in
      for k = 0 to channels - 1 do
        let x = of_float kind (op i j k inputs) in
        Bigarray.Array1.unsafe_set p k x
      done
    done
  done

let join f a b =
  fun x y c inputs ->
    f (a x y c inputs) (b x y c inputs)

let map a f =
  fun x y c inputs ->
    f (a x y c inputs)

let ( $ ) a f = map a f
let ( &+ ) a b = join (+.) a b
let ( &- ) a b = join (-.) a b
let ( &* ) a b = join ( *. ) a b
let ( &/ ) a b = join ( /.) a b

let scalar: float -> ('a, 'b, 'c) t = fun f x y c inputs -> f

let invert_f kind f =
  to_float kind (kind_max kind) -. f

let invert: ('a, 'b, 'c) t = fun x y c inputs ->
  let a = inputs.(0) in
  let kind = kind a in
  if c = 4 then get a x y c
  else to_float kind (kind_max kind) -. get a x y c

let filter_3x3: Kernel.t -> ('a, 'b, 'c) t = fun kernel x y c inputs ->
  let a = inputs.(0) in
  if x = 0 || x >= a.Image.width - 1 || y = 0 || y >= a.Image.height - 1 then 0.0
  else
    get a (x - 1) (y - 1) c *. Kernel.get kernel 0 0
    +. get a (x - 1) y c *. Kernel.get kernel 1 0
    +. get a (x - 1) (y + 1) c *. Kernel.get kernel 2 0
    +. get a x (y - 1) c *. Kernel.get kernel 0 1
    +. get a x y c *. Kernel.get kernel 1 1
    +. get a x (y + 1) c *. Kernel.get kernel 2 1
    +. get a (x + 1) (y - 1) c *. Kernel.get kernel 0 2
    +. get a (x + 1) y c *. Kernel.get kernel 1 2
    +. get a (x + 1) (y + 1) c *. Kernel.get kernel 2 2

let filter: Kernel.t -> ('a, 'b, 'c) t = fun kernel x y c inputs ->
  let a = inputs.(0) in
  let rows = Kernel.rows kernel in
  let cols = Kernel.cols kernel in
  let r2 = rows / 2 in
  let c2 = cols / 2 in
  if x < c2 || x >= a.Image.width - c2 || y < r2 || y >= a.Image.height - r2 then 0.0
  else
    let f = ref 0.0 in
    for ky = -r2 to r2 do
      for kx = -c2 to c2 do
        f := !f +. (get a (x + kx) (y + ky) c *. Kernel.get kernel (ky + r2) (kx + c2))
      done
    done;
    !f


