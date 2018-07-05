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
        let f = Kind.clamp kind (op i j k inputs) in
        let x = Kind.of_float kind f in
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
  Kind.max_f kind -. f

let invert: ('a, 'b, 'c) t = fun x y c inputs ->
  let a = inputs.(0) in
  let kind = kind a in
  if c = 4 then get a x y c
  else Kind.max_f kind -. get a x y c

let filter_3x3: Kernel.t -> ('a, 'b, 'c) t = fun kernel ->
  let k00 = Kernel.get kernel 0 0 in
  let k10 = Kernel.get kernel 1 0 in
  let k20 = Kernel.get kernel 2 0 in
  let k01 = Kernel.get kernel 0 1 in
  let k11 = Kernel.get kernel 1 1 in
  let k21 = Kernel.get kernel 2 1 in
  let k02 = Kernel.get kernel 0 2 in
  let k12 = Kernel.get kernel 1 2 in
  let k22 = Kernel.get kernel 2 2 in
  fun x y c inputs ->
    let a = inputs.(0) in
    if x = 0 || x >= a.Image.width - 1 || y = 0 || y >= a.Image.height - 1 then get a x y c
    else
      Kind.clamp (kind a)
        (get a (x - 1) (y - 1) c *. k00
         +. get a (x - 1) y c *. k10
         +. get a (x - 1) (y + 1) c *. k20
         +. get a x (y - 1) c *. k01
         +. get a x y c *. k11
         +. get a x (y + 1) c *. k21
         +. get a (x + 1) (y - 1) c *. k02
         +. get a (x + 1) y c *. k12
         +. get a (x + 1) (y + 1) c *. k22)

let filter: Kernel.t -> ('a, 'b, 'c) t = fun kernel ->
  let rows = Kernel.rows kernel in
  let cols = Kernel.cols kernel in
  let r2 = rows / 2 in
  let c2 = cols / 2 in
  if rows = 3 && cols = 3 then
    filter_3x3 kernel
  else
    fun x y c inputs ->
      let a = inputs.(0) in
      if x < c2 || x >= a.Image.width - c2 || y < r2 || y >= a.Image.height - r2 then get a x y c
      else
        let f = ref 0.0 in
        for ky = -r2 to r2 do
          for kx = -c2 to c2 do
            f := !f +. (get a (x + kx) (y + ky) c *. Kernel.get kernel (ky + r2) (kx + c2))
          done
        done;
        Kind.clamp (kind a) !f

let sobel_x: ('a, 'b, 'c) t = fun x y c inputs ->
  filter_3x3 (Kernel.of_array [|
    [| 1.0; 0.0; -1.0 |];
    [| 2.0; 0.0; -2.0 |];
    [| 1.0; 0.0; -1.0 |];
  |]) x y c inputs

let sobel_y: ('a, 'b, 'c) t = fun x y c inputs ->
  filter_3x3 (Kernel.of_array [|
    [|  1.0;  2.0;  1.0 |];
    [|  0.0;  0.0;  0.0 |];
    [| -1.0; -2.0; -1.0 |];
  |]) x y c inputs

let sobel x y c inputs =
  (sobel_x &+ sobel_y) x y c inputs

