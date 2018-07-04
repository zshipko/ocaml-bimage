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


