open Type
open Image

type ('a, 'b, 'c) t = ('a, 'b, 'c) Image.t array -> int -> int -> int -> float

type ('a, 'b, 'c) f =
  float -> ('a, 'b, 'c) Image.t array -> int -> int -> int -> float

type ('a, 'b, 'c, 'd, 'e, 'f) filter =
  output:('d, 'e, 'f) Image.t -> ('a, 'b, 'c) Image.t array -> unit

let blend a b : ('a, 'b, 'c) t =
  Expr.op (Expr.blend a b)

let max a b : ('a, 'b, 'c) t =
  Expr.op (Expr.max a b)

let min a b : ('a, 'b, 'c) t =
  Expr.op (Expr.min a b)

let grayscale : ('a, 'b, [< `Rgb | `Rgba]) t =
 fun inputs x y _c ->
  let a = inputs.(0) in
  (get_f a x y 0 *. 0.21) +. (get_f a x y 1 *. 0.72) +. (get_f a x y 2 *. 0.07)


let color : ('a, 'b, [`Gray]) t =
 fun inputs x y _c ->
  let a = inputs.(0) in
  get_f a x y 0


let cond :
       (('a, 'b, 'c) Image.t array -> int -> int -> int -> bool)
    -> ('a, 'b, 'c) t
    -> ('a, 'b, 'c) t
    -> ('a, 'b, 'c) t =
 fun cond a b inputs x y c ->
  if cond inputs x y c then a inputs x y c else b inputs x y c


let eval ?(x = ref 0) ?(y = ref 0) ?(c = ref 0) op :
    ('a, 'b, 'c, 'd, 'e, 'f) filter =
 fun ~output inputs ->
  let width, height, channels = shape output in
  let kind = kind output in
  let of_float f = Kind.of_float kind f in
  let clamp = Kind.clamp kind in
  let op = op inputs in
  for i = 0 to length output - 1 do
    let f = op !x !y !c in
    Bigarray.Array1.unsafe_set output.data i (of_float @@ clamp f);
    match output.layout with
    | Image.Interleaved ->
        (* Increment channel index *)
        incr c;
        (* If channel index is greater than the number of channels
       * then reset channel index to 0 and increment x index *)
        let () =
          if !c >= channels then
            let () = c := 0 in
            incr x
        in
        (* If x index is greater than the width then reset x index to 0
       * and increment y index *)
        if !x >= output.width then
          let () = x := 0 in
          incr y
    | Image.Planar ->
        incr x;
        let () =
          if !x >= width then
            let () = x := 0 in
            incr y
        in
        if !y >= height then
          let () = y := 0 in
          incr c
  done

let eval_expr ?(x = ref 0) ?(y = ref 0) ?(c = ref 0) body ~output inputs =
  eval ~x ~y ~c (Expr.op ~x ~y ~c body) ~output inputs

let join f a b inputs x y c = f (a inputs x y c) (b inputs x y c)

let apply f a inputs x y c = f (a inputs x y c) inputs x y c

let scalar : float -> ('a, 'b, 'c) t = fun f _inputs _x _y _c -> f

let scalar_min : ('a, 'b) Bigarray.kind -> ('a, 'b, 'c) t =
 fun k -> scalar (Kind.min_f k)


let scalar_max : ('a, 'b) Bigarray.kind -> ('a, 'b, 'c) t =
 fun k -> scalar (Kind.max_f k)


let invert_f f : ('a, 'b, 'c) t =
 fun inputs _x _y _c ->
  let kind = Input.get inputs 0 in
  Kind.max_f (Image.kind kind) -. f


let invert : ('a, 'b, 'c) t =
 fun inputs x y c ->
  let a = inputs.(0) in
  let kind = kind a in
  if c = 4 then get_f a x y c else Kind.max_f kind -. get_f a x y c




let ( $ ) a f = apply f a

let ( &+ ) a b = join ( +. ) a b

let ( &- ) a b = join ( -. ) a b

let ( &* ) a b = join ( *. ) a b

let ( &/ ) a b = join ( /. ) a b

let ( %+ ) a b = Kernel.join ( +. ) a b

let ( %- ) a b = Kernel.join ( -. ) a b

let ( %* ) a b = Kernel.join ( *. ) a b

let ( %/ ) a b = Kernel.join ( /. ) a b

let sobel_x : ('a, 'b, 'c) t =
 fun inputs x y c -> Kernel.op_3x3 Kernel.sobel_x inputs x y c


let sobel_y : ('a, 'b, 'c) t =
 fun inputs x y c -> Kernel.op_3x3 Kernel.sobel_y inputs x y c


let[@inline] sobel inputs x y c =
  Kernel.combine Kernel.sobel_x Kernel.sobel_y inputs x y c


let gaussian_blur ?std n x y z inputs =
  Kernel.op (Kernel.gaussian ?std n) x y z inputs


let transform t inputs x y c =
  let x = float_of_int x in
  let y = float_of_int y in
  let x', y' = Transform.transform t (x, y) in
  let x0', y0' = (int_of_float (ceil x'), int_of_float (ceil y')) in
  let x1', y1' = (int_of_float (floor x'), int_of_float (floor y')) in
  if x0' >= 0 && y0' >= 0 && x0' < inputs.(0).width && y0' < inputs.(0).height
  then (get_f inputs.(0) x0' y0' c +. get_f inputs.(0) x1' y1' c) /. 2.
  else 0.


let rotate ?center angle =
  let r = Transform.rotate ?center angle in
  transform r


let scale x y =
  let s = Transform.scale x y in
  transform s


let brightness input n inputs x y c =
  Expr.op (Expr.brightness input n) inputs x y c


let threshold thresh inputs x y c =
  let a = Input.get inputs 0 in
  let v = get_f a x y c in
  if v < thresh.(c) then 0.0 else Kind.max_f (kind a)
