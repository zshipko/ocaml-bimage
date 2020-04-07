open Type
open Image

type ('a, 'b, 'c) t = ('a, 'b, 'c) Image.t array -> int -> int -> int -> float

type ('a, 'b, 'c) f =
  float -> ('a, 'b, 'c) Image.t array -> int -> int -> int -> float

type ('a, 'b, 'c, 'd, 'e, 'f) filter =
  output:('d, 'e, 'f) Image.t -> ('a, 'b, 'c) Image.t array -> unit

let blend : ('a, 'b, 'c) t =
 fun inputs x y c ->
  let a = inputs.(0) in
  let b = inputs.(1) in
  (get_f a x y c +. get_f b x y c) /. 2.


let max : ('a, 'b, 'c) t =
 fun inputs x y c ->
  let a = inputs.(0) in
  let b = inputs.(1) in
  max (get_f a x y c) (get_f b x y c)


let min : ('a, 'b, 'c) t =
 fun inputs x y c ->
  let a = inputs.(0) in
  let b = inputs.(1) in
  min (get_f a x y c) (get_f b x y c)


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


let kernel_3x3 : Kernel.t -> ('a, 'b, 'c) t =
 fun kernel ->
  let k00 = Kernel.get kernel 0 0 in
  let k10 = Kernel.get kernel 1 0 in
  let k20 = Kernel.get kernel 2 0 in
  let k01 = Kernel.get kernel 0 1 in
  let k11 = Kernel.get kernel 1 1 in
  let k21 = Kernel.get kernel 2 1 in
  let k02 = Kernel.get kernel 0 2 in
  let k12 = Kernel.get kernel 1 2 in
  let k22 = Kernel.get kernel 2 2 in
  fun inputs x y c ->
    let a = Input.get inputs 0 in
    (get_f a (x - 1) (y - 1) c *. k00)
    +. (get_f a (x - 1) y c *. k10)
    +. (get_f a (x - 1) (y + 1) c *. k20)
    +. (get_f a x (y - 1) c *. k01)
    +. (get_f a x y c *. k11)
    +. (get_f a x (y + 1) c *. k21)
    +. (get_f a (x + 1) (y - 1) c *. k02)
    +. (get_f a (x + 1) y c *. k12)
    +. (get_f a (x + 1) (y + 1) c *. k22)


let kernel kernel =
  let rows = Kernel.rows kernel in
  let cols = Kernel.cols kernel in
  let r2 = rows / 2 in
  let c2 = cols / 2 in
  if rows = 3 && cols = 3 then kernel_3x3 kernel
  else fun inputs x y c ->
    let a = Input.get inputs 0 in
    let f = ref 0.0 in
    for ky = -r2 to r2 do
      let kr = kernel.(ky + r2) in
      for kx = -c2 to c2 do
        f := !f +. (get_f a (x + kx) (y + ky) c *. kr.(kx + c2))
      done
    done;
    !f


let join_kernel fn kernel kernel2 =
  let rows = Kernel.rows kernel in
  let cols = Kernel.cols kernel in
  let r2 = rows / 2 in
  let c2 = cols / 2 in
  fun inputs x y c ->
    let a = Input.get inputs 0 in
    let f = ref 0.0 in
    for ky = -r2 to r2 do
      let kr = kernel.(ky + r2) in
      let kr2 = kernel2.(ky + r2) in
      for kx = -c2 to c2 do
        let v = get_f a (x + kx) (y + ky) c in
        f := !f +. fn (v *. kr.(kx + c2)) (v *. kr2.(kx + c2))
      done
    done;
    !f

let combine_kernels kernel kernel2 =
  let r2 = Kernel.rows kernel / 2 in
  let c2 = Kernel.cols kernel / 2 in
  let r2' = Kernel.rows kernel2 / 2 in
  let c2' = Kernel.cols kernel2 / 2 in
  fun inputs x y c ->
    let a = Input.get inputs 0 in
    let f = ref 0.0 in
    for ky = -r2 to r2 do
      let kr = kernel.(ky + r2) in
      for kx = -c2 to c2 do
        let v = get_f a (x + kx) (y + ky) c in
        f := !f +. v *. kr.(kx + c2)
      done
    done;
    for ky = -r2' to r2' do
      let kr = kernel2.(ky + r2') in
      for kx = -c2' to c2' do
        let v = get_f a (x + kx) (y + ky) c in
        f := !f +. v *. kr.(kx + c2')
      done
    done;
    !f



let ( $ ) a f = apply f a

let ( &+ ) a b = join ( +. ) a b

let ( &- ) a b = join ( -. ) a b

let ( &* ) a b = join ( *. ) a b

let ( &/ ) a b = join ( /. ) a b

let ( %+ ) a b = join_kernel ( +. ) a b

let ( %- ) a b = join_kernel ( -. ) a b

let ( %* ) a b = join_kernel ( *. ) a b

let ( %/ ) a b = join_kernel ( /. ) a b

let sobel_x : ('a, 'b, 'c) t =
 fun inputs x y c -> kernel_3x3 Kernel.sobel_x inputs x y c


let sobel_y : ('a, 'b, 'c) t =
 fun inputs x y c -> kernel_3x3 Kernel.sobel_y inputs x y c


let[@inline] sobel inputs x y c =
  combine_kernels Kernel.sobel_x Kernel.sobel_y inputs x y c


let gaussian_blur ?std n x y z inputs =
  kernel (Kernel.gaussian ?std n) x y z inputs


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


let brightness n inputs x y c =
  let a = Input.get inputs 0 in
  get_f a x y c *. n


let threshold thresh inputs x y c =
  let a = Input.get inputs 0 in
  let v = get_f a x y c in
  if v < thresh.(c) then 0.0 else Kind.max_f (kind a)
