open Type
open Image

type ('a, 'b, 'c) t = ('a, 'b, 'c) Image.t array -> int -> int -> int -> float

type ('a, 'b, 'c) f =
  float Expr.t -> ('a, 'b, 'c) Image.t array -> int -> int -> int -> float

type ('a, 'b, 'c, 'd, 'e, 'f) filter =
  output:('d, 'e, 'f) Image.t -> ('a, 'b, 'c) Image.t array -> unit

let blend ?(input = 0) ?(input1 = 1) : ('a, 'b, 'c) t =
  Expr.op (Expr.blend input input1)

let max ?(input = 0) ?(input1 = 1) : ('a, 'b, 'c) t =
  Expr.op (Expr.max input input1)

let min ?(input = 0) ?(input1 = 1) : ('a, 'b, 'c) t =
  Expr.op (Expr.min input input1)

let grayscale ?(input = 0) : ('a, 'b, [< `Rgb | `Rgba ]) t =
  Expr.op (Expr.grayscale input)

let color ?(input = 0) : ('a, 'b, [ `Gray ]) t = Expr.op (Expr.color input)

let cond :
    (('a, 'b, 'c) Image.t array -> int -> int -> int -> bool) ->
    ('a, 'b, 'c) t ->
    ('a, 'b, 'c) t ->
    ('a, 'b, 'c) t =
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

let apply f a inputs x y c = f (Expr.float @@ a inputs x y c) inputs x y c

let scalar : float -> ('a, 'b, 'c) t = fun f _inputs _x _y _c -> f

let scalar_min : ('a, 'b) Bigarray.kind -> ('a, 'b, 'c) t =
 fun k -> scalar (Kind.min_f k)

let scalar_max : ('a, 'b) Bigarray.kind -> ('a, 'b, 'c) t =
 fun k -> scalar (Kind.max_f k)

let invert ?(input = 0) : ('a, 'b, 'c) t =
 fun inputs x y c ->
   let a = inputs.(input) in
   let kind = kind a in
   if c = 4 then get_f a x y c else Kind.max_f kind -. get_f a x y c

module Infix = struct
  let ( $ ) a f = apply f a

  let ( &+ ) a b = join ( +. ) a b

  let ( &- ) a b = join ( -. ) a b

  let ( &* ) a b = join ( *. ) a b

  let ( &/ ) a b = join ( /. ) a b
end

let sobel_x ?(input = 0) : ('a, 'b, 'c) t =
  Expr.op (Expr.kernel_3x3 input Kernel.sobel_x)

let sobel_y ?(input = 0) : ('a, 'b, 'c) t =
  Expr.op (Expr.kernel_3x3 input Kernel.sobel_y)

let[@inline] sobel ?(input = 0) =
  Expr.op (Expr.combine_kernel input Kernel.sobel_x Kernel.sobel_y)

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
