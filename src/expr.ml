type pixel = Color.rgb Pixel.t

type 'a t =
  | Kernel : Input.index * Kernel.t -> pixel t
  | Input : Input.index * int t * int t -> pixel t
  | X : int t
  | Y : int t
  | Int : int -> int t
  | Float : float -> float t
  | Bool : bool -> bool t
  | Float_of_int : int t -> float t
  | Int_of_float : float t -> int t
  | Fadd : float t * float t -> float t
  | Fsub : float t * float t -> float t
  | Fmul : float t * float t -> float t
  | Fdiv : float t * float t -> float t
  | Fpow : float t * float t -> float t
  | Fsqrt : float t -> float t
  | Fsin : float t -> float t
  | Fcos : float t -> float t
  | Ftan : float t -> float t
  | Fmod : float t * float t -> float t
  | Iadd : int t * int t -> int t
  | Isub : int t * int t -> int t
  | Imul : int t * int t -> int t
  | Idiv : int t * int t -> int t
  | Imod : int t * int t -> int t
  | Gt : 'a t * 'a t -> bool t
  | Eq : 'a t * 'a t -> bool t
  | Lt : 'a t * 'a t -> bool t
  | And : bool t * bool t -> bool t
  | Or : bool t * bool t -> bool t
  | Not : bool t -> bool t
  | Cond : bool t * 'a t * 'a t -> 'a t
  | Func : 'b t * (int -> int -> 'b -> 'a t) -> 'a t
  | Pixel : 'b Pixel.t -> Color.rgb Pixel.t t
  | Value : 'a -> 'a t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | Type_min : Input.index -> float t
  | Type_max : Input.index -> float t
  | Channels : Input.index -> int t
  | Shape : Input.index -> (int * int * int) t
  | Option : 'a t option -> 'a option t

let int i = Int i

let int_of_float x = Int_of_float x

let float_of_int x = Float_of_int x

let float f = Float f

let x = X

let y = Y

let some x = Option (Some x)

let none = Option None

let pixel x = Pixel x

let func i f = Func (i, f)

let map f x = Func (x, fun _ _ a -> f a)

let pixel_map f (px : 'c Pixel.t t) = map (fun px -> pixel @@ Pixel.map f px) px

let pair a b = Pair (a, b)

let map2 f a b = Func (pair a b, fun _ _ (a, b) -> f a b)

let default_input = function Some x -> x | None -> 0

let type_min ?input () = Type_min (default_input input)

let type_max ?input () = Type_max (default_input input)

let channels ?input () = Channels (default_input input)

let value x = Value x

let shape ?input () = Shape (default_input input)

let input ?index x y = Input (default_input index, x, y)

let fadd a b = Fadd (a, b)

let fsub a b = Fsub (a, b)

let fdiv a b = Fdiv (a, b)

let fmul a b = Fmul (a, b)

let iadd a b = Iadd (a, b)

let isub a b = Isub (a, b)

let idiv a b = Idiv (a, b)

let imul a b = Imul (a, b)

let and_ a b = And (a, b)

let or_ a b = Or (a, b)

let not_ a = Not a

let cond v a b = Cond (v, a, b)

let blend ?input0 ?input1 () : pixel t =
  let a = Input.or_default input0 in
  let b = match input1 with Some x -> x | None -> 1 in
  map
    (fun (a, b) -> Pixel Pixel.Infix.((a + b) /@ 2.))
    (pair (input ~index:a X Y) (input ~index:b X Y))

let min ?input0 ?input1 () : pixel t =
  let a = Input.or_default input0 in
  let b = match input1 with Some x -> x | None -> 1 in
  map
    (fun (a, b) -> if a < b then Pixel a else Pixel b)
    (pair (input ~index:a X Y) (input ~index:b X Y))

let max ?input0 ?input1 () : pixel t =
  let a = Input.or_default input0 in
  let b = match input1 with Some x -> x | None -> 1 in
  map
    (fun (a, b) -> if a > b then Pixel a else Pixel b)
    (pair (input ~index:a X Y) (input ~index:b X Y))

let brightness ?input:i scale : pixel t =
  func
    (pair scale (input ?index:i X Y))
    (fun _ _ (scale, x) -> Pixel Pixel.Infix.(x *@ scale))

let grayscale ?input:i () : pixel t =
  func (input ?index:i X Y) (fun _ _ px ->
      Pixel (Pixel.of_rgb Color.gray px |> Pixel.to_rgb))

let color ?input:i () : pixel t =
  func (input ?index:i X Y) (fun _ _ px -> Pixel px)

module Infix = struct
  let ( && ) a b = And (a, b)

  let ( || ) a b = Or (a, b)

  let ( + ) a b = Iadd (a, b)

  let ( - ) a b = Isub (a, b)

  let ( * ) a b = Imul (a, b)

  let ( / ) a b = Idiv (a, b)

  let ( +. ) a b = Fadd (a, b)

  let ( -. ) a b = Fsub (a, b)

  let ( *. ) a b = Fmul (a, b)

  let ( /. ) a b = Fdiv (a, b)

  let ( ** ) a b = Fpow (a, b)

  let ( ?> ) a b = map b a

  module Pixel = struct
    let ( + ) a b = map2 (fun a b -> Pixel Pixel.Infix.(a + b)) a b

    let ( - ) a b = map2 (fun a b -> Pixel Pixel.Infix.(a - b)) a b

    let ( * ) a b = map2 (fun a b -> Pixel Pixel.Infix.(a * b)) a b

    let ( / ) a b = map2 (fun a b -> Pixel Pixel.Infix.(a / b)) a b

    let ( +@ ) a (b : float t) =
      map2 (fun a b -> Pixel Pixel.Infix.(a +@ b)) a b

    let ( -@ ) a (b : float t) =
      map2 (fun a b -> Pixel Pixel.Infix.(a -@ b)) a b

    let ( *@ ) a (b : float t) =
      map2 (fun a b -> Pixel Pixel.Infix.(a *@ b)) a b

    let ( /@ ) a (b : float t) =
      map2 (fun a b -> Pixel Pixel.Infix.(a /@ b)) a b
  end
end

let kernel_3x3 ?input kernel : pixel t =
  let k00 = Kernel.get kernel 0 0 |> float in
  let k10 = Kernel.get kernel 1 0 |> float in
  let k20 = Kernel.get kernel 2 0 |> float in
  let k01 = Kernel.get kernel 0 1 |> float in
  let k11 = Kernel.get kernel 1 1 |> float in
  let k21 = Kernel.get kernel 2 1 |> float in
  let k02 = Kernel.get kernel 0 2 |> float in
  let k12 = Kernel.get kernel 1 2 |> float in
  let k22 = Kernel.get kernel 2 2 |> float in
  let get a b = Input (default_input input, Iadd (X, int a), Iadd (Y, int b)) in
  let open Infix.Pixel in
  map
    (fun px -> Pixel (Pixel.clamp px))
    ( (get (-1) (-1) *@ k00)
    + (get (-1) 0 *@ k10)
    + (get (-1) 1 *@ k20)
    + (get 0 (-1) *@ k01)
    + (get 0 0 *@ k11)
    + (get 0 1 *@ k21)
    + (get 1 (-1) *@ k02)
    + (get 1 0 *@ k12)
    + (get 1 1 *@ k22) )

let kernel ?input k =
  let rows = Kernel.rows k in
  let cols = Kernel.cols k in
  let r2 = rows / 2 in
  let c2 = cols / 2 in
  if rows = 3 && cols = 3 then kernel_3x3 ?input k
  else
    func (value k) (fun x y k ->
        let f = ref (Pixel (Pixel.empty Color.rgb)) in
        for ky = -r2 to r2 do
          let kr = k.(ky + r2) in
          for kx = -c2 to c2 do
            let idx = kx + c2 in
            let input =
              Infix.(
                Input (default_input input, int x + int kx, int y + int ky))
            in
            f := Infix.Pixel.(!f + (input *@ float kr.(idx)))
          done
        done;
        map (fun px -> Pixel (Pixel.clamp px)) !f)

let combine_kernel ?input fn k k2 =
  Kernel (default_input input, Kernel.combine fn k k2)

let transform ?input t =
  let input = default_input input in
  func (value t) (fun x y t ->
      let x = Float.of_int x in
      let y = Float.of_int y in
      let x', y' = Transform.transform t (x, y) in
      let x0', y0' = (Float.to_int (ceil x'), Float.to_int (ceil y')) in
      let x1', y1' = (Float.to_int (floor x'), Float.to_int (floor y')) in
      func (shape ~input ()) (fun _ _ (w, h, _) ->
          if x0' >= 0 && y0' >= 0 && x0' < w && y0' < h then
            let open Infix.Pixel in
            (Input (input, int x0', int y0') + Input (input, int x1', int y1'))
            /@ float 2.0
          else pixel (Pixel.empty Color.rgb)))

let rotate ?input ?center angle =
  let r = Transform.rotate ?center angle in
  transform ?input r

let scale ?input x y =
  let s = Transform.scale x y in
  transform ?input s

let rec prepare : type a. int ref -> int ref -> a t -> Input.t -> a =
 fun x y expr inputs ->
   match expr with
   | Option None -> None
   | Option (Some a) -> Some (prepare x y a inputs)
   | Kernel (i, k) -> prepare x y (kernel ~input:i k) inputs
   | Input (input, x', y') ->
       let x' = prepare x y x' inputs in
       let y' = prepare x y y' inputs in
       let (Any input) = Input.get inputs input in
       let f = Image.get_pixel input x' y' in
       Pixel.to_rgb f
   | X -> !x
   | Y -> !y
   | Bool b -> b
   | Int i -> i
   | Float f -> f
   | Float_of_int i ->
       let a = prepare x y i inputs in
       Float.of_int a
   | Int_of_float f ->
       let a = prepare x y f inputs in
       Float.to_int a
   | Fadd (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       a +. b
   | Fsub (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       a -. b
   | Fmul (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       a *. b
   | Fdiv (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       a /. b
   | Fpow (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       a ** b
   | Fsqrt a ->
       let a = prepare x y a inputs in
       sqrt a
   | Fsin a ->
       let a = prepare x y a inputs in
       sin a
   | Fcos a ->
       let a = prepare x y a inputs in
       cos a
   | Ftan a ->
       let a = prepare x y a inputs in
       tan a
   | Fmod (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       mod_float a b
   | Iadd (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       a + b
   | Isub (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       a - b
   | Imul (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       a * b
   | Idiv (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       a / b
   | Imod (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       a mod b
   | Gt (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       a > b
   | Eq (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       a = b
   | Lt (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       a < b
   | And (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       a && b
   | Or (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       a || b
   | Not a ->
       let a = prepare x y a inputs in
       not a
   | Cond (cond, a, b) ->
       let cond = prepare x y cond inputs in
       if cond then prepare x y a inputs else prepare x y b inputs
   | Func (f, func) ->
       let x' = prepare x y X inputs in
       let y' = prepare x y Y inputs in
       let f = prepare x y f inputs in
       let r = func x' y' f in
       prepare x y r inputs
   | Pixel px -> Pixel.to_rgb px
   | Value x -> x
   | Pair (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       (a, b)
   | Type_min index ->
       let (Any input) = inputs.(index) in
       Image.ty input |> Type.min_f
   | Type_max index ->
       let (Any input) = inputs.(index) in
       Image.ty input |> Type.max_f
   | Channels index ->
       let (Any input) = inputs.(index) in
       Image.channels input
   | Shape index ->
       let (Any input) = inputs.(index) in
       Image.shape input

let compute_at ?(x = ref 0) ?(y = ref 0) (body : pixel t) inputs x' y' =
  x := x';
  y := y';
  prepare x y body inputs

let pow a b = Fpow (a, b)

let sqrt a = Fsqrt a

let sin a = Fsin a

let cos a = Fcos a

let tan a = Ftan a

let pi () = float Util.pi

let sobel_x ?input () = kernel_3x3 ?input Kernel.sobel_x

let sobel_y ?input () = kernel_3x3 ?input Kernel.sobel_y

let[@inline] sobel ?input () =
  kernel ?input (Kernel.combine ( +. ) Kernel.sobel_x Kernel.sobel_y)

let gaussian_blur ?std ?input n = kernel ?input (Kernel.gaussian ?std n)

let invert ?input () =
  let input = Input (default_input input, X, Y) in
  map (fun px -> Pixel.map_inplace (fun i -> 1.0 -. i) px |> pixel) input
