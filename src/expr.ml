type pixel = Color.rgb Pixel.t

type image = Image.any

type 'a t =
  | Kernel : Input.index * Kernel.t -> pixel t
  | Transform : Input.index * Transform.t -> pixel t
  | Image : Input.index -> image t
  | Input : Input.index * int t * int t -> pixel t
  | X : int t
  | Y : int t
  | Int : int -> int t
  | Float : float -> float t
  | Bool : bool -> bool t
  | Gt : 'a t * 'a t -> bool t
  | Eq : 'a t * 'a t -> bool t
  | Lt : 'a t * 'a t -> bool t
  | And : bool t * bool t -> bool t
  | Or : bool t * bool t -> bool t
  | Not : bool t -> bool t
  | Cond : bool t * 'a t * 'a t -> 'a t
  | Func : 'b t * (int -> int -> 'b -> 'a t) -> 'a t
  | Pixel : 'b Pixel.t -> pixel t
  | Pixel_get : pixel t * int t -> float t
  | Pixel_set : pixel t * int t * float t -> pixel t
  | Value : 'a -> 'a t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | Shape : Input.index -> (int * int * int) t
  | Option : 'a t option -> 'a option t

let int i = Int i

let float f = Float f

let x = X

let y = Y

let some x = Option (Some x)

let none = Option None

let pixel x = Pixel x

let get px i = Pixel_get (px, i)

let set px i f = Pixel_set (px, i, f)

let set_rgb px r g b =
  Pixel_set (Pixel_set (Pixel_set (px, int 0, r), int 1, g), int 2, b)

let func i f = Func (i, f)

let map f x = Func (x, fun _ _ a -> f a)

let ( let* ) x f = map f x

let ( let+ ) x f = map (fun x -> Value (f x)) x

let pair a b = Pair (a, b)

let map2 f a b = Func (pair a b, fun _ _ (a, b) -> f a b)

let int_of_float x = map (fun x -> Int (int_of_float x)) x

let float_of_int x = map (fun x -> Float (float_of_int x)) x

let pixel_map f (px : 'c Pixel.t t) = map (fun px -> pixel @@ Pixel.map f px) px

let default_input = function Some x -> x | None -> 0

let image ?input () = Image (default_input input)

let channels ?input () =
  map (fun (_, _, c) -> Int c) (Shape (default_input input))

let value x = Value x

let shape ?input () = Shape (default_input input)

let input ?index x y = Input (default_input index, x, y)

let fadd a b = map2 (fun a b -> a +. b |> float) a b

let fsub a b = map2 (fun a b -> a -. b |> float) a b

let fdiv a b = map2 (fun a b -> a /. b |> float) a b

let fmul a b = map2 (fun a b -> a *. b |> float) a b

let iadd a b = map2 (fun a b -> a + b |> int) a b

let isub a b = map2 (fun a b -> a - b |> int) a b

let idiv a b = map2 (fun a b -> a / b |> int) a b

let imul a b = map2 (fun a b -> a * b |> int) a b

let and_ a b = And (a, b)

let or_ a b = Or (a, b)

let not_ a = Not a

let cond v a b = Cond (v, a, b)

let pow a b = map2 (fun a b -> a ** b |> float) a b

let sqrt a = map (fun a -> Float.sqrt a |> float) a

let sin a = map (fun a -> Float.sin a |> float) a

let cos a = map (fun a -> Float.cos a |> float) a

let tan a = map (fun a -> Float.tan a |> float) a

let pi () = float Util.pi

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

let combine_kernel ?input fn k k2 =
  map2 (fun k k2 -> Kernel (default_input input, Kernel.combine fn k k2)) k k2

module Infix = struct
  let ( && ) a b = And (a, b)

  let ( || ) a b = Or (a, b)

  let ( + ) a b = iadd a b

  let ( - ) a b = isub a b

  let ( * ) a b = imul a b

  let ( / ) a b = idiv a b

  let ( +. ) a b = fadd a b

  let ( -. ) a b = fsub a b

  let ( *. ) a b = fmul a b

  let ( /. ) a b = fdiv a b

  let ( ** ) a b = pow a b

  let ( ?> ) a b = map b a

  module Kernel = struct
    let ( + ) a b = combine_kernel Float.add a b

    let ( - ) a b = combine_kernel Float.sub a b

    let ( * ) a b = combine_kernel Float.mul a b

    let ( / ) a b = combine_kernel Float.div a b
  end

  module Transform = struct
    let ( + ) a b = map2 (fun a b -> Transform (0, Transform.add a b)) a b

    let ( - ) a b = map2 (fun a b -> Transform (0, Transform.sub a b)) a b

    let ( * ) a b = map2 (fun a b -> Transform (0, Transform.mul a b)) a b

    let ( / ) a b = map2 (fun a b -> Transform (0, Transform.div a b)) a b
  end

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
  let get a b = Input (default_input input, iadd X (int a), iadd Y (int b)) in
  let open Infix.Pixel in
  map
    (fun px -> Pixel (Pixel.clamp px))
    ((get (-1) (-1) *@ k00)
    + (get (-1) 0 *@ k10)
    + (get (-1) 1 *@ k20)
    + (get 0 (-1) *@ k01)
    + (get 0 0 *@ k11)
    + (get 0 1 *@ k21)
    + (get 1 (-1) *@ k02)
    + (get 1 0 *@ k12)
    + (get 1 1 *@ k22))

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

let rotate_90 ?input () =
  let input = default_input input in
  map
    (fun i ->
      let (Image.Any image) = i in
      let open Image in
      let open Util in
      let center =
        (Float.of_int image.height /. 2., Float.of_int image.height /. 2.)
      in
      rotate ~input ~center (Angle.of_degrees 90.))
    (Image input)

let rotate_180 ?input () =
  let input = default_input input in
  map
    (fun i ->
      let (Image.Any image) = i in
      let open Image in
      let open Util in
      let center =
        (Float.of_int image.width /. 2., Float.of_int image.height /. 2.)
      in
      rotate ~input ~center (Angle.of_degrees 180.))
    (Image input)

let rotate_270 ?input () =
  let input = default_input input in
  map
    (fun i ->
      let (Image.Any image) = i in
      let open Image in
      let open Util in
      let center =
        (Float.of_int image.width /. 2., Float.of_int image.width /. 2.)
      in
      rotate ~center (Angle.of_degrees 270.) ~input)
    (Image input)

let scale ?input x y =
  let s = Transform.scale x y in
  transform ?input s

let resize ?input width height =
  let input = default_input input in
  map
    (fun input ->
      let (Image.Any image) = input in
      let x = Float.of_int width /. Float.of_int image.width in
      let y = Float.of_int height /. Float.of_int image.height in
      scale x y)
    (Image input)

let rec prepare : type a. int ref -> int ref -> a t -> Input.t -> a =
 fun x y expr inputs ->
   match expr with
   | Option None -> None
   | Option (Some a) -> Some (prepare x y a inputs)
   | Kernel (i, k) -> prepare x y (kernel ~input:i k) inputs
   | Transform (i, t) -> prepare x y (transform ~input:i t) inputs
   | Image input -> Input.get inputs input
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
   | Pixel_get (px, i) ->
       let px = prepare x y px inputs in
       let i = prepare x y i inputs in
       Pixel.get px i
   | Pixel_set (px, i, f) ->
       let px = prepare x y px inputs in
       let i = prepare x y i inputs in
       let f = prepare x y f inputs in
       Pixel.set px i f;
       px
   | Value x -> x
   | Pair (a, b) ->
       let a = prepare x y a inputs in
       let b = prepare x y b inputs in
       (a, b)
   | Shape index ->
       let (Any input) = inputs.(index) in
       Image.shape input

let transform ?input t = Transform (default_input input, t)

let kernel ?input k = Kernel (default_input input, k)

let compute_at ?(x = ref 0) ?(y = ref 0) (body : pixel t) inputs x' y' =
  (* TODO: add some form of caching *)
  x := x';
  y := y';
  prepare x y body inputs

let sobel_x ?input () = kernel_3x3 ?input Kernel.sobel_x

let sobel_y ?input () = kernel_3x3 ?input Kernel.sobel_y

let[@inline] sobel ?input () =
  kernel ?input (Kernel.combine ( +. ) Kernel.sobel_x Kernel.sobel_y)

let gaussian_blur ?std ?input n = kernel ?input (Kernel.gaussian ?std n)

let invert ?input () =
  let input = Input (default_input input, X, Y) in
  map (fun px -> Pixel.map_inplace (fun i -> 1.0 -. i) px |> pixel) input

let gamma ?input g =
  let input = Input (default_input input, X, Y) in
  pixel_map (fun i -> Float.pow i g) input

let gamma_log ?input ?gamma:g () =
  let g = match g with Some g -> g | None -> 2.2 in
  gamma ?input (1.0 /. g)

let gamma_lin ?input ?gamma:g () =
  let g = match g with Some g -> g | None -> 2.2 in
  gamma ?input g
