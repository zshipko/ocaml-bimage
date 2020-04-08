type 'a t =
  | Kernel : Input.index * Kernel.t -> float t
  | Input : Input.index * int t * int t * int t -> float t
  | X : int t
  | Y : int t
  | C : int t
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
  | Func : 'b t * (int -> int -> int -> 'b -> 'a t) -> 'a t
  | Pixel : Input.index * int t * int t -> Pixel.t t
  | Pixel_norm : Input.index * int t * int t -> Pixel.t t
  | Value : 'a -> 'a t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | Kind_min : Input.index -> float t
  | Kind_max : Input.index -> float t
  | Channels : Input.index -> int t
  | Shape : Input.index -> (int * int * int) t

let int i = Int i

let int_of_float x = Int_of_float x

let float_of_int x = Float_of_int x

let float f = Float f

let x = X

let y = Y

let c = C

(*let kernel k = Kernel k*)

let func i f = Func (i, f)

let map f x = Func (x, fun _ _ _ x -> f x)

let pair a b = Pair (a, b)

let pixel input x y = Pixel (input, x, y)

let pixel_norm input x y = Pixel_norm (input, x, y)

let kind_min input = Kind_min input

let kind_max input = Kind_max input

let channels input = Channels input

let value x = Value x

let shape i = Shape i

let input i x y c = Input (i, x, y, c)

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

let blend a b : float t =
  map
    (fun (a, b) -> Float ((a +. b) /. 2.0))
    (pair (input a X Y C) (input b X Y C))

let min a b : float t =
  map
    (fun (a, b) -> if a < b then Float a else Float b)
    (pair (input a X Y C) (input b X Y C))

let max a b : float t =
  map
    (fun (a, b) -> if a > b then Float a else Float b)
    (pair (input a X Y C) (input b X Y C))

let brightness i scale : float t =
  func (pair scale (input i X Y C)) (fun _ _ _ (scale, x) -> Float (x *. scale))

let grayscale input : float t =
  func (pixel input X Y) (fun _ _ _ px ->
      let px = Pixel.data px in
      Float ((px.{0} *. 0.21) +. (px.{1} *. 0.72) +. (px.{2} *. 0.07)))

let color input : float t =
  func (pixel input X Y) (fun _ _ _ px ->
      let px = Pixel.data px in
      Float px.{0})

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
end

let kernel_3x3 input kernel =
  let k00 = Kernel.get kernel 0 0 |> float in
  let k10 = Kernel.get kernel 1 0 |> float in
  let k20 = Kernel.get kernel 2 0 |> float in
  let k01 = Kernel.get kernel 0 1 |> float in
  let k11 = Kernel.get kernel 1 1 |> float in
  let k21 = Kernel.get kernel 2 1 |> float in
  let k02 = Kernel.get kernel 0 2 |> float in
  let k12 = Kernel.get kernel 1 2 |> float in
  let k22 = Kernel.get kernel 2 2 |> float in
  let open Infix in
  let get a b = Input (input, X + int a, Y + int b, C) in
  (get (-1) (-1) *. k00)
  +. (get (-1) 0 *. k10)
  +. (get (-1) 1 *. k20)
  +. (get 0 (-1) *. k01)
  +. (get 0 0 *. k11)
  +. (get 0 1 *. k21)
  +. (get 1 (-1) *. k02)
  +. (get 1 0 *. k12)
  +. (get 1 1 *. k22)

let kernel input k =
  let rows = Kernel.rows k in
  let cols = Kernel.cols k in
  let r2 = rows / 2 in
  let c2 = cols / 2 in
  if rows = 3 && cols = 3 then kernel_3x3 input k
  else
    func (value k) (fun x y c k ->
        let f = ref (Float 0.0) in
        for ky = -r2 to r2 do
          let kr = k.(ky + r2) in
          for kx = -c2 to c2 do
            let idx = kx + c2 in
            f :=
              Infix.(
                !f
                +. Input (input, int x + int kx, int y + int ky, int c)
                   *. float kr.(idx))
          done
        done;
        !f)

let join_kernel input fn k k2 =
  let rows = Kernel.rows k in
  let cols = Kernel.cols k in
  let r2 = rows / 2 in
  let c2 = cols / 2 in
  func
    (pair (value k) (value k2))
    (fun x y c (k, k2) ->
      let f = ref (Float 0.0) in
      for ky = -r2 to r2 do
        let kr = k.(ky + r2) in
        let kr2 = k2.(ky + r2) in
        for kx = -c2 to c2 do
          let idx = kx + c2 in
          let v =
            Infix.(Input (input, int x + int kx, int y + int ky, int c))
          in
          f := Infix.(!f +. fn (v *. float kr.(idx)) (v *. float kr2.(idx)))
        done
      done;
      !f)

let combine_kernel input k k2 =
  let r2 = Kernel.rows k / 2 in
  let c2 = Kernel.cols k / 2 in
  let r2' = Kernel.rows k2 / 2 in
  let c2' = Kernel.cols k2 / 2 in
  func
    (pair (value k) (value k2))
    (fun x y c (k, k2) ->
      let f = ref (Float 0.0) in
      for ky = -r2 to r2 do
        let kr = k.(ky + r2) in
        for kx = -c2 to c2 do
          let idx = kx + c2 in
          let v =
            Infix.(Input (input, int x + int kx, int y + int ky, int c))
          in
          f := Infix.(!f +. (v *. float kr.(idx)))
        done
      done;
      for ky = -r2' to r2' do
        let kr = k2.(ky + r2') in
        for kx = -c2' to c2' do
          let idx = kx + c2' in
          let v =
            Infix.(Input (input, int x + int kx, int y + int ky, int c))
          in
          f := Infix.(!f +. (v *. float kr.(idx)))
        done
      done;
      !f)

let transform input t =
  func (value t) (fun x y c t ->
      let x = Float.of_int x in
      let y = Float.of_int y in
      let x', y' = Transform.transform t (x, y) in
      let x0', y0' = (Float.to_int (ceil x'), Float.to_int (ceil y')) in
      let x1', y1' = (Float.to_int (floor x'), Float.to_int (floor y')) in
      func (shape input) (fun _ _ _ (w, h, _) ->
          if x0' >= 0 && y0' >= 0 && x0' < w && y0' < h then
            let open Infix in
            ( Input (input, int x0', int y0', int c)
            +. Input (input, int x1', int y1', int c) )
            /. float 2.0
          else float 0.0))

let rotate input ?center angle =
  let r = Transform.rotate ?center angle in
  transform input r

let scale input x y =
  let s = Transform.scale x y in
  transform input s

let threshold input thresh =
  let v = Input (input, X, Y, C) in
  func (value ()) (fun _ _ c _ ->
      cond
        (Lt (v, float thresh.(c mod Array.length thresh)))
        (float 0.0) (kind_max input))

let rec prepare :
    type a. int ref -> int ref -> int ref -> a t -> ('b, 'c, 'd) Input.t -> a =
 fun x y c expr inputs ->
   match expr with
   | Kernel (i, k) -> prepare x y c (kernel i k) inputs
   | Input (input, x', y', c') ->
       let x' = prepare x y c x' inputs in
       let y' = prepare x y c y' inputs in
       let c' = prepare x y c c' inputs in
       let f : float = Image.get_f (Input.get inputs input) x' y' c' in
       f
   | X -> !x
   | Y -> !y
   | C -> !c
   | Bool b -> b
   | Int i -> i
   | Float f -> f
   | Float_of_int i ->
       let a = prepare x y c i inputs in
       Float.of_int a
   | Int_of_float f ->
       let a = prepare x y c f inputs in
       Float.to_int a
   | Fadd (Kernel (i, a), Kernel (_, b)) ->
       let a = join_kernel i (fun a b -> Fadd (a, b)) a b in
       prepare x y c a inputs
   | Fadd (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a +. b
   | Fsub (Kernel (i, a), Kernel (_, b)) ->
       let a = join_kernel i (fun a b -> Fsub (a, b)) a b in
       prepare x y c a inputs
   | Fsub (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a -. b
   | Fmul (Kernel (i, a), Kernel (_, b)) ->
       let a = join_kernel i (fun a b -> Fmul (a, b)) a b in
       prepare x y c a inputs
   | Fmul (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a *. b
   | Fdiv (Kernel (i, a), Kernel (_, b)) ->
       let a = join_kernel i (fun a b -> Fdiv (a, b)) a b in
       prepare x y c a inputs
   | Fdiv (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a /. b
   | Fpow (Kernel (i, a), Kernel (_, b)) ->
       let a = join_kernel i (fun a b -> Fpow (a, b)) a b in
       prepare x y c a inputs
   | Fpow (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a ** b
   | Fsqrt a ->
       let a = prepare x y c a inputs in
       sqrt a
   | Fsin a ->
       let a = prepare x y c a inputs in
       sin a
   | Fcos a ->
       let a = prepare x y c a inputs in
       cos a
   | Ftan a ->
       let a = prepare x y c a inputs in
       tan a
   | Fmod (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       mod_float a b
   | Iadd (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a + b
   | Isub (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a - b
   | Imul (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a * b
   | Idiv (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a / b
   | Imod (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a mod b
   | Gt (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a > b
   | Eq (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a = b
   | Lt (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a < b
   | And (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a && b
   | Or (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a || b
   | Not a ->
       let a = prepare x y c a inputs in
       not a
   | Cond (cond, a, b) ->
       let cond = prepare x y c cond inputs in
       if cond then prepare x y c a inputs else prepare x y c b inputs
   | Func (f, func) ->
       let x' = prepare x y c X inputs in
       let y' = prepare x y c Y inputs in
       let c' = prepare x y c C inputs in
       let f = prepare x y c f inputs in
       let r = func x' y' c' f in
       prepare x y c r inputs
   | Pixel (index, x', y') ->
       let x' = prepare x y c x' inputs in
       let y' = prepare x y c y' inputs in
       Image.get_pixel inputs.(index) x' y'
   | Pixel_norm (index, x', y') ->
       let x' = prepare x y c x' inputs in
       let y' = prepare x y c y' inputs in
       Image.get_pixel_norm inputs.(index) x' y'
   | Value x -> x
   | Pair (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       (a, b)
   | Kind_min index -> Image.kind inputs.(index) |> Type.Kind.min_f
   | Kind_max index -> Image.kind inputs.(index) |> Type.Kind.max_f
   | Channels index -> Image.channels inputs.(index)
   | Shape index -> Image.shape inputs.(index)

let op ?(x = ref 0) ?(y = ref 0) ?(c = ref 0) body =
  let f = prepare x y c body in
  fun inputs x' y' c' ->
    x := x';
    y := y';
    c := c';
    f inputs

let pow a b = Fpow (a, b)

let sqrt a = Fsqrt a

let sin a = Fsin a

let cos a = Fcos a

let tan a = Ftan a

let pi () = float Util.pi
