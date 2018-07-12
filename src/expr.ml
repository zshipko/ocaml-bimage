open Type

type _ t =
  | Input: int * int t * int t * int t -> float t
  | X: int t
  | Y: int t
  | C: int t
  | Int: int -> int t
  | Float: float -> float t
  | PI: float t
  | Float_of_int: int t -> float t
  | Int_of_float: float t -> int t
  | Fadd: float t * float t -> float t
  | Fsub: float t * float t -> float t
  | Fmul: float t * float t -> float t
  | Fdiv: float t * float t -> float t
  | Fpow: float t * float t -> float t
  | Fsqrt: float t -> float t
  | Fsin: float t -> float t
  | Fcos: float t -> float t
  | Ftan: float t -> float t
  | Iadd: int t * int t -> int t
  | Isub: int t * int t -> int t
  | Imul: int t * int t -> int t
  | Idiv: int t * int t -> int t

let rec compile: type a. int ref -> int ref -> int ref -> a t -> ('b, 'c, 'd) Input.t -> a = fun x y c expr inputs ->
  match expr with
  | Input (index, x', y', c') ->
    let x' = compile x y c x' inputs in
    let y' = compile x y c y' inputs in
    let c' = compile x y c c' inputs in
    let a = Input.get inputs index in
    Image.get a x' y' c'
  | X -> !x
  | Y -> !y
  | C -> !c
  | Int i -> i
  | Float f -> f
  | PI -> Util.pi
  | Float_of_int i ->
      let a = compile x y c i inputs in
      float_of_int a
  | Int_of_float f ->
      let a = compile x y c f inputs in
      int_of_float a
  | Fadd (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a +. b
  | Fsub (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a -. b
  | Fmul (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a *. b
  | Fdiv (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a /. b
  | Fpow (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a ** b
  | Fsqrt a ->
      let a = compile x y c a inputs in
      sqrt a
  | Fsin a ->
      let a = compile x y c a inputs in
      sin a
  | Fcos a ->
      let a = compile x y c a inputs in
      cos a
  | Ftan a ->
      let a = compile x y c a inputs in
      tan a
  | Iadd (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a + b
  | Isub (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a - b
  | Imul (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a * b
  | Idiv (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a / b

let op body =
  let x = ref 0 in
  let y = ref 0 in
  let c = ref 0 in
  let f = compile x y c body in
  fun x' y' c' inputs ->
    x := x';
    y := y';
    c := c';
    f inputs

let eval ?(x = ref 0) ?(y = ref 0) ?(c = ref 0) body ~output inputs =
  let open Image in
  let channels = channels output in
  let kind = kind output in
  let of_float = Kind.of_float kind in
  let clamp = Kind.clamp kind in
  let op = compile x y c body in
  for i = 0 to length output - 1 do
    let f = clamp @@ op inputs in
    Bigarray.Array1.unsafe_set output.data i (of_float f);

    (* Increment channel index *)
    incr c;

    (* If channel index is greater than the number of channels
     * then reset channel index to 0 and increment x index *)
    let () = if !c = channels then
      let () = c := 0 in
      incr x
    in

    (* If x index is greater than the width then reset x index to 0
     * and increment y index *)
    if !x = output.width then
      let () = x := 0 in
      incr y
  done

let int i = Int i
let int_of_float x = Int_of_float x
let float_of_int x = Float_of_int x
let float f = Float f
let x = X
let y = Y
let c = C
let input i x y c = Input (i, x, y, c)
let fadd a b = Fadd (a, b)
let fsub a b = Fsub (a, b)
let fdiv a b = Fdiv (a, b)
let fmul a b = Fmul (a, b)
let fpow a b = Fpow (a, b)
let fsqrt a = Fsqrt a
let fsin a = Fsin a
let fcos a = Fcos a
let ftan a = Ftan a
let pi = PI

module Infix = struct
  let ( + ) a b = Iadd (a, b)
  let ( - ) a b = Isub (a, b)
  let ( * ) a b = Imul (a, b)
  let ( / ) a b = Idiv (a, b)
  let ( +. ) a b = Fadd (a, b)
  let ( -. ) a b = Fsub (a, b)
  let ( *. ) a b = Fmul (a, b)
  let ( /. ) a b = Fdiv (a, b)
  let ( ** ) a b = Fpow (a, b)
  let sin = fsin
  let cos = fcos
  let tan = ftan
  let sqrt = sqrt
end

let blend =
  let open Infix in
  let a = input 0 x y c +. input 1 x y c /. float 2. in
  a /. float 2.

let invert_f kind =
  let open Infix in
  let max = Type.Kind.max_f kind in
  float max -. input 0 x y c

let test =
  let img = Magick.read "test.jpg" u8 rgb |> Error.unwrap in
  let output = Image.like img in
  let f = Op.eval (op @@ invert_f u8) in
  let start = Unix.gettimeofday () in
  let () = f ~output [| img; |] in
  let stop = Unix.gettimeofday () in
  Printf.printf "BLEND EXPR: %fsec\n" (stop -. start);
  Magick.write "test-expr.jpg" output;
  let start = Unix.gettimeofday () in
  let () = Op.eval Op.invert ~output [| img; img;|] in
  let stop = Unix.gettimeofday () in
  Printf.printf "BLEND DIRECT: %fsec\n" (stop -. start);
  Magick.write "test-expr2.jpg" output


