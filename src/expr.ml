open Type

type _ t =
  | Filter: Kernel.t -> float t
  | Input: int * int t * int t * int t -> float t
  | X: int t
  | Y: int t
  | C: int t
  | Int: int ->  int t
  | Float: float -> float t
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
  | Filter k ->
      Op.filter k inputs !x !y !c
  |Input (input, x', y', c') ->
      let x' = compile x y c x' inputs in
      let y' = compile x y c y' inputs in
      let c' = compile c y c c' inputs in
      Image.get (Input.get inputs input) x' y' c'
  | X -> !x
  | Y -> !y
  | C -> !c
  | Int i -> i
  | Float f -> f
  | Float_of_int i ->
      let a = compile x y c i inputs in
      float_of_int a
  | Int_of_float f ->
      let a = compile x y c f inputs in
      int_of_float a
  | Fadd (Filter a, Filter b) ->
      Op.join_filter ( +. ) a b inputs !x !y !c
  | Fadd (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a +. b
  | Fsub (Filter a, Filter b) ->
      Op.join_filter ( -. ) a b inputs !x !y !c
  | Fsub (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a -. b
  | Fmul (Filter a, Filter b) ->
      Op.join_filter ( *. ) a b inputs !x !y !c
  | Fmul (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a *. b
  | Fdiv (Filter a, Filter b) ->
      Op.join_filter ( /. ) a b inputs !x !y !c
  | Fdiv (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a /. b
  | Fpow (Filter a, Filter b) ->
      Op.join_filter ( ** ) a b inputs !x !y !c
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

let f body =
  let x = ref 0 in
  let y = ref 0 in
  let c = ref 0 in
  let f = compile x y c body in
  fun inputs x' y' c' ->
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
let filter k = Filter k
let input i x y c = Input (i, x, y, c)
let fadd a b = Fadd (a, b)
let fsub a b = Fsub (a, b)
let fdiv a b = Fdiv (a, b)
let fmul a b = Fmul (a, b)
let iadd a b = Iadd (a, b)
let isub a b = Isub (a, b)
let idiv a b = Idiv (a, b)
let imul a b = Imul (a, b)
let pow a b = Fpow (a, b)
let sqrt a = Fsqrt a
let sin a = Fsin a
let cos a = Fcos a
let tan a = Ftan a
let pi () = float Util.pi

let ( + ) a b = Iadd (a, b)
let ( - ) a b = Isub (a, b)
let ( * ) a b = Imul (a, b)
let ( / ) a b = Idiv (a, b)
let ( +. ) a b = Fadd (a, b)
let ( -. ) a b = Fsub (a, b)
let ( *. ) a b = Fmul (a, b)
let ( /. ) a b = Fdiv (a, b)
let ( ** ) a b = Fpow (a, b)
