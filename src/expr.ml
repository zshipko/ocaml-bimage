open Type

type _ t =
  | Kernel: Kernel.t -> float t
  | Input: int * int t * int t * int t -> float t
  | X: int t
  | Y: int t
  | C: int t
  | Int: int ->  int t
  | Float: float -> float t
  | Bool: bool -> bool t
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
  | Fmod: float t * float t -> float t
  | Iadd: int t * int t -> int t
  | Isub: int t * int t -> int t
  | Imul: int t * int t -> int t
  | Idiv: int t * int t -> int t
  | Imod: int t * int t -> int t
  | Gt: 'a t * 'a t -> bool t
  | Eq: 'a t * 'a t -> bool t
  | Lt: 'a t * 'a t -> bool t
  | And: bool t * bool t -> bool t
  | Or: bool t * bool t -> bool t
  | Not: bool t -> bool t
  | If: bool t * 'a t * 'a t -> 'a t

let rec compile: type a. int ref -> int ref -> int ref -> a t -> ('b, 'c, 'd) Input.t -> a = fun x y c expr inputs ->
  match expr with
  | Kernel k ->
      Op.kernel k inputs !x !y !c
  |Input (input, x', y', c') ->
      let x' = compile x y c x' inputs in
      let y' = compile x y c y' inputs in
      let c' = compile c y c c' inputs in
      let f: float = Image.get_f (Input.get inputs input) x' y' c' in
      f
  | X -> !x
  | Y -> !y
  | C -> !c
  | Bool b -> b
  | Int i -> i
  | Float f -> f
  | Float_of_int i ->
      let a = compile x y c i inputs in
      float_of_int a
  | Int_of_float f ->
      let a = compile x y c f inputs in
      int_of_float a
  | Fadd (Kernel a, Kernel b) ->
      Op.join_kernel ( +. ) a b inputs !x !y !c
  | Fadd (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a +. b
  | Fsub (Kernel a, Kernel b) ->
      Op.join_kernel ( -. ) a b inputs !x !y !c
  | Fsub (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a -. b
  | Fmul (Kernel a, Kernel b) ->
      Op.join_kernel ( *. ) a b inputs !x !y !c
  | Fmul (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a *. b
  | Fdiv (Kernel a, Kernel b) ->
      Op.join_kernel ( /. ) a b inputs !x !y !c
  | Fdiv (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a /. b
  | Fpow (Kernel a, Kernel b) ->
      Op.join_kernel ( ** ) a b inputs !x !y !c
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
  | Fmod (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      mod_float a b
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
  | Imod (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a mod b
  | Gt (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a > b
  | Eq (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a = b
  | Lt (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a < b
  | And (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a && b
  | Or (a, b) ->
      let a = compile x y c a inputs in
      let b = compile x y c b inputs in
      a || b
  | Not a ->
      let a = compile x y c a inputs in
      not a
  | If (cond, a, b) ->
      let cond = compile x y c cond inputs in
      if cond then compile x y c a inputs
      else compile x y c b inputs


let f body: ('a, 'b, 'c) Op.t =
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
    Bigarray.Array1.unsafe_set output.data i (of_float (Kind.normalize kind f));

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
let kernel k = Kernel k
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
let and_ a b = And (a, b)
let or_ a b = Or (a, b)
let not_ a = Not a
let if_ cond a b = If (cond, a, b)

let ( + ) a b = Iadd (a, b)
let ( - ) a b = Isub (a, b)
let ( * ) a b = Imul (a, b)
let ( / ) a b = Idiv (a, b)
let ( +. ) a b = Fadd (a, b)
let ( -. ) a b = Fsub (a, b)
let ( *. ) a b = Fmul (a, b)
let ( /. ) a b = Fdiv (a, b)
let ( ** ) a b = Fpow (a, b)
