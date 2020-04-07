open Image

type t = float array array

let create rows cols = Array.make_matrix rows cols 0.0

let from f rows cols =
  let k = create rows cols in
  for j = 0 to rows - 1 do
    for i = 0 to cols - 1 do
      k.(j).(i) <- f i j
    done
  done;
  k

let rows kernel = Array.length kernel

let cols kernel = Array.length kernel.(0)

let get kernel r c = kernel.(r).(c)

let set kernel r c d = kernel.(r).(c) <- d

let sum (kernel : t) =
  Array.map (Array.fold_left ( +. ) 0.0) kernel |> Array.fold_left ( +. ) 0.0

let normalize kernel =
  let sum = sum kernel in
  if sum = 0.0 then kernel else Array.map (Array.map (fun x -> x /. sum)) kernel

external to_array : t -> float array array = "%identity"

let of_array ?(norm = true) arr = if norm then normalize arr else arr

let sobel_x =
  of_array
    [| [| 1.0; 0.0; -1.0 |]; [| 2.0; 0.0; -2.0 |]; [| 1.0; 0.0; -1.0 |] |]

let sobel_y =
  of_array
    [| [| 1.0; 2.0; 1.0 |]; [| 0.0; 0.0; 0.0 |]; [| -1.0; -2.0; -1.0 |] |]

let gaussian ?(std = 1.4) n =
  if n mod 2 = 0 then Error.exc (`Invalid_kernel_shape (n, n));
  (* A = (1 / (2 * PI * STD ^ 2)) *)
  let std2 = std *. std in
  let a = 1.0 /. (2. *. Util.pi *. std2) in
  from
    (fun i j ->
      (* X = (-1 * ((i ^ 2 + j ^ 2) / (2 * STD ^ 2))) *)
      let x = (float_of_int (i * i) +. float_of_int (j * j)) /. (2. *. std2) in
      (* A * e ^ -1 * X *)
      a *. ((Util.e ** -1.) *. x))
    n n
  |> normalize

let op_3x3 ?(input = 0) kernel =
  let k00 = get kernel 0 0 in
  let k10 = get kernel 1 0 in
  let k20 = get kernel 2 0 in
  let k01 = get kernel 0 1 in
  let k11 = get kernel 1 1 in
  let k21 = get kernel 2 1 in
  let k02 = get kernel 0 2 in
  let k12 = get kernel 1 2 in
  let k22 = get kernel 2 2 in
  fun inputs x y c ->
    let a = Input.get inputs input in
    (get_f a (x - 1) (y - 1) c *. k00)
    +. (get_f a (x - 1) y c *. k10)
    +. (get_f a (x - 1) (y + 1) c *. k20)
    +. (get_f a x (y - 1) c *. k01)
    +. (get_f a x y c *. k11)
    +. (get_f a x (y + 1) c *. k21)
    +. (get_f a (x + 1) (y - 1) c *. k02)
    +. (get_f a (x + 1) y c *. k12)
    +. (get_f a (x + 1) (y + 1) c *. k22)

let op ?(input = 0) kernel =
  let rows = rows kernel in
  let cols = cols kernel in
  let r2 = rows / 2 in
  let c2 = cols / 2 in
  if rows = 3 && cols = 3 then op_3x3 kernel
  else fun inputs x y c ->
    let a = Input.get inputs input in
    let f = ref 0.0 in
    for ky = -r2 to r2 do
      let kr = kernel.(ky + r2) in
      for kx = -c2 to c2 do
        f := !f +. (get_f a (x + kx) (y + ky) c *. kr.(kx + c2))
      done
    done;
    !f

let join ?(input = 0) fn kernel kernel2 =
  let rows = rows kernel in
  let cols = cols kernel in
  let r2 = rows / 2 in
  let c2 = cols / 2 in
  fun inputs x y c ->
    let a = Input.get inputs input in
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

let combine ?(input = 0) kernel kernel2 =
  let r2 = rows kernel / 2 in
  let c2 = cols kernel / 2 in
  let r2' = rows kernel2 / 2 in
  let c2' = cols kernel2 / 2 in
  fun inputs x y c ->
    let a = Input.get inputs input in
    let f = ref 0.0 in
    for ky = -r2 to r2 do
      let kr = kernel.(ky + r2) in
      for kx = -c2 to c2 do
        let v = get_f a (x + kx) (y + ky) c in
        f := !f +. (v *. kr.(kx + c2))
      done
    done;
    for ky = -r2' to r2' do
      let kr = kernel2.(ky + r2') in
      for kx = -c2' to c2' do
        let v = get_f a (x + kx) (y + ky) c in
        f := !f +. (v *. kr.(kx + c2'))
      done
    done;
    !f
