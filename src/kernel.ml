type t = float array array

let v ~rows ~cols = Array.make_matrix rows cols 0.0

let from f rows cols =
  let k = v ~rows ~cols in
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

let combine op a b =
  let rows = rows a in
  let cols = cols a in
  let dest = v ~rows ~cols in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      dest.(r).(c) <- op a.(r).(c) b.(r).(c)
    done
  done;
  dest

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
