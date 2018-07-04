type t = float array array

type edge =
  | Extend
  | Wrap
  | Mirror
  | Crop

let create rows cols =
  Array.make_matrix rows cols 0.0

let get kernel r c = kernel.(r).(c)
let set kernel r c d = kernel.(r).(c) <- d

let sum (kernel: t) =
  Array.map (Array.fold_left ( +. ) 0.0) kernel
  |> Array.fold_left ( +. ) 0.0

let normalize kernel =
  let sum = sum kernel in
  Array.map (Array.map (fun x -> x /. sum)) kernel

