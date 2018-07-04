type t = float array array

type edge =
  | Extend
  | Wrap
  | Mirror
  | Crop

let create rows cols =
  Array.make_matrix rows cols 0.0

let rows kernel = Array.length kernel
let cols kernel = Array.length kernel.(0)
let get kernel r c = kernel.(r).(c)
let set kernel r c d = kernel.(r).(c) <- d
external to_array: t -> float array array = "%identity"
external of_array: float array array -> t = "%identity"

let sum (kernel: t) =
  Array.map (Array.fold_left ( +. ) 0.0) kernel
  |> Array.fold_left ( +. ) 0.0

let normalize kernel =
  let sum = sum kernel in
  if sum = 1.0 then kernel
  else Array.map (Array.map (fun x -> x /. sum)) kernel

