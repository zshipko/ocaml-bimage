type t = Image.any array

type index = int

let or_default = function Some x -> x | None -> 0

let get inputs i =
  if i < Array.length inputs then inputs.(i) else Error.exc (`Invalid_input i)

let shape inputs =
  let (Image.Any a) = get inputs 0 in
  Image.shape a
