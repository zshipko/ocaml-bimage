type input = Input : ('a, 'b, 'c) Image.t -> input

type t = input array

type index = int

let index i = i

let input x = Input x

let int_of_index i = i

let get inputs i =
  if i < Array.length inputs then inputs.(i) else Error.exc (`Invalid_input i)

let shape inputs =
  let (Input a) = get inputs 0 in
  Image.shape a
