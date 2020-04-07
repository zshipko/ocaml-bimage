open Image

type ('a, 'b, 'c) t = ('a, 'b, 'c) Image.t array

type index = int

let index i = i

let get inputs i =
  if i < Array.length inputs then inputs.(i) else Error.exc (`Invalid_input i)

let make_output ?width ?height inputs =
  let a = get inputs 0 in
  let width = match width with None -> a.width | Some w -> w in
  let height = match height with None -> a.height | Some h -> h in
  create (kind a) a.color width height
