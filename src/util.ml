open Gg

let pi = 4. *. atan 1.
let e = 2.71828

module Angle = struct
  type t = float

  let of_degrees deg = deg *. (pi /. 180.)
  let to_degrees t = 180. *. t /. pi
  external of_radians: float -> t = "%identity"
  external to_radians: t -> float = "%identity"
end

module Point = P2
module Box = Box2
