open Gg
open Util

type t = M3.t

let transform t x y =
  let pt = P2.v x y in
  let pt' = P2.tr t pt in
  (P2.x pt', P2.y pt')


let rotate ?center angle =
  let angle = -.Angle.to_radians angle in
  let pt =
    match center with
    | Some (x, y) ->
        Some (P2.v x y)
    | None ->
        None
  in
  M3.rot2 ?pt angle


let scale x y = M3.scale2 (P2.v (1.0 /. x) (1.0 /. y))
