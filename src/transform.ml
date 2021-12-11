open Util

type t = {
  e00 : float;
  e10 : float;
  e20 : float;
  (* col 0 *)
  e01 : float;
  e11 : float;
  e21 : float;
  (* col 1 *)
  e02 : float;
  e12 : float;
  e22 : float; (* col 2 *)
}

let v e00 e01 e02 e10 e11 e12 e20 e21 e22 =
  { e00; e10; e20; e01; e11; e21; e02; e12; e22 }

let id = v 1. 0. 0. 0. 1. 0. 0. 0. 1.

(* Functions *)

let neg a =
  v (-.a.e00) (-.a.e01) (-.a.e02) (-.a.e10) (-.a.e11) (-.a.e12) (-.a.e20)
    (-.a.e21) (-.a.e22)

let add a b =
  v (a.e00 +. b.e00) (a.e01 +. b.e01) (a.e02 +. b.e02) (a.e10 +. b.e10)
    (a.e11 +. b.e11) (a.e12 +. b.e12) (a.e20 +. b.e20) (a.e21 +. b.e21)
    (a.e22 +. b.e22)

let sub a b =
  v (a.e00 -. b.e00) (a.e01 -. b.e01) (a.e02 -. b.e02) (a.e10 -. b.e10)
    (a.e11 -. b.e11) (a.e12 -. b.e12) (a.e20 -. b.e20) (a.e21 -. b.e21)
    (a.e22 -. b.e22)

let mmul a b =
  if a == id then b
  else if b == id then a
  else
    v
      ((a.e00 *. b.e00) +. (a.e01 *. b.e10) +. (a.e02 *. b.e20))
      ((a.e00 *. b.e01) +. (a.e01 *. b.e11) +. (a.e02 *. b.e21))
      ((a.e00 *. b.e02) +. (a.e01 *. b.e12) +. (a.e02 *. b.e22))
      ((a.e10 *. b.e00) +. (a.e11 *. b.e10) +. (a.e12 *. b.e20))
      ((a.e10 *. b.e01) +. (a.e11 *. b.e11) +. (a.e12 *. b.e21))
      ((a.e10 *. b.e02) +. (a.e11 *. b.e12) +. (a.e12 *. b.e22))
      ((a.e20 *. b.e00) +. (a.e21 *. b.e10) +. (a.e22 *. b.e20))
      ((a.e20 *. b.e01) +. (a.e21 *. b.e11) +. (a.e22 *. b.e21))
      ((a.e20 *. b.e02) +. (a.e21 *. b.e12) +. (a.e22 *. b.e22))

let mul a b =
  v (a.e00 *. b.e00) (a.e01 *. b.e01) (a.e02 *. b.e02) (a.e10 *. b.e10)
    (a.e11 *. b.e11) (a.e12 *. b.e12) (a.e20 *. b.e20) (a.e21 *. b.e21)
    (a.e22 *. b.e22)

let div a b =
  v (a.e00 /. b.e00) (a.e01 /. b.e01) (a.e02 /. b.e02) (a.e10 /. b.e10)
    (a.e11 /. b.e11) (a.e12 /. b.e12) (a.e20 /. b.e20) (a.e21 /. b.e21)
    (a.e22 /. b.e22)

let smul a s =
  v (s *. a.e00) (s *. a.e01) (s *. a.e02) (s *. a.e10) (s *. a.e11)
    (s *. a.e12) (s *. a.e20) (s *. a.e21) (s *. a.e22)

let transpose a = v a.e00 a.e10 a.e20 a.e01 a.e11 a.e21 a.e02 a.e12 a.e22

let det a =
  let m00 = (a.e11 *. a.e22) -. (a.e21 *. a.e12) in
  (* minor. *)
  let m10 = (a.e01 *. a.e22) -. (a.e21 *. a.e02) in
  let m20 = (a.e01 *. a.e12) -. (a.e11 *. a.e02) in
  (a.e00 *. m00) -. (a.e10 *. m10) +. (a.e20 *. m20)

let inv a =
  let m00 = (a.e11 *. a.e22) -. (a.e21 *. a.e12) in
  (* minor. *)
  let m10 = (a.e01 *. a.e22) -. (a.e21 *. a.e02) in
  let m20 = (a.e01 *. a.e12) -. (a.e11 *. a.e02) in
  let m01 = (a.e10 *. a.e22) -. (a.e20 *. a.e12) in
  let m11 = (a.e00 *. a.e22) -. (a.e20 *. a.e02) in
  let m21 = (a.e00 *. a.e12) -. (a.e10 *. a.e02) in
  let m02 = (a.e10 *. a.e21) -. (a.e20 *. a.e11) in
  let m12 = (a.e00 *. a.e21) -. (a.e20 *. a.e01) in
  let m22 = (a.e00 *. a.e11) -. (a.e10 *. a.e01) in
  let det = (a.e00 *. m00) -. (a.e10 *. m10) +. (a.e20 *. m20) in
  v (m00 /. det) (-.m10 /. det) (m20 /. det) (-.m01 /. det) (m11 /. det)
    (-.m21 /. det) (m02 /. det) (-.m12 /. det) (m22 /. det)

(* 2D space transforms *)

let move2 d = v 1. 0. (fst d) 0. 1. (snd d) 0. 0. 1.

let rot2 ?pt theta =
  let c = cos theta in
  let s = sin theta in
  match pt with
  | None -> v c (-.s) 0. s c 0. 0. 0. 1.
  | Some pt ->
      let px = fst pt in
      let py = snd pt in
      v c (-.s)
        ((-.c *. px) +. (s *. py) +. px)
        s c
        ((-.s *. px) -. (c *. py) +. py)
        0. 0. 1.

let scale2 s = v (fst s) 0. 0. 0. (snd s) 0. 0. 0. 1.

let transform m p =
  ( (m.e00 *. fst p) +. (m.e01 *. snd p) +. m.e02,
    (m.e10 *. fst p) +. (m.e11 *. snd p) +. m.e12 )

let translate x y = move2 (x, y)

let rotate ?center angle =
  let angle = -.Angle.to_radians angle in
  rot2 ?pt:center angle

let scale x y = scale2 (1.0 /. x, 1.0 /. y)

module Infix = struct
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
end
