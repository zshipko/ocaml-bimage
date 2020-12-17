open Util
open Image

let rotate_90 image =
  let output = v (ty image) image.color image.height image.width in
  let center =
    (float_of_int output.width /. 2., float_of_int image.height /. 2.)
  in
  Filter.v (Expr.rotate ~center (Angle.of_degrees 90.) ~input:0)
  |> Filter.run ~output [| Input.input image |];
  output

let rotate_180 image =
  let output = v (ty image) image.color image.width image.height in
  let center =
    (float_of_int image.width /. 2., float_of_int image.height /. 2.)
  in
  Filter.v (Expr.rotate ~center (Angle.of_degrees 180.) ~input:0)
  |> Filter.run ~output [| Input.input image |];
  output

let rotate_270 image =
  let output = v (ty image) image.color image.height image.width in
  let center =
    (float_of_int image.width /. 2., float_of_int output.height /. 2.)
  in
  Filter.v (Expr.rotate ~center (Angle.of_degrees 270.) ~input:0)
  |> Filter.run ~output [| Input.input image |];
  output

let resize width height image =
  let output = v (ty image) image.color width height in
  let x = float_of_int width /. float_of_int image.width in
  let y = float_of_int height /. float_of_int image.height in
  Filter.(v (Expr.scale x y ~input:0) |> run ~output [| Input.input image |]);
  output
