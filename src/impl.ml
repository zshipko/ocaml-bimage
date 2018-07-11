open Util
open Image

let filter k ?dest image =
  let dest =
    match dest with
    | Some d -> d
    | None -> like (kind image) image.color image
  in
  let f = Op.filter k in
  Op.eval f dest [| image |];
  dest

let rotate_90 image =
  let dest = create (kind image) image.color image.height image.width in
  let center = float_of_int dest.width /. 2., float_of_int image.height /. 2. in
  Op.(eval (rotate ~center (Angle.of_degrees 90.))) dest [| image |];
  dest

let rotate_180 image =
  let dest = create (kind image) image.color image.width image.height in
  let center = float_of_int image.width /. 2., float_of_int image.height /. 2. in
  Op.(eval (rotate ~center (Angle.of_degrees 180.))) dest [| image |];
  dest

let rotate_270 image =
  let dest = create (kind image) image.color image.height image.width in
  let center = float_of_int image.width /. 2., float_of_int dest.height /. 2. in
  Op.(eval (rotate ~center (Angle.of_degrees 270.))) dest [| image |];
  dest
