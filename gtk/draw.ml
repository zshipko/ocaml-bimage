open Bimage

type t = Cairo.Surface.t

let cairo_format: type a. a color -> Cairo.Image.format = fun color ->
  match color with
    | Gray -> A8
    | Rgb -> Cairo.Image.RGB24
    | _ -> assert false

let draw f im =
  let width, height, _ = Image.shape im in
  let image = Cairo.Image.(create_for_data8 im.Image.data (cairo_format im.Image.color) width height) in
  f image



