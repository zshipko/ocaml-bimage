open Bimage

type t = Cairo.Surface.t

let cairo_format : [ gray | rgb ] Color.t -> Cairo.Image.format =
 fun color ->
   match Color.name color with
   | "gray" -> A8
   | "rgb" -> Cairo.Image.RGB24
   | _ -> invalid_arg "cairo_format"

let draw f im =
  let width, height, _ = Image.shape im in
  let image =
    Cairo.Image.(
      create_for_data8 im.Image.data
        (cairo_format im.Image.color)
        ~w:width ~h:height)
  in
  f image
