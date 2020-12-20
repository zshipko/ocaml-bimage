open Bimage
open Expr

let a = Bimage_unix.Magick.read f32 rgb Sys.argv.(1) |> Error.unwrap

(** Create an expression to get the average pixel for each pixel,
    [~@] can be used to mark an [index] parameters *)
let avg =
  func (input X Y) (fun _x _y px ->
      let f = Pixel.fold ( +. ) px 0.0 in
      Pixel.fill px (f /. Float.of_int (Pixel.length px));
      Pixel px)

let avg_minus_1 = Expr.Infix.Pixel.(avg -@ float 1.0)

let avg_times_3 = Expr.Infix.Pixel.(avg *@ float 3.0)

let () =
  let dest = Image.like a in
  ignore (Filter.v avg |> Filter.run ~output:dest [| Image.any a |])
