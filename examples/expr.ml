open Bimage
open Expr

let a = Bimage_unix.Magick.read f32 rgb Sys.argv.(1) |> Error.unwrap

(** Create an expression to get the average pixel for each pixel,
    [~@] is used to create [index] parameters *)
let avg =
  func (input ~@0 X Y) (fun _x _y px ->
      let f = Pixel.fold ( +. ) px 0.0 in
      Pixel.fill px (f /. Float.of_int (Pixel.length px));
      Pixel px)

let avg_minus_1 = Expr.Infix.Pixel.(avg -@ float 1.0)

let avg_times_3 = Expr.Infix.Pixel.(avg *@ float 3.0)

let () =
  let dest = Image.like a in
  (* Exprs can also be evaluated directly using `Op.eval_expr` *)
  Filter.(of_expr avg |> run) ~output:dest [| Image.any a |]
