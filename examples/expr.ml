open Bimage
open Expr

let a = Bimage_unix.Magick.read f32 rgb Sys.argv.(1) |> Error.unwrap

(** Create an expression to get the average pixel for each pixel,
    [~@] is used to create [index] parameters *)
let avg =
  func (pixel ~@0 X Y) (fun _x _y _c px -> float (Pixel.fold ( +. ) px 0.0))

let avg_minus_1 = Expr.Infix.(avg -. float 1.0)

let avg_times_3 = Expr.Infix.(avg *. float 3.0)

let () =
  let dest = Image.like a in
  (* Exprs can also be evaluated directly using `Op.eval_expr` *)
  Filter.of_expr avg ~output:dest [| Input.input a |]
