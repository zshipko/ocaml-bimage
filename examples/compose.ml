open Bimage
open Bimage_unix
open Expr

let _ =
  (* Load an image using ImageMagick *)
  let a =
    match Magick.read f32 rgb "test/test.jpg" with
    | Ok img -> img
    | Error e -> failwith (Error.to_string e)
  in

  (* Create an operation to convert to grayscale and subtract 0.5 *)
  let f = Infix.Pixel.(grayscale () - pixel (Pixel.v gray [ 0.5 ])) in

  (* Create a destination image *)
  let dest = Image.like_with_color gray a in

  (* Run the operation *)
  let () = Filter.v f ~output:dest [| Image.any a |] in

  (* Save the image using ImageMagick *)
  Magick.write "test2.jpg" dest
