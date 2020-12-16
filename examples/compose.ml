open Bimage
open Bimage_unix

let _ =
  (* Load an image using ImageMagick *)
  let a =
    match Magick.read f32 rgb "test/test.jpg" with
    | Ok img -> img
    | Error e -> failwith (Error.to_string e)
  in

  (* Create an operation to convert to grayscale and subtract 1.0 *)
  let f =
    let open Op in
    Infix.(grayscale &- pixel (Pixel.make rgb [ 0.5; 0.5; 0.5 ]))
  in

  (* Create a destination image *)
  let dest = Image.like_with_color gray a in

  (* Run the operation *)
  let () = Filter.make f ~output:dest [| Image.any a |] in

  (* Save the image using ImageMagick *)
  Magick.write "test2.jpg" dest
