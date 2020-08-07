open Bimage

let _ =
(* Create a new image *)
let a = Image.create u8 gray 64 64 in

(* Iterate over each pixel *)
let _ =
    Image.for_each (fun x y _px ->
      Image.set a x y 0 (x + y)
    ) a
in

(* Save the image using ImageMagick *)
Bimage_unix.Magick.write "test1.jpg" a
