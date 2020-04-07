open Bimage_unix
open Bimage

let test_magick_read () =
  let img = Magick.read u16 rgb "test.tiff" |> Error.unwrap in
  let w, h, c = Image.shape img in
  assert (w = 3072);
  assert (h = 2048);
  assert (c = 3);
  Magick.write "test-magick-write.tiff" img;
  ()

let run () = test_magick_read ()
