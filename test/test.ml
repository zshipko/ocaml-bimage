(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bimage

let unwrap = function
  | Ok x -> x
  | Error e -> Error.exc e

let _ =
  let f = Sys.argv.(1) in
  let im = unwrap  @@ Magick.read f f32 rgb in
  let dest = Image.create f32 gray im.Image.width im.Image.height in
  let () = Op.(eval grayscale dest [| im |]) in
  Magick.write "test0.jpg" dest;
  let k = Kernel.of_array [|
    [| 1.0; 0.0; -1.0 |];
    [| 2.0; 0.0; -2.0 |];
    [| 1.0; 0.0; -1.0 |];
  |] in
  let b = Kernel.of_array [|
    [| 3.0; 3.0; 3.0 |];
    [| 3.0; 3.0; 3.0 |];
    [| 3.0; 3.0; 3.0 |];
  |] in
  let f = Op.sobel in
  let start = Unix.gettimeofday () in
  let x = Image.filter b dest in
  Printf.printf "blur: %fsec\n" (Unix.gettimeofday () -. start);
  Magick.write "test1.jpg" x;
  let start = Unix.gettimeofday () in
  let () = Op.(eval f x [| dest |]) in
  Printf.printf "sobel: %fsec\n" (Unix.gettimeofday () -. start);
  Magick.write "test2.jpg" x;
  let h = Op.filter k in
  let start = Unix.gettimeofday () in
  let () = Op.eval h x [| dest |] in
  Printf.printf "sobel x: %fsec\n" (Unix.gettimeofday () -. start);
  Magick.write "test3.jpg" x;
  let g = Op.filter (Kernel.gaussian 5)  in
  let start = Unix.gettimeofday () in
  let () = Op.eval g x [| dest |] in
  Printf.printf "gaussian: %fsec\n" (Unix.gettimeofday () -. start);
  Magick.write "test4.jpg" x;
  let dest2 = Image.rotate_270 dest in
  Magick.write "test5.jpg" dest2;
  let grayscale_invert = Op.(grayscale $ invert_f (Image.kind im)) in
  let dest = Image.like (Image.kind im) im.Image.color im in
  let start = Unix.gettimeofday () in
  let () = Op.eval grayscale_invert dest [| im |] in
  Printf.printf "grayscale invert: %fsec\n" (Unix.gettimeofday () -. start);
  Magick.write "test6.jpg" dest

(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
