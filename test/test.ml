(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bimage

let unwrap msg = function
  | Some x -> x
  | None -> failwith msg

let _ =
  let f = Sys.argv.(1) in
  let im = unwrap "Invalid input file" @@ Magick.read f f32 Rgb in
  let dest = Image.create f32 Gray im.Image.width im.Image.height in
  let () = Op.(eval grayscale dest [| im |]) in
  Magick.write "test.jpg" dest;
  let k = Kernel.of_array [|
    [| -1.0; 0.0; 1.0 |];
    [| -2.0; 0.0; 2.0 |];
    [| -1.0; 0.0; 1.0 |];
  |] in
  let b = Kernel.of_array [|
    [| 3.0; 3.0; 3.0 |];
    [| 3.0; 3.0; 3.0 |];
    [| 3.0; 3.0; 3.0 |];
  |] in
  let f = Op.filter_3x3 k in
  let start = Unix.gettimeofday () in
  let x = Image.filter b dest in
  Printf.printf "DIRECT: %fsec\n" (Unix.gettimeofday () -. start);
  Magick.write "test1.jpg" x;
  let start = Unix.gettimeofday () in
  let () = Op.(eval f x [| dest |]) in
  Printf.printf "OP (filter_3x3): %fsec\n" (Unix.gettimeofday () -. start);
  Magick.write "test2.jpg" x;
  let g = Op.filter_3x3 b in
  let start = Unix.gettimeofday () in
  let () = Op.eval g x [| dest |] in
  Printf.printf "OP (filter): %fsec\n" (Unix.gettimeofday () -. start);
  Magick.write "test3.jpg" x;
  let h = Op.filter k in
  let start = Unix.gettimeofday () in
  let () = Op.eval h x [| dest |] in
  Printf.printf "OP (filter): %fsec\n" (Unix.gettimeofday () -. start);
  Magick.write "test4.jpg" x

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
