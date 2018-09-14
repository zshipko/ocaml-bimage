(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bimage

let blend =
  let open Expr in
  (input 0 x y c +. input 1 x y c) /. float 2.

let invert_f kind =
  let open Expr in
  let max = Kind.max_f kind in
  float max -. input 0 x y c

let sobel =
  let open Expr in
  kernel Kernel.sobel_x +. kernel Kernel.sobel_y

let run name f ~input ~output =
  let start = Unix.gettimeofday () in
  ignore (f ~output input);
  let stop = Unix.gettimeofday () in
  Printf.printf "%s: %fsec\n" name (stop -. start);
  Magick.write ("test-" ^ name ^ ".jpg") output

let test_write ~output input =
  Image.copy_to ~dest:output input

let test_invert ~output input =
  Op.(eval invert) ~output [| input |]

let test_expr ~output input =
  let f = Op.eval (Expr.f blend) in
  f ~output [| input; input |]

let test_expr_direct ~output input =
  Op.eval Op.blend ~output [| input; input|]

let test_grayscale ~output input =
  Op.(eval grayscale ~output [| input |])

let test_blur ~output input =
  let b = Kernel.of_array [|
    [| 3.0; 3.0; 3.0 |];
    [| 3.0; 3.0; 3.0 |];
    [| 3.0; 3.0; 3.0 |];
  |] in
  Image.kernel b ~output input

let test_sobel ~output input =
  Op.(eval Op.sobel ~output [| input |])

let test_sobel_x ~output input =
  let k = Kernel.of_array [|
    [| 1.0; 0.0; -1.0 |];
    [| 2.0; 0.0; -2.0 |];
    [| 1.0; 0.0; -1.0 |];
  |] in
  let h = Op.kernel k in
  Op.eval h ~output [| input |]

let test_gausssian_blur ~output input =
  Op.eval (Op.gaussian_blur 3) ~output [| input |]

let test_rotate_270 ~output input =
  let tmp = Image.rotate_270 input in
  Image.copy_to ~dest:output tmp

let test_grayscale_invert ~output input =
  let grayscale_invert = Op.(grayscale $ invert_f) in
  Op.eval grayscale_invert ~output[| input |]

let test_scale ~output input =
  Op.(eval (scale 0.5 1.5)) ~output [| input |]

let input = Error.unwrap @@ Magick.read Sys.argv.(1) u8 rgb
let output = Image.like input

let _ =
  run "write" test_write ~input ~output;
  run "invert" test_invert ~input ~output;
  run "expr" test_expr ~input ~output;
  run "expr-direct" test_expr_direct ~input ~output;
  run "grayscale" test_grayscale ~input ~output:(Image.like_with_color gray input);
  run "blur" test_blur ~input ~output;
  run "sobel" test_sobel ~input ~output;
  run "sobel_x" test_sobel_x ~input ~output;
  run "gaussian-blur" test_gausssian_blur ~input ~output;
  run "rotate-270" test_rotate_270 ~input ~output:(Image.create ~layout:input.Image.layout u8 rgb input.Image.height input.Image.width);
  run "grayscale-invert" test_grayscale_invert ~input ~output;
  run "scale" test_scale ~input ~output

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
