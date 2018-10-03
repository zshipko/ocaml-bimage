(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bimage

exception Assert of string
let check name a b = if a = b then () else raise (Assert name)

let only_generate_images = try Unix.getenv "ONLY_GENERATE_IMAGES" = "1" with _ -> false

let image_eq a b =
  if not only_generate_images then
    let b = Magick.read ~format:"png" ("tests/test-" ^ b ^ ".png") u8 (Image.color a) |> Error.unwrap in
    let w, h, c = Image.shape a in
    let w', h', c' = Image.shape b in
    check "image: same width" w w';
    check "image: same height" h h';
    check "image: same channels" c c';
    Image.for_each (fun x y px ->
      for i = 0 to c - 1 do
        check (Printf.sprintf "image: pixel %dx%d" x y) px.{i} (Image.get b x y i)
      done
    ) a

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

let test name f ~input ~output =
  name,
  fun () ->
    let start = Unix.gettimeofday () in
    ignore (f ~output input);
    let stop = Unix.gettimeofday () in
    Printf.printf "%s: %fsec\n" name (stop -. start);
    image_eq output name;
    Magick.write ("test-" ^ name ^ ".png") output

let test_write ~output input =
  Image.copy_to ~dest:output input

let test_invert ~output input =
  Op.(eval invert) ~output [| input |]

let test_blend_expr ~output input =
  let f = Op.eval (Expr.f blend) in
  f ~output [| input; input |]

let test_blend ~output input =
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

let test_resize ~output input =
  let im = Image.resize 123 456 input in
  Image.copy_to ~dest:output im

let input = Error.unwrap @@ Magick.read "test.jpg" u8 rgb
let output = Image.like input

let tests = [
  test "write" test_write ~input ~output;
  test "blend-expr" test_blend_expr ~input ~output;
  test "grayscale-invert" test_grayscale_invert ~input ~output;
  test "blur" test_blur ~input ~output;
  test "sobel" test_sobel ~input ~output;
  test "sobel_x" test_sobel_x ~input ~output;
  test "gaussian-blur" test_gausssian_blur ~input ~output;
  test "rotate-270" test_rotate_270 ~input ~output:(Image.create ~layout:input.Image.layout u8 rgb input.Image.height input.Image.width);
  test "resize" test_resize ~input ~output:(Image.create ~layout:input.Image.layout u8 rgb 123 456);
  test "invert" test_invert ~input ~output;
  test "blend" test_blend ~input ~output;
  test "grayscale" test_grayscale ~input ~output:(Image.like_with_color gray input);
]

let () =
  let passed = ref 0 in
  let total = ref 0 in
  List.iter (fun (name, f) ->
    Printf.printf "-----\nRunning: %s\n" name;
    incr total;
    try
      f ();
      incr passed;
      Printf.printf "\tPassed\n%!"
    with exc ->
      Printexc.to_string exc |> Printf.printf "\tError: %s\n%!"
  ) tests;
  Printf.printf "\n\n-----\nTotal: %d\n\tPassed: %d\n\tFailed: %d\n%!" !total !passed (!total - !passed)

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
