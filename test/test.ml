(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bimage
open Bimage_unix

exception Assert of string

let check name a b =
  if a = b || a + 1 = b || a - 1 = b then () else raise (Assert name)

let only_generate_images =
  try Unix.getenv "ONLY_GENERATE_IMAGES" = "1" with _ -> false

let image_eq a b =
  if not only_generate_images then (
    let b =
      Stb.read_u8 (Image.color a) ("tests/test-" ^ b ^ ".png") |> Error.unwrap
    in
    let w, h, c = Image.shape a in
    let w', h', c' = Image.shape b in
    check "image: same width" w w';
    check "image: same height" h h';
    check "image: same channels" c c';
    Image.for_each
      (fun x y px ->
        let px' = Image.get_data b x y in
        for i = 0 to c - 1 do
          check
            (Printf.sprintf "image: pixel %dx%d %d=%d" x y px.{i} px'.{i})
            px.{i} px'.{i}
        done)
      a )

let sobel =
  let open Expr in
  let open Infix.Pixel in
  kernel ~@0 Kernel.sobel_x + kernel ~@0 Kernel.sobel_y

let test name f ~input ~output =
  ( name,
    fun () ->
      let start = Unix.gettimeofday () in
      ignore (f ~output input);
      let stop = Unix.gettimeofday () in
      Printf.printf "%s: %fsec\n" name (stop -. start);
      Stb.write ("test-" ^ name ^ ".png") output |> Error.unwrap;
      image_eq output name;
      Gc.full_major ();
      Gc.minor () )

let test_write ~output input = Image.copy_to ~dest:output input

let test_invert ~output input =
  Filter.make Op.invert ~output [| Image.any input |]

let test_blend ~output input =
  Filter.make Op.blend ~output [| Image.any input; Image.any input |]

let test_grayscale ~output input =
  Op.(Filter.make grayscale ~output [| Image.any input |])

let test_blur ~output input =
  let b =
    Kernel.of_array
      [| [| 3.0; 3.0; 3.0 |]; [| 3.0; 3.0; 3.0 |]; [| 3.0; 3.0; 3.0 |] |]
  in
  let h = Expr.kernel_3x3 ~@0 b in
  Filter.of_expr h ~output [| Image.any input |]

let test_sobel ~output input =
  Op.(Filter.make sobel ~output [| Image.any input |])

let test_sobel_x ~output input =
  let k =
    Kernel.of_array
      [| [| 1.0; 0.0; -1.0 |]; [| 2.0; 0.0; -2.0 |]; [| 1.0; 0.0; -1.0 |] |]
  in
  let h = Expr.kernel_3x3 ~@0 k in
  Filter.of_expr h ~output [| Image.any input |]

let test_gausssian_blur ~output input =
  Filter.make (Op.gaussian_blur 3) ~output [| Image.any input |]

let test_rotate_270 ~output input =
  let tmp = Image.rotate_270 input in
  Image.copy_to ~dest:output tmp

let grayscale_invert =
  let open Expr in
  map (fun a -> pixel (Pixel.map_inplace (fun i -> 1.0 -. i) a)) (grayscale ~@0)

let test_blur_grayscale ~output input =
  let b =
    Kernel.of_array
      [| [| 3.0; 3.0; 3.0 |]; [| 3.0; 3.0; 3.0 |]; [| 3.0; 3.0; 3.0 |] |]
  in
  let h = Expr.kernel_3x3 ~@0 b in
  Filter.(join [| of_expr h; of_expr Expr.(grayscale ~@0) |])
    ~output
    [| Image.any input |]

let test_grayscale_invert ~output input =
  Filter.of_expr grayscale_invert ~output [| Image.any input |]

let test_resize ~output input =
  let im = Image.resize 123 456 input in
  Image.copy_to ~dest:output im

let test_crop ~output input =
  let im = Image.crop ~x:240 ~y:120 ~width:200 ~height:400 input in
  Image.copy_to ~dest:output im

let input = Error.unwrap @@ Stb.read_u8 rgb "test.jpg"

let output = Image.like input

let tests =
  [
    test "write" test_write ~input ~output;
    test "grayscale-invert" test_grayscale_invert ~input ~output;
    test "blur" test_blur ~input ~output;
    test "sobel" test_sobel ~input ~output;
    test "sobel_x" test_sobel_x ~input ~output;
    test "gaussian-blur" test_gausssian_blur ~input ~output;
    test "blur-grayscale" test_blur_grayscale ~input ~output;
    test "rotate-270" test_rotate_270 ~input
      ~output:(Image.create u8 rgb input.Image.height input.Image.width);
    test "resize" test_resize ~input ~output:(Image.create u8 rgb 123 456);
    test "invert" test_invert ~input ~output;
    test "blend" test_blend ~input ~output;
    test "grayscale" test_grayscale ~input
      ~output:(Image.like_with_color gray input);
    test "crop" test_crop ~input ~output:(Image.create u8 rgb 200 400);
  ]

let () =
  let passed = ref 0 in
  let total = ref 0 in
  List.iter
    (fun (name, f) ->
      Printf.printf "-----\nRunning: %s\n" name;
      incr total;
      try
        f ();
        incr passed;
        Printf.printf "\tPassed\n%!"
      with exc -> Printexc.to_string exc |> Printf.printf "\tError: %s\n%!")
    tests;
  Printf.printf "\n\n-----\nTotal: %d\n\tPassed: %d\n\tFailed: %d\n%!" !total
    !passed (!total - !passed);
  try Test_magick.run ()
  with ex -> Printf.eprintf "Magick Error: %s" (Error.string_of_exn ex)

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
