bimage — Image processing library
-------------------------------------------------------------------------------
%%VERSION%%

bimage is an image processing library for OCaml.

## Features

- Simple image type based on bigarrays
- Supports u8, u16, i32, i64, f32, f64 datatypes
- Composable image operations
- Image I/O using ImageMagick/GraphicsMagick and stb_image (`bimage-unix`)
- Image I/O using OpenImageIO (`bimage-io`)

bimage is distributed under the ISC license.

Homepage: https://github.com/zshipko/ocaml-bimage

## Installation

bimage can be installed with `opam`:

```
$ opam install bimage
```

bimage-io can be installed by running:

```
$ opam install bimage-io
```

Additionally, `bimage-unix`, which provides `stb-image` and `ImageMagick` bindings, can be installed by running:

```
$ opam install bimage-unix
```

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Examples

An example showing how to create an image and how to use `Image.for_each`:

```ocaml
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
```

The above example does direct image processing, however you may also use `Expr`s which can be used to compose operations.

Turning an `Expr.t` into an `Op.t`:

```ocaml
open Bimage
open Expr

let a = Bimage_unix.Magick.read f32 rgb Sys.argv.(1) |> Error.unwrap

(** Create an expression to get the average pixel for each pixel,
    [~@] is used to create [index] parameters *)
let avg = func (pixel ~@0 X Y) (fun _x _y _c px ->
  float (Pixel.fold ( +. ) px 0.0)
)

let avg_minus_1 = Expr.Infix.(avg -. float 1.0)
let avg_times_3 = Expr.Infix.(avg *. float 3.0)

let () =
  let dest = Image.like a in
  (* Exprs can also be evaluated directly using `Filter.of_expr` *)
  Filter.of_expr avg ~output:dest [| a |]
```

An example composing two `Op`s:

```ocaml
open Bimage
open Bimage_unix

let _ =
(* Load an image using ImageMagick *)
let a = match Magick.read f32 rgb "test/test.jpg" with
  | Ok img -> img
  | Error e -> failwith (Error.to_string e)
in

(* Create an operation to convert to grayscale and subtract 1.0 *)
let f = let open Op in Infix.(grayscale &- scalar 0.5) in

(* Create a destination image *)
let dest = Image.like_with_color gray a in

(* Run the operation *)
let () = Filter.make f ~output:dest [| a |] in

(* Save the image using ImageMagick *)
Magick.write "test2.jpg" dest
```

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
bimage`.

[doc]: https://zshipko.github.io/ocaml-bimage/

## Tests

In the distribution sample programs and tests are located in the
[`test`](test) directory. They can be built and run
with:

    dune runtest
