bimage — Basic image processing library
-------------------------------------------------------------------------------
%%VERSION%%

bimage is an image processing library for OCaml.

## Features

- Simple image type based on bigarrays
- Supports u8, u16, i32, i64, f32, f64, complex32 and complex64 datatypes
- Composable image operations
- Image I/O using ImageMagick or GraphicsMagick
- Optional GTK support using `bimage-gtk`

bimage is distributed under the ISC license.

Homepage: https://github.com/zshipko/bimage

## Installation

bimage can be installed with `opam`:

    opam pin add git+https://github.com/zshipko/ocaml-bimage

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Example

An example showing how to create an image and how to use `Image.each_pixel`

```ocaml
open Bimage

let _ =
    (** Create a new image *)
    let a = Image.create u8 gray 64 64 in

    (** Iterate over each pixel *)
    let _ =
        Image.each_pixel (fun x y px ->
            px.{0} <- x + y
        ) a
    in

    (** Save the image using ImageMagick *)
    Magick.write "test1.jpg" a
```

An example using `Op.t` to run a filter on an image:

```ocaml
open Bimage

let _ =
    (** Load an image using ImageMagick *)
    let Some a = Magick.read "test/test.jpg" f32 rgb in

    (** Create operation to convert to grayscale and subtract 0.1 *)
    let f = Op.(grayscale &- scalar 0.1) in

    (** Create a destination image *)
    let dest = Image.like f32 gray a in

    (** Run the operation *)
    let () = Op.eval f dest [| a |] in

    (** Save the image using ImageMagick *)
    Magick.write "test2.jpg" a
```

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
bimage`.

[doc]: https://github.com/zshipko/bimage/doc

## Tests

In the distribution sample programs and tests are located in the
[`test`](test) directory. They can be built and run
with:

    jbuilder runtest
