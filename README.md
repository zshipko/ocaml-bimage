bimage — Image processing library
-------------------------------------------------------------------------------
%%VERSION%%

bimage is an image processing library for OCaml.

## Features

- Simple image type based on bigarrays
- Supports u8, u16, i32, i64, f32, f64 datatypes
- Composable image operations
- Image I/O using OpenImageIO (`bimage-io`)
- Image I/O using ImageMagick/GraphicsMagick and stb_image (`bimage-unix`)
- GLFW window support (`bimage-display`)

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

## Getting started

- `Type.t`: Defines the type of an image: `u8`, `u16`, `f32`, `f64`, i32` or `i64`
- `Color.t`: Defines the color of an image: `gray`, `rgb`, `rgba`, `xyz` and `yuv`
  * It's possible to extend the color type by implementing [COLOR](https://github.com/zshipko/ocaml-bimage/blob/master/src/color.ml)
- `Image.t`: Image type
- `Kernel.t`: Convolution kernels
- `Transform.t`: Image transformations
- `Expr.t`: Expression combinator
  * Building blocks for image processing filters
- `Filter.t`: Executable image filter
  * Makes `Expr.t` executable

There is a corresponding file for each of these types in [src/](https://github.com/zshipko/ocaml-bimage/tree/master/src).

## Examples

See [examples/](https://github.com/zshipko/ocaml-bimage/tree/master/examples) for usage examples

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
