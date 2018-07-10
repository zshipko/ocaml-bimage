(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

include Type
module Angle = Util.Angle
module Error = Error
module Color = Color
module Data = Data
module Pixel = Pixel
module Kernel = Kernel
module Transform = Transform
module Image = struct
  include Image

  let filter k ?dest image =
    let dest =
      match dest with
      | Some d -> d
      | None -> like (kind image) image.color image
    in
    let f = Op.filter k in
    Op.eval f dest [| image |];
    dest

  type rotate = [`Deg90 | `Deg180 | `Deg270]

  let rotate angle ?center ?dest image =
    let dest =
      match dest with
      | Some d -> d
      | None -> like (kind image) image.color image
    in
    let center = match center with
      | Some (x, y) -> x, y
      | None -> float_of_int image.width /. 2., float_of_int image.height /. 2.
    in
    Op.(eval (rotate ~center angle) dest [| image |]);
    dest

  let rotate_90 image =
    let dest = create (kind image) image.color image.height image.width in
    let center = float_of_int dest.width /. 2., float_of_int image.height /. 2. in
    Op.(eval (rotate ~center (Angle.of_degrees 90.))) dest [| image |];
    dest

  let rotate_180 image =
    let dest = create (kind image) image.color image.width image.height in
    let center = float_of_int image.width /. 2., float_of_int image.height /. 2. in
    Op.(eval (rotate ~center (Angle.of_degrees 180.))) dest [| image |];
    dest

  let rotate_270 image =
    let dest = create (kind image) image.color image.height image.width in
    let center = float_of_int image.width /. 2., float_of_int dest.height /. 2. in
    Op.(eval (rotate ~center (Angle.of_degrees 270.))) dest [| image |];
    dest
end
module Op = Op

module Magick = Io.Magick

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
