(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

include Type
include Color
include Util
include Filter
module Error = Error
module Color = Color
module Data = Data
module Pixel = Pixel
module Kernel = Kernel
module Transform = Transform
module Type = Type
module Image = Image
module Input = Input
module Expr = Expr
module Hash = Hash
module Filter = Filter

exception Unsupported

type ('a, 'b, 'c) filter = ('a, 'b, 'c) Filter.t

type 'c image_u8 = (int, u8, 'c) Image.t

type 'c image_u16 = (int, u16, 'c) Image.t

type 'c image_i32 = (int32, i32, 'c) Image.t

type 'c image_i64 = (int64, i64, 'c) Image.t

type 'c image_f32 = (float, f32, 'c) Image.t

type 'c image_f64 = (float, f64, 'c) Image.t

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
