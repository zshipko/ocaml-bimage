type 'a t = {
  t: 'a;
  channels: int;
  has_alpha: bool;
}

let create  ~has_alpha ~channels t = {t; channels; has_alpha}
let has_alpha {has_alpha;_} = has_alpha
let channels {channels;_} = channels
let t {t;_} = t

type gray = [`Gray]
type rgb = [`Rgb]
type yuv = [`Yuv]
type xyz = [`Xyz]
type rgba = [`Rgba]
type any = [`Any]

let gray = create ~has_alpha:false ~channels:1 `Gray
let rgb = create ~has_alpha:false ~channels:3 `Rgb
let yuv = create ~has_alpha:false ~channels:3 `Yuv
let xyz = create ~has_alpha:false ~channels:3 `Xyz
let rgba = create ~has_alpha:false ~channels:4 `Rgba
let color n = create ~has_alpha:(n = 4) ~channels:n `Any

let channels_of_color: type a. a t -> int =
  fun {channels; _} ->
    channels
[@@inline]

