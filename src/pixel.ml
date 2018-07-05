open Type

type t = Pixel: (float, f32) Data.t -> t [@@unboxed]

let empty () =
  let p = Data.create f32 3 in
  Pixel p

let from_data data =
  let Pixel px = empty () in
  let len = Data.length data in
  let kind = Data.kind data in
  for i = 0 to 3 do
    px.{i} <- Kind.to_float kind data.{i mod len}
  done;
  Pixel px

let to_data ~dest (Pixel px) =
  let len = Data.length dest in
  let kind = Data.kind dest in
  for i = 0 to min (len - 1) 3 do
    dest.{i} <- Kind.of_float kind px.{i}
  done

let to_xyz (Pixel px) =
  let Pixel dest = empty () in
  dest.{0} <- 0.4124564 *. px.{0} +. 0.3575761 *. px.{1} +. 0.1804375 *. px.{2};
  dest.{1} <- 0.2126729 *. px.{0} +. 0.7151522 *. px.{1} +. 0.0721750 *. px.{2};
  dest.{2} <- 0.0193339 *. px.{0} +. 0.1191920 *. px.{1} +. 0.9503041 *. px.{2};
  Pixel dest

let to_yuv (Pixel px) =
  let Pixel dest = empty () in
  dest.{0} <-  0.299 *. px.{0} +. 0.587 *. px.{1} +. 0.114 *. px.{2};
  dest.{1} <- -0.147 *. px.{0} -. 0.289 *. px.{1} +. 0.436 *. px.{2};
  dest.{2} <-  0.615 *. px.{0} -. 0.515 *. px.{1} -. 0.100 *. px.{2};
  Pixel dest

let pp fmt (Pixel px) =
  Format.fprintf fmt "Scalar(%f, %f, %f)" px.{0} px.{1} px.{2}


