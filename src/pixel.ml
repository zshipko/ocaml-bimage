open Type

type t = Pixel: (float, f32) Data.t -> t [@@unboxed]

let empty n =
  let p = Data.create f32 n in
  Pixel p

let from_data data =
  let len = Data.length data in
  let Pixel px = empty len in
  let kind = Data.kind data in
  for i = 0 to len - 1 do
    px.{i} <- Kind.(to_float kind data.{i} |> normalize kind)
  done;
  Pixel px

let to_data ~dest (Pixel px) =
  let len = Data.length dest in
  let kind = Data.kind dest in
  for i = 0 to min len (Data.length px) - 1 do
    dest.{i} <- Kind.(of_float kind (denormalize kind px.{i}))
  done

let data (Pixel px) = px

let to_color (Pixel data) =
  let kind = Data.kind data in
  let f x = (Kind.to_float kind x +. Kind.min_f kind) /. (Kind.max_f kind  -. Kind.min_f kind) in
  Gg.Color.v (f data.{0}) (f data.{1}) (f data.{2}) 1.0

let from_color ~dest px =
  let Pixel dest = dest in
  let len = Data.length dest in
  let kind = Data.kind dest in
  if len >= 1 then
    dest.{0} <- Kind.of_float kind (Gg.Color.r px);
  if len >= 2 then
    dest.{1} <- Kind.of_float kind (Gg.Color.g px);
  if len >= 3 then
    dest.{2} <- Kind.of_float kind (Gg.Color.g px);
  if len >= 4 then
    dest.{3} <- Kind.of_float kind (Gg.Color.a px)

let to_xyz (Pixel px) =
  let Pixel dest = empty 3 in
  dest.{0} <- 0.4124564 *. px.{0} +. 0.3575761 *. px.{1} +. 0.1804375 *. px.{2};
  dest.{1} <- 0.2126729 *. px.{0} +. 0.7151522 *. px.{1} +. 0.0721750 *. px.{2};
  dest.{2} <- 0.0193339 *. px.{0} +. 0.1191920 *. px.{1} +. 0.9503041 *. px.{2};
  Pixel dest

let to_yuv (Pixel px) =
  let Pixel dest = empty 3 in
  dest.{0} <-  0.299 *. px.{0} +. 0.587 *. px.{1} +. 0.114 *. px.{2};
  dest.{1} <- -0.147 *. px.{0} -. 0.289 *. px.{1} +. 0.436 *. px.{2};
  dest.{2} <-  0.615 *. px.{0} -. 0.515 *. px.{1} -. 0.100 *. px.{2};
  Pixel dest

let map_inplace f (Pixel px) = Data.map_inplace f px

let map f (Pixel px) =
  let dest = Data.copy px in
  Data.map_inplace f dest;
  Pixel dest

let fold f (Pixel px) a = Data.fold f px a
let fold2 f (Pixel px) (Pixel px') a = Data.fold2 f px px' a

let pp fmt (Pixel px) =
  Format.fprintf fmt "Scalar(%f, %f, %f)" px.{0} px.{1} px.{2}


