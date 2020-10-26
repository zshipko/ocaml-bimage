type t = Pixel : (float, Type.f32) Data.t -> t [@@unboxed]

let empty n =
  let p = Data.create Type.f32 n in
  Pixel p

let length (Pixel p) = Data.length p

let compare (Pixel a) (Pixel b) = Data.compare a b

let equal (Pixel a) (Pixel b) = Data.equal a b

let from_data data =
  let len = Data.length data in
  let (Pixel px) = empty len in
  let kind = Data.kind data in
  for i = 0 to len - 1 do
    px.{i} <- Type.(to_float kind data.{i} |> normalize kind)
  done;
  Pixel px

let to_data ~dest (Pixel px) =
  let len = Data.length dest in
  let kind = Data.kind dest in
  for i = 0 to min len (Data.length px) - 1 do
    dest.{i} <- Type.(of_float kind (denormalize kind px.{i}))
  done

let data (Pixel px) = px

let rgb_to_xyz (Pixel px) =
  let (Pixel dest) = empty 3 in
  dest.{0} <-
    (0.4124564 *. px.{0}) +. (0.3575761 *. px.{1}) +. (0.1804375 *. px.{2});
  dest.{1} <-
    (0.2126729 *. px.{0}) +. (0.7151522 *. px.{1}) +. (0.0721750 *. px.{2});
  dest.{2} <-
    (0.0193339 *. px.{0}) +. (0.1191920 *. px.{1}) +. (0.9503041 *. px.{2});
  Pixel dest

let rgb_to_yuv (Pixel px) =
  let (Pixel dest) = empty 3 in
  dest.{0} <- (0.299 *. px.{0}) +. (0.587 *. px.{1}) +. (0.114 *. px.{2});
  dest.{1} <- (-0.147 *. px.{0}) -. (0.289 *. px.{1}) +. (0.436 *. px.{2});
  dest.{2} <- (0.615 *. px.{0}) -. (0.515 *. px.{1}) -. (0.100 *. px.{2});
  Pixel dest

let map_inplace f (Pixel px) = Data.map_inplace f px

let map f (Pixel px) =
  let dest = Data.copy px in
  Data.map_inplace f dest;
  Pixel dest

let map2_inplace f (Pixel a) (Pixel b) = Data.map2_inplace f a b

let map2 f (Pixel a) (Pixel b) =
  let dest = Data.copy a in
  Data.map2_inplace f dest b;
  Pixel dest

let convert_in_place from to_ px =
  map
    (fun x ->
      let x = Type.normalize from x in
      Type.denormalize to_ x)
    px

let fold f (Pixel px) a = Data.fold f px a

let fold2 f (Pixel px) (Pixel px') a = Data.fold2 f px px' a

let pp fmt (Pixel px) =
  Format.fprintf fmt "Scalar(%f, %f, %f)" px.{0} px.{1} px.{2}
