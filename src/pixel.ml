type 'a t = Pixel : ('a Color.t * (float, Type.f64) Data.t) -> 'a t [@@unboxed]

let empty (type c) color =
  let (module C: Color.COLOR with type t = c) = color in
  let p = Data.create Type.f64 (C.channels C.t) in
  Pixel (color, p)

let length (Pixel (_color, p)) = Data.length p

let compare (Pixel a) (Pixel b) = Data.compare a b

let equal (Pixel a) (Pixel b) = Data.equal a b

let to_rgb (type color) (Pixel (color, a)) =
  let (module C: Color.COLOR with type t = color) = color in
  Pixel ((module Color.Rgb), C.to_rgb C.t a)

let from_rgb (type color) color (Pixel (_rgb, a)) =
  let (module C: Color.COLOR with type t = color) = color in
  Pixel (color, C.from_rgb C.t a)

let from_data color data =
  let len = Data.length data in
  let (Pixel (_, px)) = empty color in
  let ty = Data.ty data in
  for i = 0 to len - 1 do
    px.{i} <- Type.(to_float ty data.{i} |> normalize ty)
  done;
  Pixel (color, px)

let to_data ~dest (Pixel (_, px)) =
  let len = Data.length dest in
  let ty = Data.ty dest in
  for i = 0 to min len (Data.length px) - 1 do
    dest.{i} <- Type.(of_float ty (denormalize ty px.{i}))
  done

let data (Pixel (_, px)) = px
let color (Pixel (color, _)) = color

(*let rgb_to_xyz (Pixel px) =
  let (Pixel (_, dest)) = empty 3 in
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
  Pixel dest*)

let map_inplace f (Pixel (_, px)) = Data.map_inplace f px

let map f (Pixel (color, px)) =
  let dest = Data.copy px in
  Data.map_inplace f dest;
  Pixel (color, dest)

let map2_inplace f (Pixel (_, a)) (Pixel (_, b)) = Data.map2_inplace f a b

let map2 f (Pixel (color, a)) (Pixel (_, b)) =
  let dest = Data.copy a in
  Data.map2_inplace f dest b;
  Pixel (color, dest)

let convert_in_place from to_ px =
  map
    (fun x ->
      let x = Type.normalize from x in
      Type.denormalize to_ x)
    px

let fold f (Pixel (_, px)) a = Data.fold f px a

let fold2 f (Pixel (_, px)) (Pixel (_, px')) a = Data.fold2 f px px' a

let pp fmt (Pixel (_, px)) =
  let len = Data.length px - 1 in
  Format.fprintf fmt "Pixel(";
  for p = 0 to len do
    let () = Format.fprintf fmt "%f" px.{p} in
    if p < len then
      Format.fprintf fmt ","
  done;
  Format.fprintf fmt ")";
