open Type

type t = Pixel : (float, f32) Data.t -> t [@@unboxed]

let empty n =
  let p = Data.create f32 n in
  Pixel p


let from_data data =
  let len = Data.length data in
  let (Pixel px) = empty len in
  let kind = Data.kind data in
  for i = 0 to len - 1 do
    Bigarray.Array1.set px i
      Kind.(to_float kind (Bigarray.Array1.get data i) |> normalize kind)
  done;
  Pixel px


let to_data ~dest (Pixel px) =
  let len = Data.length dest in
  let kind = Data.kind dest in
  for i = 0 to min len (Data.length px) - 1 do
    Bigarray.Array1.set dest i
      Kind.(of_float kind (denormalize kind (Bigarray.Array1.get px i)))
  done


let data (Pixel px) = px

let to_xyz (Pixel px) =
  let (Pixel dest) = empty 3 in
  Bigarray.Array1.set dest 0
    ( (0.4124564 *. Bigarray.Array1.get px 0)
    +. (0.3575761 *. Bigarray.Array1.get px 1)
    +. (0.1804375 *. Bigarray.Array1.get px 2) );
  Bigarray.Array1.set dest 1
    ( (0.2126729 *. Bigarray.Array1.get px 0)
    +. (0.7151522 *. Bigarray.Array1.get px 1)
    +. (0.0721750 *. Bigarray.Array1.get px 2) );
  Bigarray.Array1.set dest 2
    ( (0.0193339 *. Bigarray.Array1.get px 0)
    +. (0.1191920 *. Bigarray.Array1.get px 1)
    +. (0.9503041 *. Bigarray.Array1.get px 2) );
  Pixel dest


let to_yuv (Pixel px) =
  let (Pixel dest) = empty 3 in
  Bigarray.Array1.set dest 0
    ( (0.299 *. Bigarray.Array1.get px 0)
    +. (0.587 *. Bigarray.Array1.get px 1)
    +. (0.114 *. Bigarray.Array1.get px 2) );
  Bigarray.Array1.set dest 1
    ( (-0.147 *. Bigarray.Array1.get px 0)
    -. (0.289 *. Bigarray.Array1.get px 1)
    +. (0.436 *. Bigarray.Array1.get px 2) );
  Bigarray.Array1.set dest 2
    ( (0.615 *. Bigarray.Array1.get px 0)
    -. (0.515 *. Bigarray.Array1.get px 1)
    -. (0.100 *. Bigarray.Array1.get px 2) );
  Pixel dest


let map_inplace f (Pixel px) = Data.map_inplace f px

let map f (Pixel px) =
  let dest = Data.copy px in
  Data.map_inplace f dest; Pixel dest


let fold f (Pixel px) a = Data.fold f px a

let fold2 f (Pixel px) (Pixel px') a = Data.fold2 f px px' a

let pp fmt (Pixel px) =
  Format.fprintf fmt "Scalar(%f, %f, %f)" (Bigarray.Array1.get px 0)
    (Bigarray.Array1.get px 1) (Bigarray.Array1.get px 2)
