type 'a t = Pixel : ('a Color.t * floatarray) -> 'a t [@@unboxed]

let empty (type c) color =
  let (module C : Color.COLOR with type t = c) = color in
  let p = Float.Array.create (C.channels C.t) in
  Pixel (color, p)

let length (Pixel (_color, p)) = Float.Array.length p

let compare (Pixel a) (Pixel b) = if a < b then -1 else if a > b then 1 else 0

let equal (Pixel (_, a)) (Pixel (_, b)) =
  let result = ref true in
  Float.Array.iter2 (fun a b -> result := !result && Float.equal a b) a b;
  !result

let get (Pixel (_, a)) = Float.Array.get a

let set (Pixel (_, a)) = Float.Array.set a

let to_rgb (type color) (Pixel (color, a)) =
  let (module C : Color.COLOR with type t = color) = color in
  Pixel ((module Color.Rgb), C.to_rgb C.t a)

let from_rgb (type color) color (Pixel (_rgb, a)) =
  let (module C : Color.COLOR with type t = color) = color in
  Pixel (color, C.from_rgb C.t a)

let from_data (type a b) color data =
  let len = Data.length data in
  let px = empty color in
  let (module T : Type.TYPE with type t = a and type elt = b) = Data.ty data in
  for i = 0 to len - 1 do
    set px i T.(to_float data.{i} |> Type.normalize (module T))
  done;
  px

let to_data ~dest px =
  let len = Data.length dest in
  let ty = Data.ty dest in
  for i = 0 to min len (length px) - 1 do
    dest.{i} <- Type.(of_float ty (denormalize ty (get px i)))
  done

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

let data (Pixel (_, px)) = px

let color (Pixel (c, _)) = c

let map_inplace f px = Float.Array.iteri (fun i x -> set px i (f x)) (data px)

let map f (Pixel (color, px)) =
  let dest = Pixel (color, Float.Array.copy px) in
  map_inplace f dest;
  dest

let convert_in_place from to_ px =
  map
    (fun x ->
      let x = Type.normalize from x in
      Type.denormalize to_ x)
    px

let fold f (Pixel (_, px)) a = Float.Array.fold_left (fun a b -> f b a) a px

let pp fmt px =
  let len = length px - 1 in
  Format.fprintf fmt "Pixel(";
  for p = 0 to len do
    let () = Format.fprintf fmt "%f" (get px p) in
    if p < len then Format.fprintf fmt ","
  done;
  Format.fprintf fmt ")"
