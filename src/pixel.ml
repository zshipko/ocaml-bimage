type 'a t = 'a Color.t * floatarray

let empty (type c) color =
  let (module C : Color.COLOR with type t = c) = color in
  let p = Float.Array.make (C.channels C.t) 0.0 in
  (color, p)

let v color values =
  assert (Color.channels color = List.length values);
  (color, Float.Array.of_list values)

let fill (_, px) x = Float.Array.fill px 0 (Float.Array.length px) x

let length (_color, p) = Float.Array.length p

let compare a b = if a < b then -1 else if a > b then 1 else 0

let equal (_, a) (_, b) =
  let result = ref true in
  Float.Array.iter2 (fun a b -> result := !result && Float.equal a b) a b;
  !result

let get (_, a) = Float.Array.get a

let set (_, a) = Float.Array.set a

let to_rgb (type color) (color, a) : Color.rgb t =
  let (module C : Color.COLOR with type t = color) = color in
  ((module Color.Rgb : Color.COLOR with type t = Color.rgb), C.to_rgb C.t a)

let of_rgb (type color) color (_rgb, a) =
  let (module C : Color.COLOR with type t = color) = color in
  (color, C.of_rgb C.t a)

let of_data (type a b) color data =
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

let data (_, px) = px

let color (c, _) = c

let iter f px = Float.Array.iteri f (data px)

let map_inplace ?(ignore_alpha = true) f px =
  let color = color px in
  let alpha =
    if ignore_alpha && Color.has_alpha color then Color.channels color - 1
    else -1
  in
  Float.Array.iteri (fun i x -> if i <> alpha then set px i (f x)) (data px);
  px

let map2_inplace ?(ignore_alpha = true) f px px' =
  let color = color px in
  let alpha =
    if ignore_alpha && Color.has_alpha color then Color.channels color - 1
    else -1
  in
  Float.Array.iteri
    (fun i x -> if i <> alpha then set px i (f x (get px' i)))
    (data px);
  px

let map ?ignore_alpha f (color, px) =
  let dest = (color, Float.Array.copy px) in
  map_inplace ?ignore_alpha f dest

let clamp (x : 'a t) : 'a t =
  map_inplace ~ignore_alpha:false (fun x -> Type.clamp Type.f32 x) x

let fold ?(ignore_alpha = true) f (color, px) a =
  let alpha =
    if ignore_alpha && Color.has_alpha color then Color.channels color - 1
    else -1
  in
  let index = ref 0 in
  Float.Array.fold_left
    (fun a b ->
      let i = !index in
      incr index;
      if i <> alpha then f b a else a)
    a px

let pp fmt px =
  let len = length px - 1 in
  Format.fprintf fmt "Pixel(";
  for p = 0 to len do
    let () = Format.fprintf fmt "%f" (get px p) in
    if p < len then Format.fprintf fmt ","
  done;
  Format.fprintf fmt ")"

module Infix = struct
  let ( + ) a b = map2_inplace ( +. ) a b

  let ( - ) a b = map2_inplace ( -. ) a b

  let ( * ) a b = map2_inplace ( *. ) a b

  let ( / ) a b = map2_inplace ( /. ) a b

  let ( +@ ) a b = map_inplace (fun a -> a +. b) a

  let ( -@ ) a b = map_inplace (fun a -> a -. b) a

  let ( *@ ) a b = map_inplace (fun a -> a *. b) a

  let ( /@ ) a b = map_inplace (fun a -> a /. b) a
end
