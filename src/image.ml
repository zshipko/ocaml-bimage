open Color

type ('a, 'b, 'c) t = {
  width : int;
  height : int;
  color : 'c Color.t;
  ty : ('a, 'b) Type.t;
  data : ('a, 'b) Data.t;
}

type any = Any : ('a, 'b, 'c) t -> any

module type TYPE = sig
  type t

  type repr

  type storage

  val kind : (repr, storage) Bigarray.kind

  val to_float : repr -> float

  val of_float : float -> repr
end

let any image = Any image

let v (type color) ty (module C : COLOR with type t = color) width height =
  let channels = C.channels C.t in
  let data = Data.v ty (width * height * channels) in
  { width; height; ty; color = (module C); data }

let compare a b = Data.compare a.data b.data

let equal a b =
  a.width = b.width && a.height = b.height
  && Color.name a.color = Color.name b.color
  && Data.equal a.data b.data

let of_data (type color) (module C : COLOR with type t = color) width height
    data =
  let channels = C.channels C.t in
  let ty = Data.ty data in
  if width * height * channels <> Data.length data then Error.exc `Invalid_shape
  else { width; height; ty; color = (module C); data }

let like image = v (Data.ty image.data) image.color image.width image.height

let like_with_ty ty image = v ty image.color image.width image.height

let like_with_color color image =
  v (Data.ty image.data) color image.width image.height

let copy image =
  let data = Data.copy image.data in
  of_data image.color image.width image.height data

let copy_to ~dest src = Data.copy_to ~dest:dest.data src.data

let random (type color) ty (module C : COLOR with type t = color) width height =
  let channels = C.channels C.t in
  let data = Data.random ty (width * height * channels) in
  { width; height; ty; color = (module C); data }

let channels (type c) { color; _ } =
  let (module C : COLOR with type t = c) = color in
  C.channels C.t

let[@inline] ty { data; _ } = Data.ty data

let color { color; _ } = color

let shape (type c) { width; height; color; _ } =
  let (module C : COLOR with type t = c) = color in
  (width, height, C.channels C.t)

let[@inline] length t = t.width * t.height * channels t

let data { data; _ } = data

let empty_pixel image = Pixel.empty image.color

let empty_data image = Data.v (ty image) (channels image)

let[@inline] index image x y c =
  let channels = channels image in
  (y * image.width * channels) + (channels * x) + c

let index_at image offs =
  let channels = channels image in
  Data.slice image.data ~offs ~length:channels

let[@inline] get image x y c =
  let index = index image x y c in
  if index < 0 || index >= length image then Type.min (ty image)
  else image.data.{index}

let[@inline] set image x y c v =
  let index = index image x y c in
  image.data.{index} <- v

let get_f image x y c =
  let ty = ty image in
  get image x y c |> Type.to_float ty |> Type.normalize ty

let set_f image x y c v =
  let ty = ty image in
  let v = Type.denormalize ty v |> Type.of_float ty in
  set image x y c v

let get_pixel image ?dest x y =
  let c = channels image in
  let px = match dest with Some px -> px | None -> Pixel.empty image.color in
  let index = index image x y 0 in
  let ty = ty image in
  try
    for i = 0 to c - 1 do
      Pixel.set px i
        (Type.to_float ty image.data.{index + i} |> Type.normalize ty)
    done;
    px
  with _ ->
    Pixel.fill px 0.0;
    px

let set_pixel image x y px =
  let c = channels image in
  let index = index image x y 0 in
  let ty = ty image in
  try
    for i = 0 to c - 1 do
      image.data.{index + i} <-
        Type.denormalize ty (Pixel.get px i) |> Type.of_float ty
    done
  with _ -> ()

let get_data image ?dest x y =
  let index = index image x y 0 in
  let c = channels image in
  let data = Data.slice image.data ~offs:index ~length:c in
  match dest with
  | Some dest ->
      Data.copy_to ~dest data;
      dest
  | None -> data

let set_data image x y px =
  let index = index image x y 0 in
  let c = channels image in
  let data = Data.slice image.data ~offs:index ~length:c in
  Data.copy_to ~dest:px data

let map_inplace f img =
  let _ = Data.map_inplace f img.data in
  img

let map2_inplace f a b =
  let _ = Data.map2_inplace f a.data b.data in
  a

let map f img =
  let dest = copy img in
  map_inplace f dest

let map2 f img b =
  let dest = copy img in
  map2_inplace f dest b

let[@inline] for_each_pixel f ?(x = 0) ?(y = 0) ?width ?height img =
  let width =
    match width with Some w -> min (img.width - x) w | None -> img.width - x
  in
  let height =
    match height with
    | Some h -> min (img.height - y) h
    | None -> img.height - y
  in
  let px = empty_pixel img in
  for j = y to y + height - 1 do
    for i = x to x + width - 1 do
      let px = get_pixel img ~dest:px i j in
      f i j px
    done
  done

let[@inline] for_each f ?(x = 0) ?(y = 0) ?width ?height img =
  let width =
    match width with Some w -> min (img.width - x) w | None -> img.width - x
  in
  let height =
    match height with
    | Some h -> min (img.height - y) h
    | None -> img.height - y
  in
  let px = empty_data img in
  for j = y to y + height - 1 do
    for i = x to x + width - 1 do
      let px = get_data img ~dest:px i j in
      f i j px
    done
  done

let convert_to ~dest img =
  for_each_pixel
    (fun x y px ->
      let rgb = Pixel.to_rgb px in
      let color = Pixel.of_rgb dest.color rgb in
      set_pixel dest x y color)
    img

let convert k (c : 'c Color.t) img =
  let dest = v k c img.width img.height in
  convert_to ~dest img;
  dest

let avg ?(x = 0) ?(y = 0) ?width ?height img =
  let width =
    match width with None -> img.width - x | Some w -> min w (img.width - x)
  in
  let height =
    match height with None -> img.height - y | Some h -> min h (img.width - y)
  in
  let avg = Data.v Type.f64 (channels img) in
  let channels = channels img in
  let size = float_of_int (width * height) in
  let ty = ty img in
  for_each
    (fun _x _y px ->
      for i = 0 to channels - 1 do
        avg.{i} <- avg.{i} +. Type.to_float ty px.{i}
      done)
    ~x ~y ~width ~height img;
  for i = 0 to channels - 1 do
    avg.{i} <- avg.{i} /. size
  done;
  avg

let crop im ~x ~y ~width ~height =
  let dest = v (ty im) im.color width height in
  for_each
    (fun i j _ ->
      for c = 0 to channels im - 1 do
        set dest i j c (get im (x + i) (y + j) c)
      done)
    dest;
  dest

let mean_std ?(channel = 0) image =
  let ty = ty image in
  let x1 = ref 0. in
  let x2 = ref 0. in
  for_each
    (fun _x _y px ->
      let f = Type.to_float ty px.{channel} in
      x1 := !x1 +. f;
      x2 := !x2 +. (f *. f))
    image;
  let len = length image |> float_of_int in
  let mean = !x1 /. len in
  let std = sqrt ((!x2 /. len) -. (mean *. mean)) in
  (mean, std)

let fold f image init = Data.fold f image.data init

let fold2 f a b init = Data.fold2 f a.data b.data init

let fold_data f image init =
  let acc = ref init in
  for_each (fun x y px -> acc := f x y px !acc) image;
  !acc

let fold_data2 f a b init =
  let acc = ref init in
  for_each
    (fun x y px ->
      let px' = get_data b x y in
      acc := f x y px px' !acc)
    a;
  !acc

module Diff = struct
  type diff = (int * int * int, float) Hashtbl.t

  let apply diff image =
    Hashtbl.iter
      (fun (x, y, c) v ->
        let v' = get_f image x y c in
        set_f image x y c (v' +. v))
      diff

  let length x = Hashtbl.length x
end

let diff a b =
  let dest = Hashtbl.create 8 in
  let ty = ty a in
  for_each
    (fun x y px ->
      let pxb = get_data b x y in
      for i = 0 to channels a do
        let a = Type.to_float ty px.{i} |> Type.normalize ty in
        let b = Type.to_float ty pxb.{i} |> Type.normalize ty in
        if a <> b then Hashtbl.replace dest (x, y, i) (a -. b)
      done)
    a;
  dest
