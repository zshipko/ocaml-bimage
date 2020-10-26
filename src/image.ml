open Color

type layout = Planar | Interleaved

type ('a, 'b, 'c) t = {
  width : int;
  height : int;
  color : 'c Color.t;
  layout : layout;
  data : ('a, 'b) Data.t;
}

let create (type color) ?(layout = Interleaved) kind (module C: COLOR with type t = color) width height =
  let channels = C.channels C.t in
  let data = Data.create kind (width * height * channels) in
  { width; height; color = (module C); layout; data }

let compare a b = Data.compare a.data b.data

let equal a b = Data.equal a.data b.data

let of_data (type color) (module C: COLOR with type t = color) width height layout data =
  let channels = C.channels C.t in
  if width * height * channels <> Data.length data then Error.exc `Invalid_shape
  else { width; height; color = (module C); layout; data }

let like image =
  create ~layout:image.layout (Data.kind image.data) image.color image.width
    image.height

let like_with_kind kind image =
  create ~layout:image.layout kind image.color image.width image.height

let like_with_color color image =
  create ~layout:image.layout (Data.kind image.data) color image.width
    image.height

let like_with_layout layout image =
  create ~layout (Data.kind image.data) image.color image.width image.height

let copy image =
  let data = Data.copy image.data in
  of_data image.color image.width image.height image.layout data

let copy_to ~dest src = Data.copy_to ~dest:dest.data src.data

let random (type color) ?(layout = Interleaved) kind (module C: COLOR with type t = color) width height =
  let channels = C.channels C.t in
  let data = Data.random kind (width * height * channels) in
  { width; height; color = (module C); layout; data }

let channels (type c) { color; _ } =
  let (module C: COLOR with type t = c) = color in
  C.channels C.t

let[@inline] kind { data; _ } = Data.kind data

let color { color; _ } = color

let layout { layout; _ } = layout

let shape (type c) { width; height; color; _ } =
  let (module C: COLOR with type t = c) = color in
  (width, height, C.channels C.t)

let[@inline] length t =
  t.width * t.height * channels t

let data { data; _ } = data

let empty_pixel image = Pixel.empty (channels image)

let empty_data image = Data.create (kind image) (channels image)

let convert_to ~dest img =
  let dest_k = kind dest in
  let src_k = kind img in
  for i = 0 to length dest - 1 do
    dest.data.{i} <- Type.convert ~from:src_k dest_k img.data.{i}
  done

let convert k img =
  let dest = create ~layout:img.layout k img.color img.width img.height in
  convert_to ~dest img;
  dest

let of_any_color (type color) im (module C: COLOR with type t = color) : (('a, 'b, color) t, Error.t) result =
  if channels im = C.channels C.t then
    Ok (of_data (module C: COLOR with type t = color) im.width im.height im.layout im.data)
  else Error `Invalid_color

let[@inline] index image x y c =
  match image.layout with
  | Planar -> (image.width * image.height * c) + (y * image.width) + x
  | Interleaved ->
      let channels = channels image in
      (y * image.width * channels)
      + (channels * x)
      + c

let index_at image offs =
  let channels = channels image in
  Data.slice image.data ~offs ~length:channels

let[@inline] get image x y c =
  let index = index image x y c in
  if index < 0 || index >= length image then Type.min (kind image)
  else image.data.{index}

let[@inline] set image x y c v =
  let index = index image x y c in
  image.data.{index} <- v

let get_f image x y c =
  let kind = kind image in
  get image x y c |> Type.to_float kind

let set_f image x y c v =
  let kind = kind image in
  let v = Type.of_float kind v in
  set image x y c v

let get_norm image x y c =
  let kind = kind image in
  get image x y c |> Type.to_float kind |> Type.normalize kind

let set_norm image x y c v =
  let kind = kind image in
  let v = Type.denormalize kind v |> Type.of_float kind in
  set image x y c v

let get_pixel image ?dest x y =
  let c = channels image in
  let (Pixel.Pixel px) =
    match dest with Some px -> px | None -> Pixel.empty c
  in
  for i = 0 to c - 1 do
    px.{i} <- get_norm image x y i
  done;
  Pixel.Pixel px

let set_pixel image x y (Pixel.Pixel px) =
  let c = channels image in
  for i = 0 to c - 1 do
    set_norm image x y i px.{i}
  done

let get_data image ?dest x y =
  let c = channels image in
  let px =
    match dest with Some px -> px | None -> Data.create (kind image) c
  in
  for i = 0 to c - 1 do
    px.{i} <- get image x y i
  done;
  px

let set_data image x y px =
  let c = channels image in
  for i = 0 to c - 1 do
    set image x y i px.{i}
  done

let map_inplace f img = Data.map_inplace f img.data

let map2_inplace f a b = Data.map2_inplace f a.data b.data

let map f img =
  let dest = copy img in
  map_inplace f dest;
  dest

let map2 f img b =
  let dest = copy img in
  map2_inplace f dest b;
  dest

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

let avg ?(x = 0) ?(y = 0) ?width ?height img =
  let width =
    match width with None -> img.width - x | Some w -> min w (img.width - x)
  in
  let height =
    match height with None -> img.height - y | Some h -> min h (img.width - y)
  in
  let avg = Data.create Type.f32 (channels img) in
  let channels = channels img in
  let size = float_of_int (width * height) in
  let kind = kind img in
  for_each
    (fun _x _y px ->
      for i = 0 to channels - 1 do
        avg.{i} <- avg.{i} +. Type.to_float kind px.{i}
      done)
    ~x ~y ~width ~height img;
  for i = 0 to channels - 1 do
    avg.{i} <- avg.{i} /. size
  done;
  avg

let convert_layout layout im =
  let width, height, _ = shape im in
  let dest = create ~layout (kind im) (color im) width height in
  for_each
    (fun x y px ->
      for i = 0 to Data.length px - 1 do
        dest.data.{index dest x y i} <- px.{i}
      done)
    im;
  dest

let crop im ~x ~y ~width ~height =
  let dest = create ~layout:im.layout (kind im) im.color width height in
  for_each
    (fun i j _ ->
      for c = 0 to channels im - 1 do
        set dest i j c (get im (x + i) (y + j) c)
      done)
    dest;
  dest

let mean_std ?(channel = 0) image =
  let kind = kind image in
  let x1 = ref 0. in
  let x2 = ref 0. in
  for_each
    (fun _x _y px ->
      let f = Type.to_float kind px.{channel} in
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
        let v' = get_norm image x y c in
        set_norm image x y c (v' +. v))
      diff

  let length x = Hashtbl.length x
end

let diff a b =
  let dest = Hashtbl.create 8 in
  let kind = kind a in
  for_each
    (fun x y px ->
      let pxb = get_data b x y in
      for i = 0 to channels a do
        let a = Type.to_float kind px.{i} |> Type.normalize kind in
        let b = Type.to_float kind pxb.{i} |> Type.normalize kind in
        if a <> b then Hashtbl.replace dest (x, y, i) (a -. b)
      done)
    a;
  dest
