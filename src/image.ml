open Type

type ('a, 'b, 'c) t = {
  width: int;
  height: int;
  color: 'c Color.t;
  step: int;
  data: ('a, 'b) Data.t;
}

let create ?mmap kind color width height =
  let channels = channels_of_color color in
  let data = Data.create ?mmap kind (width * height * channels) in
  let step = width * channels in
  {width; height; color; step; data}

let of_data color width height data =
  let channels = channels_of_color color in
  let step = width * channels in
  if step * height <> Data.length data then
    Error.exc `Invalid_shape
  else
  {width; height; color; step; data}

let like depth color image =
  create depth color image.width image.height

let random kind color width height =
  let channels = channels_of_color color in
  let data = Data.random kind (width * height * channels) in
  let step = width * channels in
  {width; height; color; step; data}

let channels {color; _} = channels_of_color color
let kind {data; _} = Data.kind data [@@inline]
let shape {width; height; color; _} = width, height, channels_of_color color
let length {width; height; color; _} = width * height * channels_of_color color

let convert_to ?(scale = 1.0) ~dest img =
  let dest_k = kind dest in
  let src_k = kind img in
  for i = 0 to length dest - 1 do
    dest.data.{i} <- Kind.of_float dest_k @@ Kind.to_float src_k img.data.{i} *. scale
  done

let convert ?(scale = 1.0) k img =
  let dest = create k img.color img.width img.height in
  convert_to ~scale ~dest img;
  dest

let index image x y =
  y * image.step + image.color.Color.channels * x
[@@inline]

let index_at image offs =
  let length = image.color.Color.channels in
  Data.slice image.data ~offs ~length

let at image x y =
  index_at image (index image x y)

let get image x y c =
  let index = index image x y + c in
  if index < 0 || index >= length image then 0.0
  else Kind.to_float (kind image) image.data.{index}

let set image x y c v =
  let index = index image x y in
  image.data.{index + c} <- Kind.of_float (kind image) v

let get_pixel image x y =
  at image x y |> Pixel.from_data

let set_pixel image x y px =
  let dest = at image x y in
  Pixel.to_data ~dest px

let each_pixel f ?(x = 0) ?(y = 0) ?width ?height img =
  let width =
    match width with
    | Some w -> min (img.width - x) w
    | None -> img.width - x
  in
  let height =
    match height with
    | Some h -> min (img.height - y) h
    | None -> img.height - y
  in
  for j = y to y + height - 1 do
    for i = x to x + width - 1 do
      let px = at img i j in
      f i j px
    done
  done
[@@inline]

let avg_in_rect img x y width height =
  let avg = Data.create f32 (channels img) in
  let channels = channels img in
  let size = float_of_int (width * height) in
  let kind = kind img in
  each_pixel (fun _x _y px ->
    for i = 0 to channels - 1 do
      avg.{i} <- avg.{i} +. Kind.to_float kind px.{i}
    done
  ) ~x ~y ~width ~height img;
  for i = 0 to channels - 1 do
    avg.{i} <- avg.{i} /. size
  done;
  avg

