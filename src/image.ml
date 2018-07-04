open Type

type ('a, 'b, 'c) t = {
  width: int;
  height: int;
  color: 'c color;
  data: ('a, 'b) Data.t;
}

let create ?mmap depth color width height =
  let data = Data.create ?mmap depth (width * height * channels_of_color color) in
  {width; height; color; data}

let like depth color image =
  create depth color image.width image.height

let random depth color width height =
  let data = Data.random depth (width * height * channels_of_color color) in
  {width; height; color; data}

let channels {color; _} = channels_of_color color
let kind {data; _} = Data.kind data
let shape {width; height; color; _} = width, height, channels_of_color color
let length {width; height; color; _} = width * height * channels_of_color color

let convert_to ?(scale = 1.0) k ~dest img =
  let src_k = kind img in
  for i = 0 to length dest - 1 do
    dest.data.{i} <- of_float k @@ to_float src_k img.data.{i} *. scale
  done

let convert ?(scale = 1.0) k img =
  let dest = create k img.color img.width img.height in
  convert_to ~scale k ~dest img;
  dest


let index image x y =
  let channels = channels image in
  y * image.width * channels + channels * x

let index_at image offs =
  let length = channels image in
  Data.slice image.data ~offs ~length

let at image x y =
  index_at image (index image x y)

let get image x y c =
  let index = index image x y in
  let kind = kind image in
  to_float kind image.data.{index + c}

let set image x y c v =
  let index = index image x y in
  let kind = kind image in
  image.data.{index + c} <- of_float kind v

let each_pixel f img =
  for j = 0 to img.height - 1 do
    for i = 0 to img.width - 1 do
      let px = at img i j in
      f i j px
    done
  done
[@@inline]
