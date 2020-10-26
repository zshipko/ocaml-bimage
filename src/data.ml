open Bigarray

type ('a, 'b) t = ('a, 'b, c_layout) Array1.t

let[@inline] kind t = Array1.kind t

let create kind n =
  let arr = Bigarray.Array1.create kind Bigarray.C_layout n in
  Array1.fill arr (Type.of_float kind 0.0);
  arr

let random kind n =
  let dest = create kind n in
  for i = 0 to n - 1 do
    dest.{i} <- Type.of_float kind (Random.float (Type.max kind))
  done;
  dest

let[@inline] length data = Array1.dim data

let fold2 f a b init =
  let acc = ref init in
  for i = 0 to length a - 1 do
    acc := f a.{i} b.{i} !acc
  done;
  !acc

let fold f a init =
  let acc = ref init in
  for i = 0 to length a - 1 do
    acc := f a.{i} !acc
  done;
  !acc

let hash a = Hashtbl.hash a

let compare a b = compare a b

let equal a b = compare a b = 0

let of_float ?dest t arr =
  let of_float = Type.of_float t in
  let size = length arr in
  let dest = match dest with None -> create t size | Some d -> d in
  for i = 0 to size - 1 do
    dest.{i} <- of_float arr.{i}
  done;
  dest

let to_float ?dest arr =
  let to_float = Type.to_float (kind arr) in
  let size = length arr in
  let dest = match dest with None -> create Type.f32 size | Some d -> d in
  for i = 0 to size - 1 do
    dest.{i} <- to_float arr.{i}
  done;
  dest

let of_array t arr = Array1.of_array t C_layout arr

let to_array data =
  let size = length data in
  let arr = Array.make size data.{0} in
  for i = 0 to size - 1 do
    arr.(i) <- data.{i}
  done;
  arr

let fill = Bigarray.Array1.fill

let slice ~offs ~length d = Bigarray.Array1.sub d offs length

let map_inplace f a =
  for i = 0 to length a - 1 do
    a.{i} <- f a.{i}
  done

let map2_inplace f a b =
  for i = 0 to length a - 1 do
    a.{i} <- f a.{i} b.{i}
  done

let copy_to ~dest src = Bigarray.Array1.blit src dest

let copy data =
  let dest = create (Bigarray.Array1.kind data) (length data) in
  copy_to ~dest data;
  dest

let convert_to fn ~dest data =
  let len = length data in
  for i = 0 to len - 1 do
    dest.{i} <- fn data.{i}
  done

let convert kind fn data =
  let len = length data in
  let dst = create kind len in
  for i = 0 to len - 1 do
    dst.{i} <- fn data.{i}
  done;
  dst
