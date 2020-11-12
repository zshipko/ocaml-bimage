open Bigarray

type ('a, 'b) t = ('a, 'b, c_layout) Array1.t

let[@inline] ty t = Array1.kind t |> Type.of_kind

let create (type a b) (module T : Type.TYPE with type t = a and type elt = b) n
    =
  let arr = Bigarray.Array1.create T.kind Bigarray.C_layout n in
  Array1.fill arr (T.of_float 0.0);
  arr

let random t n =
  let dest = create t n in
  for i = 0 to n - 1 do
    dest.{i} <- Type.of_float t (Random.float (Type.max_f t))
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

let of_array t arr = Array1.of_array (Type.kind t) C_layout arr

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
  let dest = create (ty data) (length data) in
  copy_to ~dest data;
  dest

let convert_to fn ~dest data =
  let len = length data in
  for i = 0 to len - 1 do
    dest.{i} <- fn data.{i}
  done

let convert ty fn data =
  let len = length data in
  let dst = create ty len in
  for i = 0 to len - 1 do
    dst.{i} <- fn data.{i}
  done;
  dst
