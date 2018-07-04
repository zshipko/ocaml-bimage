open Bigarray
open Type

type ('a, 'b) t = ('a, 'b, c_layout) Array1.t

let kind t = Array1.kind t

let create kind ?mmap n =
  let arr = match mmap with
    | Some name ->
        let fd = Unix.openfile name Unix.[O_RDWR; O_CREAT] 0o0655 in
        let arr = Unix.map_file fd kind Bigarray.C_layout true [| n |] in
        Bigarray.array1_of_genarray arr
    | None ->
        Bigarray.Array1.create kind Bigarray.C_layout n
  in Array1.fill arr (Kind.of_float kind 0.0); arr

let random kind n =
  let dest = create kind n in
  for i = 0 to n -1 do
    dest.{i} <- Kind.of_float kind (Random.float (Kind.max kind))
  done;
  dest

let length data = Array1.dim data

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
let equal a b = a = b

let of_float ?dest t arr =
  let of_float = Kind.of_float t in
  let size = length arr in
  let dest = match dest with None -> create t size | Some d -> d in
  for i = 0 to size - 1 do
    dest.{i} <- of_float arr.{i}
  done;
  dest

let to_float ?dest arr =
  let to_float = Kind.to_float (kind arr) in
  let size = length arr in
  let dest = match dest with None -> create f32 size | Some d -> d in
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

let convert_to kind fn ~dest data =
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
