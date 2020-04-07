type t = Int64.t

module Set = Set.Make (Int64)

let bit_set number n b =
  let open Int64 in
  if b then logor number (shift_left one n)
  else logand number (lognot (shift_left one n))

let bit_check number n =
  Int64.(compare (logand (shift_right number n) one) one) = 0

(* Get average pixel value *)
let pixel_avg px =
  let px = Pixel.data px in
  let len = Data.length px in
  Data.fold ( +. ) px 0.0 /. float_of_int len

let phash im =
  let im = Impl.resize 8 8 im in
  let h = ref Int64.zero in
  let index = ref 0 in
  for j = 0 to 7 do
    for i = 0 to 7 do
      let p = Image.get_pixel im i j |> pixel_avg in
      h := bit_set !h !index (p > 0.5);
      incr index
    done
  done;
  !h

let diff a b =
  let diff = ref 0 in
  for i = 0 to 63 do
    if bit_check a i <> bit_check b i then incr diff
  done;
  !diff

let equal a b = diff a b = 0

let to_string hash = Printf.sprintf "%016Lx" hash

let to_int64 hash = hash
