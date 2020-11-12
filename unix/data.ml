open Bigarray

let create_mmap (type a b) ?(mode = 0o0655)
    (module T : Bimage.TYPE with type t = a and type elt = b) ~filename n =
  let arr =
    let fd = Unix.openfile filename Unix.[ O_RDWR; O_CREAT ] mode in
    let arr = Unix.map_file fd T.kind Bigarray.C_layout true [| n |] in
    Bigarray.array1_of_genarray arr
  in
  Array1.fill arr (Bimage.Type.of_float (module T) 0.0);
  arr
