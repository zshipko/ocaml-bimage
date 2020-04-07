open Bigarray

let create_mmap ?(mode = 0o0655) kind ~filename n =
  let arr =
    let fd = Unix.openfile filename Unix.[ O_RDWR; O_CREAT ] mode in
    let arr = Unix.map_file fd kind Bigarray.C_layout true [| n |] in
    Bigarray.array1_of_genarray arr
  in
  Array1.fill arr (Bimage.Kind.of_float kind 0.0);
  arr
