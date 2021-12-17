open Bigarray

let mmap (type a b) ?(offset = 0L) ?(mode = 0o0655)
    (module T : Bimage.TYPE with type t = a and type elt = b) ~filename n =
  let make_new () =
    let fd = Unix.openfile filename Unix.[ O_RDWR; O_CREAT ] mode in
    let arr =
      Unix.map_file ~pos:offset fd T.kind Bigarray.C_layout true [| n |]
    in
    let arr = Bigarray.array1_of_genarray arr in
    Array1.fill arr (Bimage.Type.of_float (module T) 0.0);
    Unix.close fd;
    arr
  in
  let arr =
    if Sys.file_exists filename then
      let stat = Unix.stat filename in
      let d = Bimage.Type.depth (module T) / 8 in
      let len = Int64.to_int offset + (d * n) in
      if stat.Unix.st_size >= len then (
        let fd = Unix.openfile filename Unix.[ O_RDWR ] mode in
        let ba =
          Unix.map_file ~pos:offset fd T.kind Bigarray.C_layout true [| n |]
          |> Bigarray.array1_of_genarray
        in
        Unix.close fd;
        ba)
      else make_new ()
    else make_new ()
  in
  arr
