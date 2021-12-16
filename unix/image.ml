let create_mmap (type color) ?offset ?mode kind
    (module C : Bimage.COLOR with type t = color) ~filename w h =
  let data =
    Data.create_mmap ?offset ?mode kind ~filename (w * h * C.channels C.t)
  in
  Bimage.Image.of_data (module C) w h data
