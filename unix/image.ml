let create_mmap (type color) ?mode ?(layout = Bimage.Image.Interleaved) kind (module C: Bimage.COLOR with type t = color) ~filename
    w h =
  let data =
    Data.create_mmap ?mode kind ~filename (w * h * C.channels C.t)
  in
  Bimage.Image.of_data (module C) w h layout data
