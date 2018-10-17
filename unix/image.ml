let create_mmap ?(layout = Bimage.Image.Interleaved) kind color ~filename w h =
  let data = Data.create_mmap kind ~filename (w * h * Bimage.Color.channels color) in
  Bimage.Image.of_data color w h layout data
