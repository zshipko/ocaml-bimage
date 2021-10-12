let () =
  let image =
    Bimage_unix.Magick.read Bimage.f32 Bimage.rgba Sys.argv.(1) |> Result.get_ok
  in
  Bimage_display.show [ ("test", Bimage.Image.any image) ]
