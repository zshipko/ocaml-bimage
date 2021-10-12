let () =
  let image =
    Bimage_io.read Bimage.f32 Bimage.rgb Sys.argv.(1) |> Result.get_ok
  in
  Bimage_display.show [ ("test", Bimage.Image.any image) ]
