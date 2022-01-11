open Bimage_display
open Bimage

let mouse_callback _window x y =
  let x = Float.to_int x in
  let y = Float.to_int y in
  Printf.printf "%d, %d\n%!" x y

let () =
  let image =
    Bimage_io.read f32 rgb Sys.argv.(1) |> Result.get_ok
  in
  let window = Window.create "test" image () in
  let () = Window.on_mouse_move mouse_callback window in
  Bimage_display.show_all [ window ]
