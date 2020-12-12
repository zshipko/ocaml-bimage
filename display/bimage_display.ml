open Bimage

module Texture = struct
  type t = {
    has_alpha : bool;
    id : int;
    internal : int;
    kind : int;
    color : int;
    framebuffer : int;
    width : int;
    height : int;
  }

  external create :
    int -> int -> bool -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t -> t
    = "bimage_create_texture"

  external draw :
    t -> int -> int -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t -> unit
    = "bimage_draw_texture"

  let create window image =
    let () = GLFW.makeContextCurrent ~window:(Some window) in
    create image.Image.width image.Image.height
      (Color.has_alpha image.Image.color)
      image.Image.data

  let draw t window image =
    let () = GLFW.makeContextCurrent ~window:(Some window) in
    let w, h = GLFW.getWindowSize ~window in
    let () = draw t w h image.Image.data in
    GLFW.swapBuffers ~window
end

module Window = struct
  type t = { texture : Texture.t; window : GLFW.window; image: Image.any }

  let init () =
    GLFW.init ();
    GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:3;
    GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:0

  let rec create ?width ?height title image =
    init ();
    let width = match width with
      | Some x -> x
      | None -> image.Image.width
    in
    let height = match height with
      | Some x -> x
      | None -> image.Image.height
    in
    let window = GLFW.createWindow ~width ~height ~title () in
    let texture = Texture.create window image in
    let t = { window; texture; image = Image.Any image } in
    update t;
    t
  and update window =
    let Image.Any image = window.image in
    Texture.draw window.texture window.window image

  let on_mouse_button f window =
    let g _window i b keys =
      f window i b keys
    in
    ignore (GLFW.setMouseButtonCallback ~window:window.window ~f:(Some g))

  let mouse_position t x y =
    let (w, h) = GLFW.getWindowSize ~window:t.window in
    let Any image = t.image in
    let image_width = Float.of_int image.width in
    let image_height = Float.of_int image.height in
    let x' = Float.max (Float.of_int w -. Float.of_int t.texture.width) 0.0 /. 2. in
    let y' = Float.max (Float.of_int h -. Float.of_int t.texture.height) 0.0 /. 2. in
    let x = Float.min (Float.max (x -. x') 0.0) image_width in
    let y = Float.min (Float.max (y -. y') 0.0) image_height in
    (x, y)

  let on_mouse_move f window =
    let g _window x y =
      let (x, y) = mouse_position window x y in
      f window x y
    in
    ignore (GLFW.setCursorPosCallback ~window:window.window ~f:(Some g))

  let on_key f window =
    let g _window =
      f window
    in
    ignore (GLFW.setKeyCallback ~window:window.window ~f:(Some g))

  let get_mouse_position t =
    let (x, y) = GLFW.getCursorPos ~window:t.window in
    mouse_position t x y

  let get_key t key =
    GLFW.getKey ~window:t.window ~key

  let get_mouse_button t button =
    GLFW.getMouseButton ~window:t.window ~button

  let show window =
    GLFW.showWindow ~window:window.window

  let hide window =
    GLFW.hideWindow ~window:window.window

  let close window =
    hide window;
    GLFW.setWindowShouldClose ~window:window.window ~b:true
end

let show_all windows' =
  let windows = Hashtbl.create 8 in
  List.iter (fun v ->
      Hashtbl.replace windows v v) windows';
  let should_close () = Hashtbl.fold (fun k v acc ->
      let acc = acc && GLFW.windowShouldClose ~window:v.Window.window in
      let () = if acc then
          let () = GLFW.hideWindow ~window:v.Window.window in
          Hashtbl.remove windows k
      in
      acc) windows true in
  let current_window () = Hashtbl.fold (fun _ v acc ->
      match acc with
      | Some x -> Some x
      | None -> if GLFW.getWindowAttrib ~window:v.Window.window ~attribute:Focused then
          Some v
        else None) windows None in
  while not (should_close ()) do
    let current_window = current_window () in
    let () = match current_window with
      | Some w ->
        Window.update w
      | None -> () in
    GLFW.pollEvents ()
  done

let show images =
  let windows = List.map (fun (k, v) ->
      let Image.Any v = v in
      let w = Window.create k v in
      Window.on_key (fun window key _ action _ ->
          match action, key with
          | GLFW.Press, GLFW.Escape -> Window.close window
          | _ -> ()
        ) w;
      w
    ) images
  in
  show_all windows
