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
  type image = Image : ('a, 'b, 'c) Image.t -> image
  type t = { texture : Texture.t; window : GLFW.window; image: image }

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
    let t = { window; texture; image = Image image } in
    update t;
    t
  and update window =
    let Image image = window.image in
    Texture.draw window.texture window.window image

  let show window =
    GLFW.showWindow ~window:window.window

  let hide window =
    GLFW.hideWindow ~window:window.window
end

let show images =
  let windows = Hashtbl.create (List.length images) in
  List.iter (fun (k, v) ->
      let Bimage.Image.Any v = v in
      Hashtbl.replace windows k (Window.create k v)) images;
  let should_close () = Hashtbl.fold (fun k v acc ->
      let acc = acc && GLFW.windowShouldClose ~window:v.Window.window in
      let () = if acc then
          let () = GLFW.hideWindow ~window:v.Window.window in
          Hashtbl.remove windows k
      in
      acc) windows true in
  let current_window () =Hashtbl.fold (fun _ v acc ->
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
