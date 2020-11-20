open Bimage
open Tsdl

type image = Image : (_, _, _) Image.t -> image

type t = {
  image : image;
  sdl_window : Sdl.window;
  sdl_renderer : Sdl.renderer;
  sdl_texture : Sdl.texture;
  sdl_surface : Sdl.surface;
}

let window { sdl_window; _ } = sdl_window

let texture { sdl_texture; _ } = sdl_texture

let surface { sdl_surface; _ } = sdl_surface

let renderer { sdl_renderer; _ } = sdl_renderer

let determine_format c =
  if c = 3 then
    if Sys.big_endian then Sdl.Pixel.format_bgr24 else Sdl.Pixel.format_rgb24
  else if Sys.big_endian then Sdl.Pixel.format_rgba8888
  else Sdl.Pixel.format_abgr8888

let make_surface image renderer =
  let w, h, c = Image.shape image in
  let depth = 8 * c in
  let pitch = w * c in
  let f = determine_format c in
  match
    Sdl.create_rgb_surface_with_format_from image.Image.data ~w ~h ~pitch ~depth
      f
  with
  | Ok surface -> (
      match Sdl.create_texture_from_surface renderer surface with
      | Ok tx -> Ok (tx, surface)
      | Error e -> Error e )
  | Error e -> Error e

let create ?title flags image =
  let w, h, _ = Image.shape image in
  match Sdl.create_window_and_renderer ~w ~h flags with
  | Ok (w, r) -> (
      (match title with Some t -> Sdl.set_window_title w t | None -> ());
      match make_surface image r with
      | Ok (tx, s) ->
          Ok
            {
              image = Image image;
              sdl_window = w;
              sdl_renderer = r;
              sdl_texture = tx;
              sdl_surface = s;
            }
      | Error e -> Error e )
  | Error e -> Error e

let update ?image window =
  let (Image image) =
    match image with Some image -> Image image | None -> window.image
  in
  let w, _, c = Image.shape image in
  Sdl.update_texture window.sdl_texture None image.Image.data (w * c)

let draw window =
  ignore @@ Sdl.render_clear window.sdl_renderer;
  ignore @@ Sdl.render_copy window.sdl_renderer window.sdl_texture;
  Sdl.render_present window.sdl_renderer
