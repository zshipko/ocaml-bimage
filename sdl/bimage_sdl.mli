open Bimage
open Tsdl

type t

val window: t -> Sdl.window
val texture: t -> Sdl.texture
val surface: t -> Sdl.surface
val renderer: t -> Sdl.renderer

val create: ?title:string -> Sdl.Window.flags -> (int, u8, [< rgb | rgba ]) Image.t -> t Sdl.result
val update: t -> (int, u8, [< rgb | rgba ]) Image.t -> unit Sdl.result
val draw: t -> unit
