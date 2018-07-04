open Bimage

let windows : (string, GWindow.window * GMisc.image) Hashtbl.t = Hashtbl.create 32

let pixbuf_of_mat width height channels ptr =
  let ptr = Gpointer.region_of_bigarray ptr in
  GdkPixbuf.from_data ~rowstride:(width * channels) ~has_alpha:(channels = 4) ~width ~height ~bits:8 ptr

type t = string

let create ?width:(width=800) ?height:(height=600) ?mousedown ?mouseup ?keydown ?keyup ?timer title m =
  GMain.init () |> ignore;
  let window = GWindow.window ~title ~width ~height ~deletable:true () in
  let scroll = GBin.scrolled_window ~width ~height ~show:true () in
  let image = GMisc.image () in
  let width, height, channels = Image.shape m in
  let pixbuf = pixbuf_of_mat width height channels m.Image.data in
  image#set_pixbuf pixbuf;
  scroll#add_with_viewport image#coerce;
  window#add scroll#coerce;
  let timer = match timer with
    | None -> None
    | Some (ms, callback) -> Some (GMain.Timeout.add ~ms ~callback)
  in
  window#connect#destroy ~callback:(fun () ->
    Hashtbl.remove windows title;
    let () = match timer with None -> () | Some timer -> ignore (GMain.Timeout.remove timer) in
    if Hashtbl.length windows = 0 then
      ignore (GMain.Timeout.add ~ms:100 ~callback:(fun () -> GMain.quit (); false))
  ) |> ignore;
  Hashtbl.replace windows title (window, image);
  window#event#add [`BUTTON_PRESS; `BUTTON_RELEASE; `KEY_PRESS; `KEY_RELEASE];
  let () =
    match mousedown with
    | Some mousedown ->
      ignore @@ window#event#connect#button_press ~callback:(fun event ->
        mousedown (GdkEvent.Button.x event |> int_of_float) (GdkEvent.Button.y event |> int_of_float);
        true)
    | None -> ()
  in
  let () =
    match mouseup with
    | Some mouseup ->
      ignore @@ window#event#connect#button_release ~callback:(fun event ->
        mouseup (GdkEvent.Button.x event |> int_of_float) (GdkEvent.Button.y event |> int_of_float);
        true)
    | None -> ()
  in
  let () =
    match keydown with
    | Some keydown ->
      ignore @@ window#event#connect#key_press ~callback:(fun event ->
        let c = GdkEvent.Key.keyval event in
        keydown c;
        true)
    | None -> ()
  in
  let () =
    match keyup with
    | Some keyup ->
      ignore @@ window#event#connect#key_release ~callback:(fun event ->
        let c = GdkEvent.Key.keyval event in
        keyup c;
        true)
    | None -> ()
  in
  title

let exists title =
  Hashtbl.mem windows title

let find title =
  try
    Some (Hashtbl.find windows title)
  with Not_found -> None

let get name =
  if exists name then Some name else None

let resize ~width ~height title =
  match find title with
  | Some (window, _) ->
      window#resize ~width ~height
  | None -> ()

let destroy title =
  match find title with
  | Some (window, _) ->
    window#destroy ()
  | None -> ()

let update title m =
  match find title with
  | Some (_, image) ->
    let width, height, channels = Image.shape m in
    if width = (GdkPixbuf.get_width image#pixbuf) && height = (GdkPixbuf.get_height image#pixbuf) then
        let pixbuf = pixbuf_of_mat width height channels m.Image.data in
        image#set_pixbuf pixbuf
  | None -> ()

let show_all ?ms:(ms=0) () =
  if Hashtbl.length windows = 0 then ()
  else
    if ms > 0 then
      GMain.Timeout.add ~ms ~callback:(fun () -> GMain.quit (); false) |> ignore;
    Hashtbl.iter (fun _ (v, _) -> v#show ()) windows;
    GMain.main ()
