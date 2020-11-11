open Bimage

let iter f ?(x = 0) ?(y = 0) ?width ?height img (acc: 'a) : 'a Lwt.t =
  let open Lwt.Infix in
  let open Image in
  let width =
    match width with Some w -> min (img.width - x) w | None -> img.width - x
  in
  let height =
    match height with
    | Some h -> min (img.height - y) h
    | None -> img.height - y
  in
  let rec inner img x' y' acc =
    if x' >= width then
      inner img x (y' + 1) acc
    else if y' >= height then
      Lwt.return acc
    else
      let px = get_data img x' y' in
      Lwt_preemptive.detach (fun () ->
        f x' y' px acc
      ) () >>= inner img (x' + 1) y'
    in inner img x y acc

let eval ?(x = ref 0) ?(y = ref 0) ?(c = ref 0) op : output:('a, 'b, 'c) Image.t -> Input.t -> unit Lwt.t =
 fun ~output inputs ->
   let open Lwt.Infix in
   let width, _height, channels = Image.shape output in
   let kind = Image.ty output in
   let of_float f = Type.of_float kind f in
   let denormalize = Type.denormalize kind in
   let clamp = Type.clamp kind in
   let op = op inputs in
   let rec inner i =
     if i >= Image.length output then Lwt.return_unit
     else
     Lwt_preemptive.detach (fun (x, y, c) ->
       let f = op x y c in
       Bigarray.Array1.unsafe_set output.data i (of_float @@ denormalize @@ clamp f);
      ) (!x, !y, !c) >>= fun () ->
     (* Increment channel index *)
     incr c;
     (* If channel index is greater than the number of channels
        * then reset channel index to 0 and increment x index *)
     let () =
       if !c >= channels then
         let () = c := 0 in
         incr x
     in
     (* If x index is greater than the width then reset x index to 0
        * and increment y index *)
     let () =
       if !x >= width then
         let () = x := 0 in
         incr y
     in inner (i + 1)
   in inner 0

