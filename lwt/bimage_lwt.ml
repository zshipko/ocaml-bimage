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
