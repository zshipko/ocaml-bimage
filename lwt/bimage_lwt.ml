open Bimage

include Filter.Make(struct
  type 'a io = 'a Lwt.t
  let bind = Lwt.bind
  let detach = Lwt_preemptive.detach
  let return = Lwt.return
end)
