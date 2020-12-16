open Bimage

module Filter = Filter.Make (struct
  type 'a io = 'a Lwt.t

  let bind = Lwt.bind

  let detach = Lwt_preemptive.detach

  let wrap = Lwt.wrap

  let wait (x : unit io) = Lwt_preemptive.run_in_main (fun () -> x)
end)
