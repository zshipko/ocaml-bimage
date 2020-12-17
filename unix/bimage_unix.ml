module Magick = Magick
module Stb = Stb
module Data_unix = Data
module Image_unix = Image

module Thread = struct
  module Filter = struct
    include Bimage.Filter.Make (struct
      type 'a io = 'a

      let threads = Hashtbl.create 12

      let bind x (f : unit -> unit io) =
        let x = f x in
        x

      let detach f x =
        let t = Thread.create f x in
        Hashtbl.replace threads (Thread.id t) t

      let wrap f = f ()

      let wait () = Hashtbl.iter (fun _ v -> Thread.join v) threads
    end)
  end
end
