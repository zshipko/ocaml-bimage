module Magick = Magick
module Stb = Stb
module Data_unix = Data
module Image_unix = Image

module Thread = struct
  module IO = struct
    type 'a t = Thread.t list * 'a
  end

  module Filter = struct
    include Bimage.Filter.Make (struct
      type 'a io = 'a IO.t

      let bind (t, x) (f : unit -> unit io) =
        let t', x = f x in
        (t @ t', x)

      let detach f x =
        let t' = Thread.create f x in
        ([ t' ], ())

      let wrap f = ([], f ())

      let wait (t, ()) = List.iter Thread.join t
    end)
  end
end
