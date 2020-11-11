module type FILTER = sig
  type 'a io
  type ('a, 'b, 'c) t =
    output:('a, 'b, 'c) Image.t -> Input.t -> unit io
end

module Filter = struct
  module Make(S: sig type 'a io end) : FILTER with type 'a io = 'a S.io = struct
    type 'a io = 'a S.io

    type ('a, 'b, 'c) t =
      output:('a, 'b, 'c) Image.t -> Input.t -> unit io
  end
end
