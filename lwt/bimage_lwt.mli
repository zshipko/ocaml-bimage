val iter :
  (int -> int -> ('b, 'c) Bimage.Data.t -> 'a -> 'a) ->
  ?x:int ->
  ?y:int ->
  ?width:int ->
  ?height:int -> ('b, 'c, 'd) Bimage.Image.t -> 'a -> 'a Lwt.t
