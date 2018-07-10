type t = [
  | `Invalid_shape
  | `Invalid_kernel_shape
  | `Msg of string
]

let to_string = function
  | `Invalid_shape -> "invalid shape"
  | `Invalid_kernel_shape -> "invalid kernel shape"
  | `Msg m -> m

exception Exc of t

let exc x = raise (Exc x)

let unwrap = function
  | Ok x -> x
  | Error e -> exc e
