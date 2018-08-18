type t = [
  | `Invalid_shape
  | `Invalid_kernel_shape of int * int
  | `Invalid_input of int
  | `Invalid_layout
  | `Msg of string
]

let to_string = function
  | `Invalid_shape -> "invalid shape"
  | `Invalid_kernel_shape (r, c) -> Printf.sprintf "invalid kernel shape: %dx rows x %d cols" r c
  | `Invalid_input index -> Printf.sprintf "invalid input index: %d" index
  | `Invalid_layout -> "invalid_layout"
  | `Msg m -> m

exception Exc of t

let exc x = raise (Exc x)

let unwrap = function
  | Ok x -> x
  | Error e -> exc e
