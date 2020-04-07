type t =
  [ `Invalid_shape
  | `Invalid_kernel_shape of int * int
  | `Invalid_input of int
  | `Invalid_layout
  | `Invalid_color
  | `Msg of string ]

let to_string = function
  | `Invalid_shape -> "invalid shape"
  | `Invalid_kernel_shape (r, c) ->
      Printf.sprintf "invalid kernel shape: %dx rows x %d cols" r c
  | `Invalid_input index -> Printf.sprintf "invalid input index: %d" index
  | `Invalid_layout -> "invalid layout"
  | `Invalid_color -> "invalid color"
  | `Msg m -> m

exception Exc of t

let exc x = raise (Exc x)

let unwrap = function Ok x -> x | Error e -> exc e

let string_of_exn = function
  | Exc x -> to_string x
  | x -> Printexc.to_string x
