
open Core.Std

let strings_to_bigarray2 strings =
  List.map strings ~f:String.to_array
  |> List.to_array
  |> Bigarray.Array2.of_array Bigarray.char Bigarray.c_layout
