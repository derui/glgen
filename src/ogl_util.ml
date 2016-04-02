module A = Typedvec.Std.Algebra
module S = Typedvec.Std.Size

open Core.Std

let strings_to_bigarray2 strings =
  List.map strings ~f:String.to_array
  |> List.to_array
  |> Bigarray.Array2.of_array Bigarray.char Bigarray.c_layout

let empty_mat4 () = A.Mat.make ~row:S.four ~col:S.four ~init:0.0
let identity_mat4 () = A.Mat.identity S.four
