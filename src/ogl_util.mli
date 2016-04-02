(**
   This module providing some generally functions for Ogl_command_* modules.

   @author derui
   @version 0.1
*)
module A = Typedvec.Std.Algebra
module S = Typedvec.Std.Size

val empty_mat4: unit -> Ogl_types.mat4
(* [empty_mat4 ()] get a new empty matrix that fill in 0.0 all. *)

val identity_mat4: unit -> Ogl_types.mat4
(* [identity_mat4 ()] get a new identity matrix 4x4 *)

val strings_to_bigarray2 : string list -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t
(* [strings_to_bigarray2 strings] convert string list to a bigarray that memory layouted 2-dimension *)
