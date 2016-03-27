(**
   This module providing some generally functions for Ogl_command_* modules.

   @author derui
   @version 0.1
*)

val strings_to_bigarray2 : string list -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array2.t
(* [strings_to_bigarray2 strings] convert string list to a bigarray that memory layouted 2-dimension *)
