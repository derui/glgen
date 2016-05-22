(* This module defines command bindings that are can not generate automatic from gl.xml. *)
open Core.Std

(* Get nullable argument name of given command. *)
let nullable_args = function
  | "glDrawElements" | "glDrawElementsBaseVertex" |
      "glDrawElementsInstanced" -> ["indices"]
  | _ -> []

let is_offset_or_index = function
  | "glVertexAttribPointer" | "glVertexAttribIPointer" |
      "glVertexAttribLPointer" -> ["pointer"]
  | _ -> []

let command_definitions = function
  (* glMapBuffer *)
  | "glMapBuffer" -> format_of_string "@[<hov 2>let map_buffer ~size ~kind ~target ~access =@ \
@[let ret = Inner.map_buffer target access in@]@ \
@[let typ = typ_of_bigarray_kind kind in@]@ \
@[bigarray_of_ptr array1 size kind (from_voidp typ ret)@]\
@]@." |> Option.some

  (* glMapBufferRange *)
  | "glMapBufferRange" -> format_of_string "@[<hov 2>let map_buffer_range ~target ~offset ~length ~access ~kind =@ \
@[let ret = Inner.map_buffer_range target offset length access in@]@ \
@[let typ = typ_of_bigarray_kind kind in@]@ \
@[bigarray_of_ptr array1 length kind (from_voidp typ ret)@]\
@]@." |> Option.some

  (* glMapNamedBuffer *)
  | "glMapNamedBuffer" -> format_of_string "@[<hov 2>let map_named_buffer ~size ~kind ~target ~access =@ \
@[let ret = Inner.map_buffer target access in@]@ \
@[let typ = typ_of_bigarray_kind kind in@]@ \
@[bigarray_of_ptr array1 size kind (from_voidp typ ret)@]\
@]@." |> Option.some

  (* glMapNamedBufferRange *)
  | "glMapNamedBufferRange" -> format_of_string "@[<hov 2>let map_named_buffer_range ~target ~offset ~length ~access ~kind =@ \
@[let ret = Inner.map_buffer_range target offset length access in@]@ \
@[let typ = typ_of_bigarray_kind kind in@]@ \
@[bigarray_of_ptr array1 length kind (from_voidp typ ret)@]\
@]@." |> Option.some

  | _ -> None

let command_declarations = function
  (* glMapBuffer *)
  | "glMapBuffer" -> format_of_string "@[val map_buffer:@ size:int@ ->@ kind:('a, 'f) Bigarray.kind@ \
->@ target:int32@ ->@ access:int32@ ->@ ('a, 'f) bigarray@]@." |> Option.some

  (* glNamedBuffer *)
  | "glMapNamedBuffer" -> format_of_string "@[val map_named_buffer:@ size:int@ ->@ kind:('a, 'f) Bigarray.kind@ \
->@ target:int32@ ->@ access:int32@ ->@ ('a, 'f) bigarray@]@." |> Option.some

  (* glMapBufferRange *)
  | "glMapBufferRange" -> format_of_string "@[val map_buffer_range:@ target:int32@ ->@ offset:int@ \
->@ length:int@ ->@ access:int32@ \
->@ kind:('a, 'f) Bigarray.kind@ ->@ ('a, 'f) bigarray@]@." |> Option.some

  (* glMapNamedBufferRange *)
  | "glMapNamedBufferRange" -> format_of_string "@[val map_named_buffer_range:@ target:int32@ ->@ offset:int@ \
->@ length:int@ ->@ access:int32@ \
->@ kind:('a, 'f) Bigarray.kind@ ->@ ('a, 'f) bigarray@]@." |> Option.some

  | _ -> None
