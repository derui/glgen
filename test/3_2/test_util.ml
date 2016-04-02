open Core.Std
module Ogl_command = Ogl_command_3_2
module Ogl_enum = Ogl_enum_3_2

let load_shaders ~vertex_shader ~fragment_shader =
  let open Ogl_command in
  let module A = Bigarray.Array1 in
  let vertexShaderID = create_shader Ogl_enum.gl_vertex_shader in
  let fragmentShaderID = create_shader Ogl_enum.gl_fragment_shader in

  let to_len_ary src =
    let len = String.length src |> Int32.of_int_exn in
    A.of_array Bigarray.int32 Bigarray.c_layout [|len|]
  in
  shader_source ~shader:vertexShaderID ~count:1
    ~string:(Ogl.Std.Util.strings_to_bigarray2 [vertex_shader])
    ~length:(to_len_ary vertex_shader);
  shader_source ~shader:fragmentShaderID ~count:1
    ~string:(Ogl.Std.Util.strings_to_bigarray2 [fragment_shader])
    ~length:(to_len_ary fragment_shader);

  compile_shader vertexShaderID;
  compile_shader fragmentShaderID;

  let shader_prog = create_program () in
  attach_shader ~program:shader_prog ~shader:vertexShaderID;
  attach_shader ~program:shader_prog ~shader:fragmentShaderID;

  link_program shader_prog;
  detach_shader ~program:shader_prog ~shader:vertexShaderID;
  detach_shader ~program:shader_prog ~shader:fragmentShaderID;

  delete_shader vertexShaderID;
  delete_shader fragmentShaderID;
  shader_prog
