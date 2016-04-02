open Core.Std
module Ogl_command = Ogl_command_3_2
module Ogl_enum = Ogl_enum_3_2

let print_compile_log shader =
  let open Ogl_command in
  let module A = Bigarray.Array1 in 
  let params = A.of_array Bigarray.int32 Bigarray.c_layout [|0l|] in
  get_shaderiv ~shader ~pname:Ogl_enum.gl_compile_status ~params;

  if (A.get params 0) <> Ogl_enum.Boolean.gl_true then
    let log = A.create Bigarray.char Bigarray.c_layout 1024 in
    get_shader_info_log ~shader ~buf_size:1024 ~length:params ~info_log:log;
    let log = Ctypes.bigarray_start Ctypes.array1 log |> Ctypes.string_from_ptr ~length:1024 in 
    Printf.printf "compile error: %s\n" log
  else ()

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
  print_compile_log vertexShaderID;
  compile_shader fragmentShaderID;
  print_compile_log fragmentShaderID;

  let shader_prog = create_program () in
  attach_shader ~program:shader_prog ~shader:vertexShaderID;
  attach_shader ~program:shader_prog ~shader:fragmentShaderID;

  link_program shader_prog;
  detach_shader ~program:shader_prog ~shader:vertexShaderID;
  detach_shader ~program:shader_prog ~shader:fragmentShaderID;

  delete_shader vertexShaderID;
  delete_shader fragmentShaderID;
  shader_prog
