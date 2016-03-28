open Sdlcaml.Std

module Ogl_command = Ogl_command_3_2
module Ogl_enum = Ogl_enum_3_2

let with_sdl f =
  let open Flags in
  Init.init [Sdl_init_flags.SDL_INIT_VIDEO];
  f ();
  Init.quit ()

let make_vbo () =
  let vertex_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    [| -1.0; -1.0; 0.0;
       0.0;  1.0; 0.0;
       1.0; -1.0; 0.0;
    |] in
  let open Ogl_command in
  let buffer = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [|0l|] in
  gen_buffers ~n:1 ~buffers:buffer;
  let id = Bigarray.Array1.get buffer 0 in
  bind_buffer ~buffer:id ~target:Ogl_enum.gl_array_buffer;
  buffer_data ~target:Ogl_enum.gl_array_buffer ~size:(Bigarray.Array1.dim vertex_data)
    ~data:vertex_data ~usage:Ogl_enum.gl_static_draw;
  buffer

let%spec "Vertex Buffer Object can use in core profile" =
  let open Flags in
  with_sdl (fun () ->
    let window = Window.create ~title:"test" ~x:0 ~y:0 ~w:100 ~h:200 ~flags:[`HIDDEN;`OPENGL] in
    let open Types.Result.Monad_infix in
    let ctx = Gl.create_context window in
    ctx >>= (fun ctx -> begin

      let open Ogl_command in
      let buffer = make_vbo () in

      let module A = Bigarray.Array1 in
      let params = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [|0l|] in
      get_buffer_parameteriv ~target:Ogl_enum.gl_array_buffer
        ~pname:Ogl_enum.gl_buffer_access ~params;
      (A.get params 0) [@eq Ogl_enum.gl_read_write];

      get_buffer_parameteriv ~target:Ogl_enum.gl_array_buffer
        ~pname:Ogl_enum.gl_buffer_mapped ~params;
      (A.get params 0) [@eq Ogl_enum.Boolean.gl_false];

      get_buffer_parameteriv ~target:Ogl_enum.gl_array_buffer
        ~pname:Ogl_enum.gl_buffer_usage ~params;
      (A.get params 0) [@eq Ogl_enum.gl_static_draw];

      get_buffer_parameteriv ~target:Ogl_enum.gl_array_buffer
        ~pname:Ogl_enum.gl_buffer_size ~params;
      (A.get params 0) [@eq 9l];

      delete_buffers ~n:1 ~buffers:buffer;
      Types.Result.return ()
    end
                        >>= fun () -> Gl.delete_context ctx) |> ignore;
    Window.destroy window
  )

let vertex_shader_src = "
#version 130
in vec3 VertexPosition;
invariant gl_Position;
void main () {
  gl_Position = vec4(VertexPosition, 1.0);
}"

let fragment_shader_src = "
#version 130
out vec4 Color;
void main() {
  Color = vec4(1.0, 0.0, 0.0, 1.0);
}"

let load_shaders () =
  let open Ogl_command in
  let module A = Bigarray.Array1 in
  let vertexShaderID = create_shader Ogl_enum.gl_vertex_shader in
  let fragmentShaderID = create_shader Ogl_enum.gl_fragment_shader in

  let to_len_ary src =
    let len = String.length vertex_shader_src |> Int32.of_int in
    A.of_array Bigarray.int32 Bigarray.c_layout [|len|]
  in
  shader_source ~shader:vertexShaderID ~count:1
    ~string:(Ogl.Std.Util.strings_to_bigarray2 [vertex_shader_src])
    ~length:(to_len_ary vertex_shader_src);
  shader_source ~shader:fragmentShaderID ~count:1
    ~string:(Ogl.Std.Util.strings_to_bigarray2 [fragment_shader_src])
    ~length:(to_len_ary fragment_shader_src);

  compile_shader vertexShaderID;
  compile_shader fragmentShaderID;

  let shader_prog = create_program () in
  attach_shader ~program:shader_prog ~shader:vertexShaderID;
  attach_shader ~program:shader_prog ~shader:fragmentShaderID;

  link_program shader_prog;
  let vertexPosAttrib = get_attrib_location ~program:shader_prog ~name:"VertexPosition" in
  (shader_prog, Int32.of_int vertexPosAttrib)

let%spec "Vertex Buffer Object can draw in context " =
  let open Flags in
  with_sdl (fun () ->
    let window = Window.create ~title:"test" ~x:0 ~y:0 ~w:100 ~h:200 ~flags:[`HIDDEN;`OPENGL] in
    let open Types.Result.Monad_infix in
    let ctx = Gl.create_context window in
    ctx >>= (fun ctx -> begin
      let open Ogl_command in
      let module A = Bigarray.Array1 in
      let arrays = A.of_array Bigarray.int32 Bigarray.c_layout [|0l|] in
      gen_vertex_arrays ~n:1 ~arrays;
      let vao = A.get arrays 0 in
      bind_vertex_array ~array:vao;
      let vbobj = make_vbo () in
      let sprog, pos = load_shaders () in

      viewport ~x:0 ~y:0 ~width:640 ~height:480;
      clear ~mask:Ogl_enum.Clear_buffer_mask.gl_color_buffer_bit;

      use_program sprog;
      bind_buffer ~target:Ogl_enum.gl_array_buffer ~buffer:(A.get vbobj 0);
      let pointer = A.of_array Bigarray.float32 Bigarray.c_layout [||] in
      vertex_attrib_pointer ~index:pos ~size:3 ~typ:Ogl_enum.Vertex_pointer_type.gl_float
        ~normalized:false ~stride:0 ~pointer;
      enable_vertex_attrib_array ~index:pos;
      draw_arrays ~mode:Ogl_enum.Primitive_type.gl_triangles ~first:0 ~count:9;
      delete_buffers ~n:1 ~buffers:vbobj;
    end;
      Types.Result.return ctx
      >>= Gl.delete_context) |> ignore;
    Window.destroy window
  )
