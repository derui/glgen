open Core.Std
open Sdlcaml.Std

module Ogl_command = Ogl_command_3_2
module Ogl_enum = Ogl_enum_3_2

let with_sdl f =
  let open Flags in
  Init.init [Sdl_init_flags.SDL_INIT_VIDEO];
  f ();
  Init.quit ()

let vertex_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
  [| -1.0; -1.0; 0.0;
     1.0;  -1.0; 0.0;
     0.0; 1.0; 0.0;
  |]

let make_vbo () =
  let open Ogl_command in
  let buffer = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [|0l|] in
  gen_buffers ~n:1 ~buffers:buffer;
  let id = Bigarray.Array1.get buffer 0 in
  bind_buffer ~buffer:id ~target:Ogl_enum.gl_array_buffer;
  buffer_data ~target:Ogl_enum.gl_array_buffer ~size:(Bigarray.Array1.dim vertex_data * (Ctypes.sizeof Ctypes.float))
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
      (A.get params 0) [@eq 36l];

      delete_buffers ~n:1 ~buffers:buffer;
      Types.Result.return ()
    end
                        >>= fun () -> Gl.delete_context ctx) |> ignore;
    Window.destroy window
  )

let vertex_shader_src = "
#version 330 core\n
layout(location = 0) in vec3 VertexPosition;\n
void main () {\n
  gl_Position.xyz = VertexPosition;\n
  gl_Position.w = 1.0;\n
}\n"

let fragment_shader_src = "
#version 330 core\n
out vec3 color;\n
void main() {\n
  color = vec3(1.0, 0.0, 0.0);\n
}\n"

let%spec "Vertex Buffer Object can draw in context " =
  let open Flags in
  with_sdl (fun () ->
    let open Types.Result.Monad_infix in
    Gl.use_version ~major:3 ~minor:3 () >>= fun () -> 
    Gl.set_attribute ~attr:Sdl_gl_attr.SDL_GL_DOUBLEBUFFER ~value:1 >>= fun () ->
    let window = Window.create ~title:"test" ~x:0 ~y:0 ~w:640 ~h:480 ~flags:[`OPENGL] in
    Gl.create_context window >>= (fun ctx ->
    Gl.set_swap_interval 1 >>= fun () ->
      begin
        let open Ogl_command in
        let module A = Bigarray.Array1 in
        let sprog = Test_util.load_shaders ~vertex_shader:vertex_shader_src ~fragment_shader:fragment_shader_src in
        let arrays = A.of_array Bigarray.int32 Bigarray.c_layout [|0l|] in
        gen_vertex_arrays ~n:1 ~arrays;
        bind_vertex_array ~array:(A.get arrays 0);
        let pos = get_attrib_location ~program:sprog ~name:"VertexPosition" |> Int32.of_int_exn in

        let vbobj = make_vbo () in
        use_program sprog;
        enable_vertex_attrib_array ~index:pos;
        bind_buffer ~target:Ogl_enum.gl_array_buffer ~buffer:(A.get vbobj 0);
        vertex_attrib_pointer ~index:pos ~size:3 ~typ:Ogl_enum.Vertex_pointer_type.gl_float
                              ~normalized:false ~stride:0 ~pointer:None;

        let rec loop counter =
          if counter = 0 then ()
          else 
            begin
              clear_color ~red:0.0 ~blue:0.4 ~green:0.0 ~alpha:0.0;
              clear ~mask:Int32.(bit_or Ogl_enum.Clear_buffer_mask.gl_color_buffer_bit Ogl_enum.Clear_buffer_mask.gl_depth_buffer_bit);

              point_size ~size:5.0;
              draw_arrays ~mode:Ogl_enum.Primitive_type.gl_triangles ~first:0 ~count:3;

              Gl.swap_window window;
              Timer.delay 16l;
              loop (pred counter)
            end
        in
        loop 100;

        delete_vertex_arrays ~n:1 ~arrays;
        delete_buffers ~n:1 ~buffers:vbobj;
      end;
      Types.Result.return ctx
      >>= Gl.delete_context) |> ignore;
    Window.destroy window |> Types.Result.return
    )
