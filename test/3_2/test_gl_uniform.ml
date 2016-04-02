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

let vertex_shader_src = "
#version 330 core\n
layout(location = 0) in vec3 VertexPosition;\n
uniform float gScale; \n
void main () {\n
  gl_Position = vec4(gScale * VertexPosition.x, gScale * VertexPosition.y, VertexPosition.z, 1.0);\n
}\n"

let fragment_shader_src = "
#version 330 core\n
out vec3 color;\n
void main() {\n
  color = vec3(1.0, 0.0, 0.0);\n
}\n"

let%spec "Ogl can apply uniform to location" =
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
        let vertex_pos = get_attrib_location ~program:sprog ~name:"VertexPosition" |> Int32.of_int_exn in
        let uniform_location = get_uniform_location ~program:sprog ~name:"gScale" in
        uniform_location [@ne (-1)];

        let arrays = A.of_array Bigarray.int32 Bigarray.c_layout [|0l|] in
        gen_vertex_arrays ~n:1 ~arrays;
        bind_vertex_array ~array:(A.get arrays 0);

        let vbobj = make_vbo () in
        use_program sprog;
        enable_vertex_attrib_array ~index:vertex_pos;
        bind_buffer ~target:Ogl_enum.gl_array_buffer ~buffer:(A.get vbobj 0);

        let rec loop counter scale =
          if counter = 0 then ()
          else 
            begin
              clear_color ~red:0.0 ~blue:0.4 ~green:0.0 ~alpha:0.0;
              clear ~mask:Int32.(bit_or Ogl_enum.Clear_buffer_mask.gl_color_buffer_bit Ogl_enum.Clear_buffer_mask.gl_depth_buffer_bit);

              vertex_attrib_pointer ~index:vertex_pos ~size:3 ~typ:Ogl_enum.Vertex_pointer_type.gl_float
                ~normalized:false ~stride:0 ~pointer:None;
              uniform1f ~location:uniform_location ~v0:scale;
              draw_arrays ~mode:Ogl_enum.Primitive_type.gl_triangles ~first:0 ~count:3;

              Gl.swap_window window;
              Timer.delay 16l;
              loop (pred counter) (scale +. 0.01)
            end
        in
        loop 100 0.1;

        delete_vertex_arrays ~n:1 ~arrays;
        delete_buffers ~n:1 ~buffers:vbobj;
      end;
      Types.Result.return ctx
      >>= Gl.delete_context) |> ignore;
    Window.destroy window |> Types.Result.return
  )
