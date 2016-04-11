[%%suite

 open Core.Std
 open Sdlcaml.Std
 module M = Typedvec.Std.Algebra.Mat
 module Alg = Typedvec.Std.Algebra

 module Ogl_command = Ogl_command_3_2
 module Ogl_enum = Ogl_enum_3_2

 let with_sdl f =
   let open Flags in
   Init.init [Sdl_init_flags.SDL_INIT_VIDEO];
   f ();
   Init.quit ()

 let index_data = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout
   [|
     0l;3l;1l;
     1l;3l;2l;
     2l;3l;0l;
     0l;1l;2l;
   |]

 let vertex_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
   [| -1.0; -1.0; 0.0;
      0.0;  -1.0; 1.0;
      1.0; -1.0; 0.0;
      0.0; 1.0; 0.0;
   |]

 let matrix = Ogl.Std.Util.identity_mat4 () 
 let transform_matrix scale =
   M.set matrix ~row:0 ~col:0 ~v:(cos scale);
   M.set matrix ~row:0 ~col:2 ~v:((sin scale) *. -1.0);
   M.set matrix ~row:2 ~col:0 ~v:(sin scale);
   M.set matrix ~row:2 ~col:2 ~v:(cos scale);
   matrix

 let make_vbo () =
   let open Ogl_command in
   let buffer = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [|0l|] in
   gen_buffers ~n:1 ~buffers:buffer;
   let id = Bigarray.Array1.get buffer 0 in
   bind_buffer ~buffer:id ~target:Ogl_enum.gl_array_buffer;
   buffer_data ~target:Ogl_enum.gl_array_buffer ~size:(Bigarray.Array1.dim vertex_data * (Ctypes.sizeof Ctypes.float))
     ~data:vertex_data ~usage:Ogl_enum.gl_static_draw;
   buffer

 let vertex_shader_src = "\
#version 330 core\n
layout(location = 0) in vec3 VertexPosition;\n
uniform mat4 gWorld; \n
out vec4 Color; \n
void main () {\n
  gl_Position = gWorld * vec4(VertexPosition, 1.0);\n
  Color = vec4(clamp(VertexPosition, 0.0, 1.0), 1.0);\n
}\n"

 let fragment_shader_src = "\
#version 330 core\n
in vec4 Color;\n
out vec4 FragColor;\n
void main() {\n
  FragColor = Color;\n
}\n"

 let%spec "Ogl can draw elements with index elements" =
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
         let uniform_location = get_uniform_location ~program:sprog ~name:"gWorld" in
         uniform_location [@ne (-1)];

         let vbobj = make_vbo () in
         use_program sprog;
         bind_buffer ~target:Ogl_enum.gl_array_buffer ~buffer:(A.get vbobj 0);

         let ibo = A.of_array Bigarray.int32 Bigarray.c_layout [|0l|] in
         gen_buffers ~n:1 ~buffers:ibo;
         let ibo_buf = A.get ibo 0 in 
         bind_buffer ~buffer:ibo_buf ~target:Ogl_enum.gl_element_array_buffer;
         buffer_data ~target:Ogl_enum.gl_element_array_buffer
           ~size:(Bigarray.Array1.dim index_data * (Ctypes.sizeof Ctypes.uint))
           ~data:index_data ~usage:Ogl_enum.gl_static_draw;

         let rec loop counter scale =
           if counter = 0 then ()
           else 
             begin
               clear_color ~red:0.0 ~blue:0.4 ~green:0.0 ~alpha:0.0;
               clear ~mask:Int32.(bit_or Ogl_enum.Clear_buffer_mask.gl_color_buffer_bit Ogl_enum.Clear_buffer_mask.gl_depth_buffer_bit);

               enable_vertex_attrib_array ~index:pos;

               let m = transform_matrix scale |> M.to_array |> Array.to_list |> Array.concat in
               let m = A.of_array Bigarray.float32 Bigarray.c_layout m in
               uniform_matrix4fv ~location:uniform_location ~count:1
                 ~transpose:true ~value:m;

               bind_buffer ~target:Ogl_enum.gl_array_buffer ~buffer:(A.get vbobj 0);
               vertex_attrib_pointer ~index:pos ~size:3 ~typ:Ogl_enum.Vertex_pointer_type.gl_float
                 ~normalized:false ~stride:0 ~pointer:None;

               bind_buffer ~buffer:ibo_buf ~target:Ogl_enum.gl_element_array_buffer;

               draw_elements ~mode:Ogl_enum.Primitive_type.gl_triangles ~count:12
                 ~typ:Ogl_enum.List_name_type.gl_unsigned_int ~indices:None;

               disable_vertex_attrib_array ~index:pos;

               Gl.swap_window window;
               Timer.delay 16l;
               loop (pred counter) (scale +. 0.01)
             end
         in
         loop 100 0.1;

         delete_vertex_arrays ~n:1 ~arrays;
         delete_buffers ~n:1 ~buffers:ibo;
         delete_buffers ~n:1 ~buffers:vbobj;
       end;
       Types.Result.return ctx
       >>= Gl.delete_context) |> ignore;
     Window.destroy window |> Types.Result.return
   )
]
