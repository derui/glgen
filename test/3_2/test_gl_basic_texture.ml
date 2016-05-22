[%%suite
 open Core.Std
 open Sdlcaml.Std
 module S = Sdlcaml_image.Std
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
   [| -1.0; -1.0; 0.5773; 0.0; 0.0;
      0.0;  -1.0; -1.15475; 0.5; 0.0;
      1.0; -1.0; 0.5773; 1.0; 0.0;
      0.0; 1.0; 0.0; 0.5; 1.0;
   |]

 let world_trans ~scale ~trans ~rotate =
   let perspective = Ogl.Std.Camera.make_perspective_projection ~fov:30.0 ~ratio:(640.0 /. 480.0) ~near:1.0 ~far:1000.0 in
   let camera = Ogl.Std.Camera.make_matrix ~pos:[%vec [0.0;2.0;-3.0]]
     ~at:[%vec [0.45;0.0;1.0]]
     ~up:[%vec [0.0;1.0;0.0]]
   in 
   let open Typedvec.Std.Algebra.Mat.Open in
   perspective *: camera *: trans *: rotate *: scale

 let rotation x y z =
   let module M = Typedvec.Std.Algebra.Mat in 
   let rotate_z = Ogl.Std.Util.identity_mat4 () in
   M.set rotate_z ~row:0 ~col:0 ~v:(cos z);
   M.set rotate_z ~row:0 ~col:1 ~v:((sin z) *. -1.0);
   M.set rotate_z ~row:1 ~col:0 ~v:(sin z);
   M.set rotate_z ~row:1 ~col:1 ~v:(cos z);
   
   let rotate_y = Ogl.Std.Util.identity_mat4 () in
   M.set rotate_y ~row:0 ~col:0 ~v:(cos y);
   M.set rotate_y ~row:0 ~col:2 ~v:((sin y) *. -1.0);
   M.set rotate_y ~row:2 ~col:0 ~v:(sin y);
   M.set rotate_y ~row:2 ~col:2 ~v:(cos y);

   let rotate_x = Ogl.Std.Util.identity_mat4 () in
   M.set rotate_x ~row:1 ~col:1 ~v:(cos x);
   M.set rotate_x ~row:1 ~col:2 ~v:((sin x) *. -1.0);
   M.set rotate_x ~row:2 ~col:1 ~v:(sin x);
   M.set rotate_x ~row:2 ~col:2 ~v:(cos x);
   let open M.Open in
   rotate_x *: rotate_y *: rotate_z

 let make_world scale =
   let module M = Typedvec.Std.Algebra.Mat in 
   let scale_t = Ogl.Std.Util.identity_mat4 () in

   let pos = Ogl.Std.Util.identity_mat4 () in
   M.set ~row:0 ~col:3 ~v:(sin scale *. 0.1) pos;
   M.set ~row:1 ~col:3 ~v:(sin scale *. 0.1) pos;
   M.set ~row:2 ~col:3 ~v:1.0 pos;

   let angle = (sin scale) in
   let rotate = rotation angle angle angle in

   world_trans ~scale:scale_t ~trans:pos ~rotate

 let make_vbo () =
   let open Ogl_command in
   let buffer = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [|0l|] in
   gen_buffers ~n:1 ~buffers:buffer;
   let id = Bigarray.Array1.get buffer 0 in
   bind_buffer ~buffer:id ~target:Ogl_enum.gl_array_buffer;
   buffer_data ~target:Ogl_enum.gl_array_buffer
     ~size:(Bigarray.Array1.dim vertex_data * (Ctypes.sizeof Ctypes.float))
     ~data:vertex_data ~usage:Ogl_enum.gl_static_draw;
   buffer

 let vertex_shader_src = "\
#version 330 core\n
layout(location = 0) in vec3 VertexPosition;\n
layout(location = 1) in vec2 TexCoord;\n
uniform mat4 gWorld; \n
out vec2 TexCoord0;\n
void main () {\n
  gl_Position = gWorld * vec4(VertexPosition, 1.0);\n
  TexCoord0 = TexCoord;\n
}\n"

 let fragment_shader_src = "\
#version 330 core\n
in vec2 TexCoord0;\n
out vec4 FragColor;\n
uniform sampler2D gSampler;\n
void main() {\n
  FragColor = texture2D(gSampler, TexCoord0.st);\n
}\n"

 let%spec "Ogl can draw elements with index elements" =
   let open Flags in
   with_sdl (fun () ->
     let open Types.Result.Monad_infix in
     S.General.init [`PNG] >>= fun () ->
     Gl.use_version ~major:3 ~minor:3 () >>= fun () -> 
     Gl.set_attribute ~attr:Sdl_gl_attr.SDL_GL_DOUBLEBUFFER ~value:1 >>= fun () ->
     let window = Window.create ~title:"test" ~x:0 ~y:0 ~w:640 ~h:480 ~flags:[`OPENGL] in
     Gl.create_context window >>= (fun ctx ->
       S.Loader.load "sample.png" >>= fun tex ->
       Surface.rect tex >>= fun rect ->
       Surface.pixels ~surface:tex ~kind:Bigarray.int32 >>= fun pixels ->
       Gl.set_swap_interval 1 >>= fun () ->
       begin
         let open Ogl_command in
         let module A = Bigarray.Array1 in
         let sprog = Test_util.load_shaders ~vertex_shader:vertex_shader_src ~fragment_shader:fragment_shader_src in
         let arrays = A.of_array Bigarray.int32 Bigarray.c_layout [|0l|] in
         gen_vertex_arrays ~n:1 ~arrays;
         bind_vertex_array ~array:(A.get arrays 0);

         let pos = get_attrib_location ~program:sprog ~name:"VertexPosition" |> Int32.of_int_exn in
         let texture = get_attrib_location ~program:sprog ~name:"TexCoord" |> Int32.of_int_exn in
         let uniform_location = get_uniform_location ~program:sprog ~name:"gWorld" in
         let sampler_location = get_uniform_location ~program:sprog ~name:"gSampler" in
         sampler_location [@ne (-1)];
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

           (* Generate texture object *)
         let textures = A.of_array Bigarray.int32 Bigarray.c_layout [|0l|] in
         gen_textures ~n:1 ~textures;
         bind_texture ~target:Ogl_enum.Texture_target.gl_texture_2d ~texture:(A.get textures 0);
         tex_image2_d ~target:Ogl_enum.Texture_target.gl_texture_2d ~level:0
           ~internalformat:Int32.(to_int_exn Ogl_enum.Pixel_format.gl_rgba)
           ~width:(rect.Structures.Rect.w) ~height:(rect.Structures.Rect.h)
           ~border:0 ~format:Ogl_enum.Pixel_format.gl_rgba ~typ:Ogl_enum.Pixel_type.gl_unsigned_byte
           ~pixels;

         tex_parameterf ~target:Ogl_enum.Texture_target.gl_texture_2d
           ~pname:Ogl_enum.Texture_parameter_name.gl_texture_min_filter
           ~param:(Int32.to_float Ogl_enum.Texture_min_filter.gl_linear);
         tex_parameterf ~target:Ogl_enum.Texture_target.gl_texture_2d
           ~pname:Ogl_enum.Texture_parameter_name.gl_texture_mag_filter
           ~param:(Int32.to_float Ogl_enum.Texture_mag_filter.gl_linear);
         bind_texture ~target:Ogl_enum.Texture_target.gl_texture_2d ~texture:0l;
         uniform1i ~location:sampler_location ~v0:0;

         clear_color ~red:0.0 ~blue:0.4 ~green:0.0 ~alpha:0.0;
         front_face ~mode:Ogl_enum.Front_face_direction.gl_cw;
         cull_face ~mode:Ogl_enum.Cull_face_mode.gl_back;
         enable ~cap:Ogl_enum.Enable_cap.gl_cull_face;

         let rec loop counter scale =
           if counter = 0 then ()
           else 
             begin
               clear ~mask:Int32.(bit_or Ogl_enum.Clear_buffer_mask.gl_color_buffer_bit Ogl_enum.Clear_buffer_mask.gl_depth_buffer_bit);

               enable_vertex_attrib_array ~index:pos;
               enable_vertex_attrib_array ~index:texture;

               (* Make world matrix *)
               let m = make_world scale |> M.to_array |> Array.to_list |> Array.concat in
               let m = A.of_array Bigarray.float32 Bigarray.c_layout m in
               uniform_matrix4fv ~location:uniform_location ~count:1
                 ~transpose:true ~value:m;

               (* Bind buffer object to render *)
               bind_buffer ~target:Ogl_enum.gl_array_buffer ~buffer:(A.get vbobj 0);
               vertex_attrib_pointer ~index:pos ~size:3 ~typ:Ogl_enum.Vertex_pointer_type.gl_float
                 ~normalized:false ~stride:20 ~pointer:(`Offset 0);
               vertex_attrib_pointer ~index:texture ~size:2 ~typ:Ogl_enum.Vertex_pointer_type.gl_float
                 ~normalized:false ~stride:20 ~pointer:(`Offset 12);

                 (* Bind element buffer object to render *)
               bind_buffer ~buffer:ibo_buf ~target:Ogl_enum.gl_element_array_buffer;

               (* Active and bind texture unit *)
               active_texture ~texture:Ogl_enum.gl_texture0;
               bind_texture ~target:Ogl_enum.Texture_target.gl_texture_2d ~texture:(A.get textures 0);

                 (* Draw element with buffers that are binded before *)
               draw_elements ~mode:Ogl_enum.Primitive_type.gl_triangles ~count:12
                 ~typ:Ogl_enum.List_name_type.gl_unsigned_int ~indices:None;

               disable_vertex_attrib_array ~index:pos;
               disable_vertex_attrib_array ~index:texture;

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
