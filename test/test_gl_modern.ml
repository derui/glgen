open Sdlcaml.Std

module Ogl = Ogl_command_3_2
module Ogl_enum = Ogl_enum_3_2

let with_sdl f = 
  let open Flags in
  Init.init [Sdl_init_flags.SDL_INIT_VIDEO];
  f ();
  Init.quit ()

let%spec "The SDL Renderer module can create renderer of the window" =
  let open Flags in
  with_sdl (fun () -> 
    let window = Window.create ~title:"test" ~x:0 ~y:0 ~w:100 ~h:200 ~flags:[`HIDDEN;`OPENGL] in
    let open Types.Result.Monad_infix in
    let ctx = Gl.create_context window in
    ctx >>= (fun ctx -> begin
      
      let vertex_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
        [| -1.0; -1.0; 0.0;
           0.0;  1.0; 0.0;
           1.0; -1.0; 0.0;
        |] in
      let open Ogl in
      let buffer = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [|0l|] in 
      gen_buffers ~n:1 ~buffers:buffer;
      let id = Bigarray.Array1.get buffer 0 in
      bind_buffer ~buffer:id ~target:Ogl_enum.gl_array_buffer;
      buffer_data ~target:Ogl_enum.gl_array_buffer ~size:(Bigarray.Array1.dim vertex_data)
        ~data:vertex_data ~usage:Ogl_enum.gl_static_draw;

      let module A = Bigarray.Array1 in 
      let params = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [|0l|] in 
        (* get_buffer_parameteriv ~target:Ogl_enum.gl_array_buffer *)
        (*   ~pname:Ogl_enum.gl_buffer_access ~params; *)
        (* (A.get params 0) [@eq Ogl_enum.gl_read_write]; *)

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
    >>= fun () -> Gl.delete_context ctx |> Types.Result.return) |> ignore;
    Window.destroy window
  )

(* let vertex_shader_src = " *)
(* #version 130 *)
(* in vec3 VertexPosition; *)
(* invariant gl_Position; *)
(* void main () { *)
(*   gl_Position = vec4(VertexPosition, 1.0); *)
(* }" *)

(* let fragment_shader_src = " *)
(* #version 130 *)
(* out vec4 Color; *)
(* void main() { *)
(*   Color = vec4(1.0, 0.0, 0.0, 1.0); *)
(* }" *)

(* let load_shaders () = *)
(*   let open Gl.Api in *)
(*   let vertexShaderID = glCreateShader Shader.GL_VERTEX_SHADER in *)
(*   let fragmentShaderID = glCreateShader Shader.GL_FRAGMENT_SHADER in *)

(*   glShaderSource vertexShaderID vertex_shader_src; *)
(*   glShaderSource fragmentShaderID fragment_shader_src; *)

(*   glCompileShader vertexShaderID; *)
(*   glCompileShader fragmentShaderID; *)

(*   let shader_prog = glCreateProgram () in *)
(*   glAttachShader ~program:shader_prog ~shader:vertexShaderID; *)
(*   glAttachShader ~program:shader_prog ~shader:fragmentShaderID; *)

(*   glLinkProgram shader_prog; *)
(*   let vertexPosAttrib = glGetAttribLocation shader_prog "VertexPosition" in *)
(*   (shader_prog, vertexPosAttrib) *)

(* let test_sdl_gl_basic surface = *)
(*   let open Gl.Api in *)
(*   let open Gl.VBO in *)
(*   begin *)
(*     let vao = glGenVertexArray () in *)
(*     glBindVertexArray vao; *)
(*     let vbobj = make_vbo () in *)
(*     let sprog, pos = load_shaders () in *)

(*     glViewport 0 0 640 480; *)
(*     glClear [Clear.GL_COLOR_BUFFER_BIT;]; *)

(*     glUseProgram sprog; *)
(*     glBindBuffer Buffer.GL_ARRAY_BUFFER vbobj; *)
(*     glVertexAttribPointer ~index:pos ~size:3 ~vert_type:VertexArray.GL_FLOAT ~normalize:false *)
(*       ~stride:0; *)
(*     glEnableVertexAttribArray pos; *)
(*     glDrawArrays ~mode:DrawArrays.GL_TRIANGLES ~first:0 ~count:9; *)

(*     Sdl_video.gl_swap (); *)

(*     Thread.delay 3.0; *)
(*   end *)

