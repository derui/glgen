open Core.Std
open Sdlcaml.Std
open Ctypes

let with_sdl f =
  let open Flags.Sdl_init_flags in
  Init.init [SDL_INIT_VIDEO];
  protect ~f ~finally:(fun () -> Init.quit ())

let%spec "SDL GL can create and delete context" =
  with_sdl (fun () ->
    let open Types.Result.Monad_infix in
    let window = Window.create ~title:"test" ~x:100 ~y:100 ~h:100 ~w:100 ~flags:[`OPENGL] in
    let monad = Gl.create_context window >>=
      fun context -> Gl.delete_context context |> Types.Result.return in
    Window.destroy window;
    (Types.Result.is_success monad) [@true "create and delete"]
  )

let%spec "SDL GL can set and get attribute for OpenGL window" =
  with_sdl (fun () ->
    let open Types.Result.Monad_infix in
    let open Flags.Sdl_gl_attr in
    Gl.reset_attributes ();
    let ret = Gl.set_attribute ~attr:SDL_GL_RED_SIZE ~value:5
              >>= fun () -> Gl.set_attribute ~attr:SDL_GL_GREEN_SIZE ~value:5
              >>= fun () -> Gl.set_attribute ~attr:SDL_GL_BLUE_SIZE ~value:5
              >>= fun () -> Gl.set_attribute ~attr:SDL_GL_ALPHA_SIZE ~value:0 in
    (Types.Result.is_success ret) [@true "set complete"];

    let window = Window.create ~title:"test" ~x:100 ~y:100 ~h:100 ~w:100 ~flags:[`OPENGL] in
    let ret = Gl.create_context window >>=
      fun context -> Gl.get_attribute SDL_GL_RED_SIZE >>=
      fun r -> Gl.get_attribute SDL_GL_GREEN_SIZE >>=
      fun g -> Gl.get_attribute SDL_GL_BLUE_SIZE >>=
      fun b -> Gl.get_attribute SDL_GL_ALPHA_SIZE >>=
      fun a -> begin
        (r >= 5) [@true "set red size"];
        (g >= 5) [@true "set green size"];
        (b >= 5) [@true "set blue size"];
        (a >= 0) [@true "set alpha size"];
        Types.Result.return context
      end
              >>= fun context -> Gl.delete_context context |> Types.Result.return in
    Window.destroy window;
    (Types.Result.is_success ret) [@true "get complete"];
  )
