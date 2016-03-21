(**
   This module providing some utility function for camera

   @author derui
   @version 0.1
*)

val make_matrix: pos:Ogl_types.vec -> at:Ogl_types.vec -> up:Ogl_types.vec
  -> Ogl_types.mat4
(** Make camera matrix and return it.
    Camera matrix is made by three elements, position of camera, position of looking at,
    and direction of above of camera.

    @param pos position of camera
    @param at position looked by camera
    @param up up-direction of camera
    @return Camera transformation matrix made by arguments.
*)

val make_perspective_projection : fov:float ->
  ratio:float ->
  near:float ->
  far:float -> Ogl_types.mat4
(**
   construct a projection matrix.
   `fov' is that the field of view angle in the y direction.
   `ratio` is screen ratio that determines the field of view in the x direction.
   near and far is clip that visible range along the Z axis.

   @param fov Field Of View
   @param the ratio of x to y
   @param near near clip
   @param far far clip
*)

val make_ortho_projection : left:float -> right:float ->
  top:float -> bottom:float -> near:float -> far:float -> Ogl_types.mat4
(**
   construct a projection matrix for `ortho projection'.
   given parameters of near and far are mostly equivalant of
   perspective_projection arguments.
   Left and right specifies the coordinates for the left and right vertical clipping planes.
   Bottom and top specifies the coodinates for the left and right horizontal clipping planes.
*)
