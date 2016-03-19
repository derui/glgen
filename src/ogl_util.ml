open Core.Std
module A = Typedvec.Std.Algebra
module V = Typedvec.Std.Vec
module M = Typedvec.Std.Mat

module Camera = struct

  let make_matrix ~pos ~at ~up =
    let camera_direct = V.normalize (V.sub pos at) in
    let up = V.normalize up in
    let camera_x = V.normalize (V.cross up camera_direct) in
    let up = V.normalize (V.cross camera_direct camera_x) in
    let open V in
    let ex = -. (pos.x *. camera_x.x +. pos.y *. camera_x.y +. pos.z *. camera_x.z)
    and ey = -. (pos.x *. up.x +. pos.y *. up.y +. pos.z *. up.z)
    and ez = -. (pos.x *. camera_direct.x +. pos.y *. camera_direct.y +. pos.z *. camera_direct.z) in
    { M.m11 = camera_x.x; m12 = camera_x.y; m13 = camera_x.z; m14 = ex;
      m21 = up.x; m22 = up.y; m23 = up.z; m24 = ey;
      m31 = camera_direct.x; m32 = camera_direct.y; m33 = camera_direct.z; m34 = ez;
      m41 = 0.0; m42 = 0.0; m43 = 0.0; m44 = 1.0
    }


  let make_perspective_matrix ~fov ~ratio ~near ~far =
    let pi = 3.14159265358979323846 in
    let f = tan (fov /. 2.0 *. pi /. 180.0) in
    let a = -. ((far +. near) /. (far -. near))
    and b = -. ((2.0 *. far *. near) /. (far -. near)) in
    { M.m11 = f /. ratio   ; M.m12 = 0.0; M.m13 = 0.0; M.m14 =  0.0;
      M.m21 = 0.0 ; M.m22 = f;   M.m23 = 0.0; M.m24 =  0.0;
      M.m31 = 0.0 ; M.m32 = 0.0; M.m33 = a;   M.m34 = b;
      M.m41 = 0.0 ; M.m42 = 0.0; M.m43 = -1.0;   M.m44 =  0.0;
    }
end

module Shader = struct

  type result = Loaded of Gl_api.shader | Failed of string

  let vertex_suffix = "vert"
  let fragment_suffix = "frag"

  let compile_shader ty source =
    let open Gl_api in
    let id = glCreateShader ty in
    match glGetError () with
    | Get.GL_NO_ERROR ->
      glShaderSource id source;
      glCompileShader id;

      if glGetShader_bool ~shader:id ~pname:GetShader.GL_COMPILE_STATUS then
        Loaded (id)
      else
        let info = glGetShaderInfoLog id in
        glDeleteShader id;
        Failed (info)
    | _ -> Failed ("Some error occurs")

  let load ?shader_type filename =
    let open Gl_api in
    let load_file ty =
      In_channel.with_file filename
        ~f:(fun ch -> In_channel.input_all ch |> compile_shader ty) in
    match shader_type with
    | None ->
      if Filename.check_suffix filename vertex_suffix then
        load_file Shader.GL_VERTEX_SHADER
      else if Filename.check_suffix filename fragment_suffix then
        load_file Shader.GL_FRAGMENT_SHADER
      else
        failwith "unknown shader type"
    | Some(ty) -> load_file ty

  let info_log = Gl_api.glGetShaderInfoLog
  let source = Gl_api.glGetShaderSource
  let delete = Gl_api.glDeleteShader

end

module Program = struct

  let create = Gl_api.glCreateProgram
  let attach program shader = Gl_api.glAttachShader ~program ~shader
  let link p = Gl_api.glLinkProgram p
  let attrib_location program name = Gl_api.glGetAttribLocation ~program ~name
  let uniform_location program name = Gl_api.glGetUniformLocation ~program ~name

  let location program locate name =
    match locate with
    | `Attrib -> attrib_location program name
    | `Uniform -> uniform_location program name

end

let ortho_projection ~left ~right ~top ~bottom ~near ~far =
  let width_ratio = 2.0 /. (right -. left)
  and height_ratio = 2.0 /. (top -. bottom)
  and z_ratio = -2.0 /. (far -. near)
  and tx = -1.0 *. (right +. left) /. (right -. left)
  and ty = -1.0 *. (top +. bottom) /. (top -. bottom)
  and tz = -1.0 *. (far +. near) /. (far -. near) in

  { M.m11 = width_ratio ; M.m12 = 0.0;          M.m13 = 0.0; M.m14 = tx;
    M.m21 = 0.0         ; M.m22 = height_ratio; M.m23 = 0.0; M.m24 = ty;
    M.m31 = 0.0         ; M.m32 = 0.0;          M.m33 = z_ratio; M.m34 = tz;
    M.m41 = 0.0         ; M.m42 = 0.0;          M.m43 = 0.0; M.m44 = 1.0;
  }

let perspective_projection ~left ~right ~top ~bottom ~near ~far =
  let maxY = top
  and minY = -. top
  and minX = left
  and maxX = right in

  let x_diff = maxX -. minX in
  let y_diff = maxY -. minY in
  let z_diff = far -. near in
  let near_twice = 2.0 *. near in

  let a = near_twice /. x_diff
  and b = near_twice /. y_diff
  and c = (maxX +. minX) /. x_diff
  and d = (maxY +. minY) /. y_diff
  and e = -. (far +. near) /. z_diff
  and f = -. (near_twice *. far) /. z_diff
  in
  { M.m11 = a   ; M.m12 = 0.0; M.m13 = c; M.m14 =  0.0;
    M.m21 = 0.0 ; M.m22 = b;   M.m23 = d; M.m24 =  0.0;
    M.m31 = 0.0 ; M.m32 = 0.0; M.m33 = e;   M.m34 = f;
    M.m41 = 0.0 ; M.m42 = 0.0; M.m43 = -1.0;   M.m44 =  0.0;
  }
