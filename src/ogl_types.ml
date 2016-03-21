module A = Typedvec.Std.Algebra
module S = Typedvec.Std.Size

type vec = (S.three S.t) A.vec
type mat4 = (S.four S.t, S.four S.t) A.mat

let empty_mat4 () = A.Mat.make ~row:S.four ~col:S.four ~init:0.0
