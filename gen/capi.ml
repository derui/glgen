module Base_type = struct
  type t = [
    `GLenum
  | `GLboolean
  | `GLbitfield
  | `GLvoid
  | `GLbyte
  | `GLshort
  | `GLint
  | `GLintptr
  | `GLclampx
  | `GLubyte
  | `GLushort
  | `GLuint
  | `GLsizei
  | `GLsizeiptr
  | `GLfloat
  | `GLclampf
  | `GLdouble
  | `GLclampd
  | `GLchar
  | `GLfixed
  | `GLint64
  | `GLuint64
  | `GLsync
  | `Void
  | `GLDEBUGPROC
  ]

  let of_string = function
    | "GLenum" -> `GLenum
    | "GLboolean" -> `GLboolean
    | "GLbitfield" -> `GLbitfield
    | "GLvoid" -> `GLvoid
    | "GLbyte" -> `GLbyte
    | "GLshort" -> `GLshort
    | "GLint" -> `GLint
    | "GLintptr" -> `GLintptr
    | "GLclampx" -> `GLclampx
    | "GLubyte" -> `GLubyte
    | "GLushort" -> `GLushort
    | "GLuint" -> `GLuint
    | "GLsizei" -> `GLsizei
    | "GLsizeiptr" -> `GLsizeiptr
    | "GLfloat" -> `GLfloat
    | "GLclampf" -> `GLclampf
    | "GLdouble" -> `GLdouble
    | "GLclampd" -> `GLclampd
    | "GLchar" -> `GLchar
    | "GLfixed" -> `GLfixed
    | "GLint64" -> `GLint64
    | "GLuint64" -> `GLuint64
    | "GLsync" -> `GLsync
    | "void" -> `Void
    | "GLDEBUGPROC" -> `GLDEBUGPROC
    | s -> failwith (Printf.sprintf "Unknown base type %s" s)

  let string_of = function
    | `GLenum -> "GLenum"
    | `GLboolean -> "GLboolean"
    | `GLbitfield -> "GLbitfield"
    | `GLvoid -> "GLvoid"
    | `GLbyte -> "GLbyte"
    | `GLshort -> "GLshort"
    | `GLint -> "GLint"
    | `GLintptr -> "GLintptr"
    | `GLclampx -> "GLclampx"
    | `GLubyte -> "GLubyte"
    | `GLushort -> "GLushort"
    | `GLuint -> "GLuint"
    | `GLsizei -> "GLsizei"
    | `GLsizeiptr -> "GLsizeiptr"
    | `GLfloat -> "GLfloat"
    | `GLclampf -> "GLclampf"
    | `GLdouble -> "GLdouble"
    | `GLclampd -> "GLclampd"
    | `GLchar -> "GLchar"
    | `GLfixed -> "GLfixed"
    | `GLint64 -> "GLint64"
    | `GLuint64 -> "GLuint64"
    | `GLsync -> "GLsync"
    | `Void -> "Void"
    | `GLDEBUGPROC -> "GLDEBUGPROC"
end

module Capi_type = struct
(* A type of C api as OCaml type system *)
  type t = [
  | `Base of Base_type.t
  | `Const of t
  | `Ptr of t
  ]

  let to_string t = 
    let rec loop acc = function
      | `Base t -> acc ^ (Base_type.string_of t)
      | `Const t -> loop ("const " ^ acc) t
      | `Ptr t -> loop ("ptr " ^ acc) t
    in
    loop "" t
end

(* Providing type and type definitions mapped for ocaml and Ctypes. *)
module Ocaml_type = struct
  type ctypes = [
    `Builtin of string
  | `View of string * string * string * string
  | `Def of string * string
  | `Conversion of string * (Format.formatter -> string -> unit)
  ]

  type t = {
    name: string;
    def: [ `Alias of string | `Abstract of string | `Builtin];
    ctypes: ctypes;
  }

  (* for GLchar *)
  let gl_char = {
    name = "char";
    def = `Builtin;
    ctypes = `Builtin "uchar";
  }

  (* for GLbyte *)
  let gl_byte = {
    name = "gl_byte";
    def = `Alias "int";
    ctypes = `Builtin "char";
  }

  (* for GLubyte *)
  let gl_ubyte = {
    name = "gl_ubyte";
    def = `Alias "int";
    ctypes = `View ("int_as_gl_ubyte_t",
                    "Unsigned.UInt8.to_int", "Unsigned.UInt8.of_int",
                    "uint8_t")
  }

  (* for GLenum *)
  let gl_enum = {
    name = "gl_enum";
    def = `Alias "int32";
    ctypes = `View ("int32_as_uint32_t",
                    "Unsigned.UInt32.to_int32", "Unsigned.UInt32.of_int32",
                    "uint32_t");
  }

  (* for GLboolean *)
  let gl_boolean = {
    name = "bool";
    def = `Builtin;
    ctypes = `View ("bool_as_uint8_t",
                    "(fun v -> Unsigned.UChar.(compare v zero <> 0))",
                    "(fun b -> Unsigned.UChar.(of_int (Pervasives.compare b false)))",
                    "uchar")
  }

  (* for GLint *)
  let gl_int = {
    name = "int";
    def = `Builtin;
    ctypes = `Builtin "int";
  }

  (* for GLuint *)
  let gl_uint = {
    name = "int";
    def = `Builtin;
    ctypes = `View ("int_as_uint",
                    "Unsigned.UInt.to_int", "Unsigned.UInt.of_int",
                    "uint")
  }

  (* for GLbitfield *)
  let gl_bitfield = {gl_uint with name = "gl_bitfield"; def = `Alias "int"}

  (* for GLvoid *)
  let gl_void = {
    name = "unit";
    def = `Builtin;
    ctypes = `Builtin "void";
  }

  (* for void *)
  let void = gl_void

  (* for GLshort *)
  let gl_short = {
    name = "int";
    def = `Builtin;
    ctypes = `Builtin "short";
  }

  (* for GLushort *)
  let gl_ushort = {
    name = "int";
    def = `Builtin;
    ctypes = `Builtin "ushort";
  }

  (* for GLint32 *)
  let gl_int32 = {
    name = "int32";
    def = `Builtin;
    ctypes = `Builtin "int32_t";
  }

  (* for GLuint32 *)
  let gl_uint32 = {
    name = "uint32";
    def = `Alias "int32";
    ctypes = `View ("int32_as_uint32_t",
                    "Unsigned.UInt32.to_int32", "Unsigned.UInt32.of_int32",
                    "uint32_t");
  }

  (* for GLfloat *)
  let gl_float = {
    name = "float";
    def = `Builtin;
    ctypes = `Builtin "float";
  }

  (* for GLdouble *)
  let gl_double = {
    name = "float";
    def = `Builtin;
    ctypes = `Builtin "double";
  }

  (* for GLint64 *)
  let gl_int64 = {
    name = "int64";
    def = `Builtin;
    ctypes = `Builtin "int64_t";
  }

  (* for GLuint64 *)
  let gl_uint64 = {
    name = "int64";
    def = `Builtin;
    ctypes = `View ("int64_as_uint64_t",
                    "Unsigned.UInt64.to_int64", "Unsigned.UInt64.of_int64",
                    "uint64_t")
  }

  (* for GLfixed *)
  let gl_fixed = {gl_int with name = "fixed"; def = `Alias "int"}

  let gl_intptr = gl_int
  let gl_clampx = gl_int
  let gl_sizei = gl_int
  let gl_sizeiptr = gl_int

  let gl_clampf = gl_float

  let gl_clampd = gl_double

  (* for GLsync *)
  let gl_sync = {
    name = "gl_sync";
    def = `Abstract "unit ptr";
    ctypes = `Def ("gl_sync",
                   "let gl_sync : gl_sync typ = ptr void\n \
 let gl_sync_opt : gl_sync option typ = ptr_opt void"
    )
  }

  let gl_debug_proc = {
    name = "(int32 -> int32 -> int32 -> int32 -> int -> string -> unit ptr -> unit)";
    def = `Builtin;
    ctypes = `Def ("gl_debug_proc",
                   "let gl_debug_proc = int32_as_uint32_t @-> \
int32_as_uint32_t @-> \
int32_as_uint32_t @-> \
int @-> string @-> (ptr void) @-> returning void"
    )
  }

  let string = {
    name = "string";
    def = `Builtin;
    ctypes = `Builtin "string";
  }

  let ctype_ba_as_voidp name = `View (
    name,
    "(fun _ -> assert false)",
    "(fun b -> to_voidp (bigarray_start array1 b))",
    "(ptr void)"
  )

  (* for char */const char * *)
  let ba_as_charp = {
    name = "(char, Bigarray.int8_unsigned_elt) bigarray";
    def = `Builtin;
    ctypes = ctype_ba_as_voidp "ba_as_charp"
  }

  (* for char **/const char ** *)
  let ba_as_stringp = {
    name = "(string, Bigarray.int8_unsigned_elt) bigarray";
    def = `Builtin;
    ctypes = ctype_ba_as_voidp "ba_as_stringp"
  }

  let ba_as_int8p = {
    name = "(int, Bigarray.int8_signed_elt) bigarray";
    def = `Builtin;
    ctypes = ctype_ba_as_voidp "ba_as_int8p"
  }

  let ba_as_uint8p = {
    name = "uint8_bigarray";
    def = `Alias "(int, Bigarray.int8_unsigned_elt) bigarray";
    ctypes = ctype_ba_as_voidp "ba_as_uint8p"
  }

  let ba_as_int16p = {
    name = "(int, Bigarray.int16_signed_elt) bigarray";
    def = `Builtin;
    ctypes = ctype_ba_as_voidp "ba_as_int16p"
  }

  let ba_as_uint16p = {
    name = "uint16_bigarray";
    def = `Alias "(int, Bigarray.int16_unsigned_elt) bigarray";
    ctypes = ctype_ba_as_voidp "ba_as_uint16p"
  }

  let ba_as_int32p = {
    name = "(int32, Bigarray.int32_elt) bigarray";
    def = `Builtin;
    ctypes = ctype_ba_as_voidp "ba_as_int32p"
  }

  let ba_as_uint32p = {
    name = "uint32_bigarray";
    def = `Alias "(int32, Bigarray.int32_elt) bigarray";
    ctypes = ctype_ba_as_voidp "ba_as_uint32p";
  }

  let ba_as_nativeintp = {
    name = "(nativeint, Bigarray.nativeint_elt) bigarray";
    def = `Builtin;
    ctypes = ctype_ba_as_voidp "ba_as_nativeintp"
  }

  let ba_as_int64p = {
    name = "(int64, Bigarray.int64_elt) bigarray";
    def = `Builtin;
    ctypes = ctype_ba_as_voidp "ba_as_int64p"
  }

  let ba_as_uint64p = {
    name = "uint64_bigarray";
    def = `Alias "(int64, Bigarray.int64_elt) bigarray";
    ctypes = ctype_ba_as_voidp "ba_as_uint64p";
  }

  let ba_as_doublep = {
    name = "(float, Bigarray.float64_elt) bigarray";
    def = `Builtin;
    ctypes = ctype_ba_as_voidp "ba_as_doublep";
  }

  let ba_as_floatp = {
    name = "(float, Bigarray.float32_elt) bigarray";
    def = `Builtin;
    ctypes = ctype_ba_as_voidp "ba_as_floatp";
  }

  let ba_as_voidp =
    let pp_wrap f arg =
      Format.fprintf f
        "@[let %s = to_voidp (bigarray_start array1 %s) in\n@]" arg arg
    in
    {
      name = "('a, 'b) bigarray";
      def = `Builtin;
      ctypes = `Conversion ("(ptr void)", pp_wrap);
    }

end

(* Convert capi_type to definition of ocaml_type *)
let capi_to_ocaml_type_def t =
  let unknown_def t =
    let str = Capi_type.to_string t in
    `Unknown (Printf.sprintf "No OCaml type definition for %s" str)
  in
  match t with
  | `Base t -> begin match t with
    | `GLenum -> `Ok Ocaml_type.gl_enum
    | `GLboolean -> `Ok Ocaml_type.gl_boolean
    | `GLbitfield -> `Ok Ocaml_type.gl_bitfield
    | `GLvoid -> `Ok Ocaml_type.gl_void
    | `GLbyte -> `Ok Ocaml_type.gl_byte
    | `GLshort -> `Ok Ocaml_type.gl_short
    | `GLint -> `Ok Ocaml_type.gl_int
    | `GLintptr -> `Ok Ocaml_type.gl_intptr
    | `GLclampx -> `Ok Ocaml_type.gl_clampx
    | `GLubyte -> `Ok Ocaml_type.gl_ubyte
    | `GLushort -> `Ok Ocaml_type.gl_ushort
    | `GLuint -> `Ok Ocaml_type.gl_uint
    | `GLsizei -> `Ok Ocaml_type.gl_sizei
    | `GLsizeiptr -> `Ok Ocaml_type.gl_sizeiptr
    | `GLfloat -> `Ok Ocaml_type.gl_float
    | `GLclampf -> `Ok Ocaml_type.gl_clampf
    | `GLdouble -> `Ok Ocaml_type.gl_double
    | `GLclampd -> `Ok Ocaml_type.gl_clampd
    | `GLchar -> `Ok Ocaml_type.gl_char
    | `GLfixed -> `Ok Ocaml_type.gl_fixed
    | `GLint64 -> `Ok Ocaml_type.gl_int64
    | `GLuint64 -> `Ok Ocaml_type.gl_uint64
    | `GLsync -> `Ok Ocaml_type.gl_sync
    | `GLDEBUGPROC -> `Ok Ocaml_type.gl_debug_proc
    | `Void -> `Ok Ocaml_type.void
  end
  (* char * *)
  | `Ptr (`Base `GLchar) -> `Ok Ocaml_type.ba_as_charp
  | `Ptr (`Ptr (`Base `GLchar)) -> `Ok Ocaml_type.ba_as_stringp
  (* void ** *)
  | `Ptr (`Ptr (`Base `Void)) -> `Ok Ocaml_type.ba_as_nativeintp
  | `Const (`Ptr (`Ptr (`Base `GLchar))) -> `Ok Ocaml_type.ba_as_stringp
  | `Ptr (`Base base)
  | `Const (`Ptr (`Base base)) -> begin match base with
    (* const char * as string. *)
    | `GLchar -> `Ok Ocaml_type.string
    | `GLuint -> `Ok Ocaml_type.ba_as_uint32p
    | `GLintptr -> `Ok Ocaml_type.ba_as_uint32p
    | `GLsizei -> `Ok Ocaml_type.ba_as_uint32p
    | `GLsizeiptr -> `Ok Ocaml_type.ba_as_uint32p
    | `GLbyte -> `Ok Ocaml_type.ba_as_int8p
    | `GLdouble -> `Ok Ocaml_type.ba_as_doublep
    | `GLfloat -> `Ok Ocaml_type.ba_as_floatp
    | `GLint -> `Ok Ocaml_type.ba_as_int32p
    | `GLshort -> `Ok Ocaml_type.ba_as_int16p
    | `GLushort -> `Ok Ocaml_type.ba_as_uint16p
    | `GLubyte -> `Ok Ocaml_type.ba_as_uint8p
    | `GLfixed -> `Ok Ocaml_type.ba_as_uint32p
    | `GLboolean -> `Ok Ocaml_type.ba_as_uint8p
    | `GLenum -> `Ok Ocaml_type.ba_as_uint32p
    | `GLint64 -> `Ok Ocaml_type.ba_as_uint64p
    | `GLuint64 -> `Ok Ocaml_type.ba_as_uint64p
    | `Void -> `Ok Ocaml_type.ba_as_voidp
    | _ -> unknown_def t
  end
  | t -> unknown_def t

let ocaml_type_to_ctype_name typ = 
  match typ.Ocaml_type.ctypes with
  | `Builtin n -> n
  | `View (n, _, _, _) -> n
  | `Def (n, _) -> n
  | `Conversion (t, _) -> t
