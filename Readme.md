# OCaml OpenGL binding library and generator
Ogl is a library that is OpenGL binding for OCaml, and contains generator for OpenGL bindings each versions of OpenGL.

## Install
You should use opam if you want install this library. Instructions as follows are manual install.

Before, you should install requirements libraries. Recommend use opam for.
- ctypes
- typedvec

And then, type command in you console on the directory checkouted this.

```
$ omake install
```

## Use library
This library providing OpenGL bindings, but not contains all functions of OpenGL 3.2/3.2 and between 4.0 to 4.5 .

Provided libraries in this repository are as **library** , not **package** , so sample using this library is follows for OCaml source for OpenGL 3.2 .

```ocaml
(* Compiling this source must link ogl_3_2.cma/cmxa file *)
(* Ogl_<major>_<minor> module contains functions OpenGL defined *)
module Ogl = Ogl_3_2
(* Ogl_enum_<major>_<minor> module contains enums OpenGL defined *)
module Ogl_enum = Ogl_enum_3_2
```

You can use multi version OpenGL library version in one program with link multi version of it, but should not use multi version.
Functions and enums defined by OpenGL has backward compatibility each version (some versions has breaking changes), then you decide the version of library provided to use in a program.
