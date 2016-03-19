(* Define generator for .mli of bindings. *)

module C = Command

open Core.Std
module Type_map = Map.Make(String)

let convert_type_to_type_annotation typ =
  let open Capi in
  match typ.Ocaml_type.def with
  | `Builtin -> typ.Ocaml_type.name
  | `Alias s -> s
  | `Abstract s -> s

let convert_param_to_type_annotation param =
  let typ, name = param in
  match Capi.capi_to_ocaml_type_def typ with
  | `Unknown s -> failwith s
  | `Ok typ -> let typ = convert_type_to_type_annotation typ in
               let name = Util.convert_gl_to_ocaml_name name in 
               Format.asprintf "%s:%s -> " name typ

(* Command generator *)
let generate_commands ppf commands =
  let open Core.Std in
  let generate_command ppf command =
    let ret_typ, name = command.C.proto in
    match Special_defs.command_declarations name with
    | Some f -> Format.fprintf ppf f
    | None -> begin
      let name = Util.convert_gl_to_ocaml_name name in
      let ret_typ = Capi.capi_to_ocaml_type_def ret_typ in
      match ret_typ with
      | `Unknown s -> failwith s
      | `Ok typ -> begin
        let typ_name = convert_type_to_type_annotation typ in
        let params = List.map command.C.params ~f:(fun param ->
          convert_param_to_type_annotation param
        ) in
        let params = if List.is_empty params then ["unit -> "] else params in 
        Format.fprintf ppf "@[val %s: %s %s@]@ " name (String.concat params) typ_name
      end
    end
  in
  Format.fprintf ppf "@[<hov 0>\
@[(* {2:binding_types Types used by bindings} *)@]";
  List.iter commands ~f:(generate_command ppf);
  Format.fprintf ppf "@]"

let print_types ppf =
  Format.fprintf ppf "@[\
(* #1 types for bindings *)
type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
@]@\n"

let print_packages ppf =
  Format.fprintf ppf "@[open Ctypes@\n@]"

let generate_mli ppf commands =
  print_packages ppf;
  print_types ppf;
  generate_commands ppf commands
