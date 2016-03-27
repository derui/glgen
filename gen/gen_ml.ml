(* Define generator for .ml of bindings. *)

module C = Command

open Core.Std
module Type_map = Map.Make(String)

let to_ocaml_types commands =
  let open Core.Std in 
  let module M = Type_map in
  let params = List.map commands ~f:(fun command -> command.C.params)
  |> List.join |> List.map ~f:Tuple2.get1 in
  let ret_types = List.map commands ~f:(fun command -> command.C.proto) |> List.map ~f:Tuple2.get1 in

  List.join [params;ret_types] |> List.map ~f:Capi.capi_to_ocaml_type_def
  |> List.fold_left ~f:(fun map typ ->
    match typ with
    | `Unknown s -> Printf.fprintf stderr "%s" s; map
    | `Ok s -> M.add map ~key:s.Capi.Ocaml_type.name ~data:s
  ) ~init:M.empty

let print_types ppf commands =
  let open Core.Std in 
  Format.fprintf ppf "@[\
(* #1 types for bindings *)
type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
type ('a, 'b) bigarray2 = ('a, 'b, Bigarray.c_layout) Bigarray.Array2.t
@]\n";

  let ctypes = to_ocaml_types commands in
  Type_map.data ctypes |> List.iter ~f:(fun ctype ->
    let module O = Capi.Ocaml_type in 
    match ctype.O.def with
    | `Abstract typ -> Format.fprintf ppf "@[type %s = %s@.@]" ctype.O.name typ
    | `Builtin | `Alias _ -> ()
  )

let print_ctype_definitions ppf commands =
  let open Core.Std in 
  let module M = Type_map in

  let ctypes = to_ocaml_types commands in 
  let module O = Capi.Ocaml_type in
  (* view definitions prints first, these are as primitive types for definitions follows *)
  M.data ctypes |> List.filter ~f:(fun typ ->
    match typ.O.ctypes with
    | `View _ -> true
    | _ -> false
  ) |> List.iter ~f:(fun typ ->
    match typ.O.ctypes with
    | `View (name, read, write, typ) ->
       Format.fprintf ppf "@[let %s = view ~read:%s ~write:%s %s@]" name read write typ
    | _ -> ()
  );

  M.data ctypes |> List.iter ~f:(fun typ ->
    let module O = Capi.Ocaml_type in
    match typ.O.ctypes with
    | `Def (name, def) -> Format.fprintf ppf "@[%s@]" def
    | _ -> ()
  )

let print_packages ppf =
  Format.fprintf ppf "@[open Ctypes\n\
open Foreign\n
@]"

(* Ctypes foreign bindings generator *)
let generate_foreign ppf command =
  let open Core.Std in 
  let ret_typ, orig_name = command.C.proto in
  let name = Util.convert_gl_to_ocaml_name orig_name in
  match Capi.capi_to_ocaml_type_def ret_typ with
  | `Unknown s -> failwith s
  | `Ok typ -> begin
    let typ_name = Capi.ocaml_type_to_ctype_name typ in
    let params = List.map command.C.params ~f:Tuple2.get1 |> List.map ~f:Capi.capi_to_ocaml_type_def in
    let params = List.fold_left params ~f:(fun lst t ->
      match t with
      | `Unknown s -> failwith s
      | `Ok t -> t :: lst
    ) ~init: [] |> List.map ~f:Capi.ocaml_type_to_ctype_name |> List.rev
      |> (fun lst -> if List.is_empty lst then ["void"] else lst)
      |> String.concat ~sep:" @-> " 
    in

    Format.fprintf ppf "@[let %s = foreign \"%s\" (%s @-> returning %s)@]@,"
      name orig_name params typ_name
  end

let generate_foreigns ppf commands =
  let open Core.Std in 
  Format.fprintf ppf "@[<hov 2>module Inner = struct@,";

  List.iter commands ~f:(generate_foreign ppf);

  Format.fprintf ppf "@]@[<hov 0>end@]@."

(* OCaml bindings generator *)
let generate_ocaml_binding ppf command =
  let open Core.Std in 
  let ret_typ, orig_name = command.C.proto in
  let name = Util.convert_gl_to_ocaml_name orig_name in
  match Special_defs.command_definitions orig_name with
  | Some f -> Format.fprintf ppf f
  | None -> begin
    match Capi.capi_to_ocaml_type_def ret_typ with
    | `Unknown s -> failwith s
    | `Ok typ -> begin
      let args = List.map command.C.params ~f:Tuple2.get2 |> List.map ~f:Util.convert_gl_to_ocaml_name in 
      let labeled_args = List.map args ~f:(fun arg -> "~" ^ arg) in

      Format.fprintf ppf "@[<hov 2>let %s %s = @ " name (String.concat ~sep:" " labeled_args);
      List.iter command.C.params ~f:(fun (typ, name) ->
        let name = Util.convert_gl_to_ocaml_name name in
        match Capi.capi_to_ocaml_type_def typ with
        | `Unknown s -> Printf.fprintf stderr "%s" s 
        | `Ok typ -> begin
          let module O = Capi.Ocaml_type in 
          match typ.O.ctypes with
          | `Conversion (_, wrap) -> wrap ppf name;
          | _ -> ()
        end
      );
      Format.fprintf ppf "@[Inner.%s %s@]" name (String.concat ~sep:" " args);
      Format.fprintf ppf "@]@."
    end
  end

let generate_ocaml_bindings ppf commands =
  List.iter commands ~f:(generate_ocaml_binding ppf)

let generate_ml ppf commands =
  print_packages ppf;
  print_types ppf commands;
  print_ctype_definitions ppf commands;
  generate_foreigns ppf commands;
  generate_ocaml_bindings ppf commands
