(**
   This program is a part of Ogl package.
   This program provide to make source file that defines modules for OCaml.

   @author derui
   @since 0.2
*)
open Core.Std

module Type_map = Map.Make(String)
module Type_set = Set.Make(String)
type t = Type_set.t Type_map.t

let children = Util.children

(* Return parsed groups that has enums each groups *)
let parse xml =
  let groups = Util.child_exn xml "groups"
         |> Fn.flip Util.children "group"
  in
  let tbl = Type_map.empty in
  List.fold_left groups ~f:(fun map e ->
    let name = Xml.attrib e "name" in 
    let set = Type_set.empty in
    let set = children e "enum" |> List.fold_left ~f:(fun s e -> 
      let name = Xml.attrib e "name" in
      Type_set.add s name
    ) ~init: set in
    Type_map.add map ~key:name ~data:set
  ) ~init:tbl
