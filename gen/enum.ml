(**
   This program is a part of Sdlcaml package.
   This program provide to make source file that defines modules for OCaml.

   @author derui
   @since 0.2
*)
open Core.Std

module Type_map = Map.Make(String)
type t = {
  name: string;
  value: [`Int64 of int64 | `Int32 of int32]
}

let children = Util.children

let parse xml =
  let enums = children xml "enums"
         |> List.map ~f:(Fn.flip children "enum")
         |> List.join in 
  let tbl = Type_map.empty in 
  List.fold_left enums ~f:(fun map e ->
    let name = Xml.attrib e "name"
    and value = Xml.attrib e "value"
    and typ = Xml.attribs e |> List.filter ~f:(fun (name, v) -> name = "type")
        |> (function
          | [] | [(_, "u")] -> `Int32
          | [(_, "ull")] -> `Int64
          | _ -> failwith "Unknown enum element format"
        ) in 
    let data = {
      name;
      value = match typ with
      | `Int32 -> `Int32 Int32.(of_string value)
      | `Int64 -> `Int64 Int64.(of_string value);
    } in 
    Type_map.add map ~key:name ~data
  ) ~init:tbl
