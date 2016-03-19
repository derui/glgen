(**
   This program is a part of Sdlcaml package.
   This program provide to make source file that defines binding interfaces for
   OpenGL commands.

   @author derui
   @since 0.2
*)
module List = Core.Std.List
module Map = Core.Std.Map
module String = Core.Std.String
module Fn = Core.Std.Fn
module Set = Core.Std.Set
module Option = Core.Std.Option

exception Parse_error of string

module Type_map = Map.Make(String)
module Type_set = Set.Make(String)
type t = {
  commands: Command.t Type_map.t;
  enums: Enum.t Type_map.t;
}

let filter_feature xml major minor =
  let feature_version = float_of_string (Printf.sprintf "%d.%d" major minor) in
  let features = Util.children xml "feature" in
  List.filter features ~f:(fun feature ->
    let api = Xml.attrib feature "api" in
    if api = "gl" then
      let version' = Xml.attrib feature "number" |> float_of_string in
      feature_version >= version'
    else
      false
  )

(* extract commands in the feature and merge into a large commands *)
let extract_commands map feature =
  let requirements = Util.children feature "require"
  and removings = Util.children feature "remove" in
  let required_commands = List.map ~f:(Fn.flip Util.children "command") requirements |> List.join in
  let map = List.fold_left required_commands ~f:(fun map command ->
    let name = Xml.attrib command "name" in
    Type_map.add map ~key:name ~data:command
  ) ~init:map in 

  let removed_commands = List.map ~f:(Fn.flip Util.children "command") removings |> List.join in
  List.fold_left removed_commands ~f:(fun map command ->
    let name = Xml.attrib command "name" in
    Type_map.remove map name
  ) ~init:map

let collect_required_commands features =
  let command_map = Type_map.empty in
  List.fold_left features ~f:(fun map feature ->
    extract_commands map feature
  ) ~init:command_map

(* collect commands from <command> tag in xml. *)
let parse_commands xml commands =
  let command_map = Type_map.empty in
  let command_elements = Util.child_exn xml "commands" |> Fn.flip Util.children "command" in 
  let command_map = List.fold_left command_elements ~f:(fun map data ->
    let name, command' = Command.parse data in
    Type_map.add map ~key:name ~data:command'
  ) ~init:command_map in

  Type_map.fold commands ~f:(fun ~key ~data map ->
    Type_map.add map ~key ~data:((Type_map.find_exn command_map key) ())
  ) ~init:Type_map.empty

let collect_required_enums features =
(* extract commands in the feature and merge into a large commands *)
  let extract_enums set feature =
    let requirements = Util.children feature "require"
    and removings = Util.children feature "remove" in
    let required_enums = List.map ~f:(Fn.flip Util.children "enum") requirements |> List.join 
    and removed_enums = List.map ~f:(Fn.flip Util.children "enum") removings |> List.join in

    let set = List.fold_left required_enums ~f:(fun map enum ->
      let name = Xml.attrib enum "name" in
      Type_set.add map name
    ) ~init:set in 

    List.fold_left removed_enums ~f:(fun set enum ->
      let name = Xml.attrib enum "name" in
      Type_set.remove set name
    ) ~init:set
  in 
  
  let enum_set = Type_set.empty in

  List.fold_left features ~f:(fun set feature ->
    extract_enums set feature
  ) ~init:enum_set |> Type_set.to_list

(* Get enums flitered with feature requiring *)
let filter_enums enum_map required =
  let map = Type_map.empty in
  List.fold_left required ~f:(fun map enum ->
    let open Option.Monad_infix in
    Type_map.find enum_map enum |>
        Option.value_map ~f:(fun data -> Type_map.add map ~key:enum ~data) ~default:map
  ) ~init:map

let parse ~xml ~major ~minor = 
  let features = filter_feature xml major minor in
  let commands = collect_required_commands features |> parse_commands xml in
  let enums = Enum.parse xml in 
  let enums = collect_required_enums features |> filter_enums enums in
  {
    commands;
    enums;
  }
