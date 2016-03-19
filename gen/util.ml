open Core.Std
  
let children elements name =
  let f = function
    | Xml.Element (tag, _, _) -> tag = name
    | _ -> false
  in
  List.filter ~f (Xml.children elements)

let child elements name =
  children elements name |> List.hd

let child_exn elements name =
  children elements name |> List.hd_exn

let text_content e =
  let rec fetch_pc_data = function
    | Xml.Element (_, _, el) -> List.map el ~f:fetch_pc_data |> List.join
    | Xml.PCData s -> [s]
  in
  Xml.map fetch_pc_data e |> List.join

let replace_reserved_word = function
  | "type" -> "typ"
  | "end" -> "end_"
  | "val" -> "val_"
  | s -> s

let normalize_name name = 
  let open Core.Std in
  String.fold name ~f:(fun accum c ->
    if Char.is_uppercase c then (Char.lowercase c) :: '_' :: accum
    else c :: accum
  ) ~init:[]
  |> List.rev
  |> String.of_char_list
  |> replace_reserved_word

let convert_gl_to_ocaml_name name =
  let open Core.Std in
  normalize_name name |> String.substr_replace_first ~pattern:"gl_" ~with_:""
