(**
   This program is a part of Sdlcaml package.
   This program provide to make source file that defines binding interfaces for
   OpenGL commands.
   
   @author derui
   @since 0.2
*)
open Core.Std

type param = Capi.Capi_type.t * string
type t = {
  proto: param;
  params: param list;
  alias: string option
}

let parse_param param =
  let elements = Xml.children param in
  let rec loop typ lst =
    match lst with
    | [] -> typ
    | s :: rest -> begin match s with
      | "*" -> loop (`Ptr typ) rest
      | "**" -> loop (`Ptr (`Ptr typ)) rest
      | "const" -> loop (`Const typ) rest
      | "*const*" -> loop (`Ptr (`Ptr typ)) rest
      | "void" -> loop (`Base `Void) rest
      | _ -> failwith (Printf.sprintf "unknown definition %s" s)
    end
  in
  let base_type = match Util.child param "ptype" with
    | None -> `Base `Void
    | Some e -> `Base (Capi.Base_type.of_string (Util.text_content e |> List.hd_exn))
  in
  let name = Util.child_exn param "name" |> Util.text_content |> List.hd_exn in
  let qualified =  List.fold_left elements ~f:(fun typ e ->
    match e with
    | Xml.Element (tag, _, children) -> typ
    | Xml.PCData s -> (String.strip s |> String.lstrip |> String.split ~on:' ') :: typ
  ) ~init:[]
         |> List.join
  in
  let wrapped = loop base_type qualified in
  (wrapped, name)

let xml_to_name xml =
  let proto = Util.child_exn xml "proto" in
  Util.child_exn proto "name" |> Util.text_content |> String.concat

(* parse and construct command type from <command> tag *)
let parse xml =
  (xml_to_name xml, (fun () ->
    let proto = Util.child_exn xml "proto" in
    let params = Util.children xml "param" in
    let proto = parse_param proto in
    let params = List.map params ~f:parse_param in
    {
      proto;
      params;
      alias = Util.child xml "alias" |> Option.map ~f:(fun xml -> Xml.attrib xml "name")
    }
   )
  )
