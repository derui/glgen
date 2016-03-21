open Core.Std

type gen_type = Mli | Ml | Enum

let major = ref 0
let minor = ref 0
let xml_file = ref ""
let gen_type : gen_type option ref = ref None

let specs = [
  ("-major", Arg.Int (fun v -> major := v), "major version of OpenGL");
  ("-minor", Arg.Int (fun v -> minor := v), "minor version of OpenGL");
  ("-type", Arg.Symbol (["mli";"ml";"enum"], function
  | "mli" -> gen_type := Some Mli
  | "ml" -> gen_type := Some Ml
  | "enum" -> gen_type := Some Enum
  | s -> failwith (Printf.sprintf "Unknown paramter for -type : %s" s)
   ), "Type to generate binding source");
]

let () =
  Arg.parse specs (fun s -> xml_file := s) "Usage: gl_gen -type <classify generating binding> -major <major version of OpenGL> -minor <minor version of OpenGL> <xml as OpenGL Registry>";

  try
    let xml = Xml.parse_file !xml_file in
    let parsed = Glreg_parser.parse ~xml ~major:!major ~minor:!minor in
    let commands = Glreg_parser.Type_map.data parsed.Glreg_parser.commands
    and enums = parsed.Glreg_parser.enums
    and groups = parsed.Glreg_parser.groups
    in 
    match !gen_type with
    | None -> failwith "No target type specifies to generate"
    | Some Mli -> Gen_mli.generate_mli Format.std_formatter commands
    | Some Ml -> Gen_ml.generate_ml Format.std_formatter commands
    | Some Enum -> Gen_enum.generate Format.std_formatter enums groups
  with Failure s -> Printf.fprintf stderr "%s%s" (Exn.backtrace ()) s; exit 1
