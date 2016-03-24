(* Define generator for enum of bindings. *)

module E = Enum

open Core.Std
module Type_map = Map.Make(String)
module Type_set = Set.Make(String)

let print_enum ppf enums v =
  let enum = Type_map.find_exn enums v in
  let name = String.lowercase enum.Enum.name in 
  match enum.Enum.value with
  | `Int32 v -> Format.fprintf ppf "@,@[let %s@ =@ %ldl@]" name v
  | `Int64 v -> Format.fprintf ppf "@,@[let %s@ =@ %LdL@]" name v

let generate ppf enums groups =
  let group_contained = Type_set.empty in 
  let group_contained = Type_map.fold groups ~f:(fun ~key ~data printed ->
    let name = Util.normalize_name key |> Fn.flip String.drop_prefix 1 |> String.capitalize in 
    Format.fprintf ppf "@[<v 2>module %s = struct" name;

    let contained = Type_set.filter data ~f:(Type_map.mem enums) in
    Type_set.iter contained ~f:(print_enum ppf enums);

    Format.fprintf ppf "@]@,@[<v 0>end@]@.";
    Type_set.union printed contained
  ) ~init:group_contained in
  Format.fprintf ppf "@[<v 0>";

  (* Print enums on toplevel if not contained any group. *)
  Type_map.filteri enums ~f:(fun ~key ~data -> Fn.non (Type_set.mem group_contained) key)
  |> Type_map.iteri ~f:(fun ~key ~data -> print_enum ppf enums key);

  Format.fprintf ppf "@."
