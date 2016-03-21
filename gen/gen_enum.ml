(* Define generator for enum of bindings. *)

module E = Enum

open Core.Std
module Type_map = Map.Make(String)
module Type_set = Set.Make(String)

let generate ppf enums groups =
  Type_map.iteri groups ~f:(fun ~key ~data ->
    let name = Util.normalize_name key
                               |> Fn.flip String.drop_prefix 1 
                               |> String.capitalize in 
    Format.fprintf ppf "@[<v 2>module %s = struct" name;

    Type_set.filter data ~f:(Type_map.mem enums) |>
        Type_set.iter ~f:(fun v ->
          let enum = Type_map.find_exn enums v in
          let name = String.lowercase enum.Enum.name in 
          match enum.Enum.value with
          | `Int32 v -> Format.fprintf ppf "@,@[let %s@ =@ %ld@]" name v
          | `Int64 v -> Format.fprintf ppf "@,@[let %s@ =@ %Ld@]" name v
        );

    Format.fprintf ppf "@]@,@[<v 0>end@]@."
  )
