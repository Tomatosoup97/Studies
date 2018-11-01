
type normal_type
  = TP_Int
  | TP_Bool
  | TP_Array of normal_type

let rec string_of_normal_type = function
  | TP_Int -> "int"
  | TP_Bool -> "bool"
  | TP_Array el -> string_of_normal_type el ^ "[]"

type extended_type = normal_type list

let string_of_extended_type xs =
    String.concat ", " @@ List.map string_of_normal_type xs

type result_type
  = RT_Unit
  | RT_Void

type env_type
  = ENVTP_Var of normal_type
  | ENVTP_Fn of extended_type * extended_type

let string_of_env_type = function
  | ENVTP_Var t -> string_of_normal_type t
  | ENVTP_Fn (xs, []) -> Format.sprintf "fn(%s)"
    (string_of_extended_type xs)
  | ENVTP_Fn (xs, rs) -> Format.sprintf "fn(%s) -> (%s)"
    (string_of_extended_type xs)
    (string_of_extended_type rs)