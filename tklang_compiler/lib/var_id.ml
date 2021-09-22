type t = VarId of int

let compare (VarId a: t) (VarId b: t): int = Int.compare a b

let to_string (VarId x: t): string = "{" ^ Int.to_string x ^ "}"

let to_string_with_type_scheme (VarId x: t) (ts: Type_scheme.t): string = "{" ^ Int.to_string x ^ ": " ^ Type_scheme.to_string ts  ^ "}"
