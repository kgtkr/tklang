type t = TypeVarId of int

let compare (TypeVarId a: t) (TypeVarId b: t):int = Int.compare a b

let to_string (TypeVarId x: t):string = "'" ^ Int.to_string x
