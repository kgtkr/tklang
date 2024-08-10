type t = ExprId of int

let compare (ExprId a: t) (ExprId b: t):int = Int.compare a b
