type t = TypeVarIdGen of int

let make: t = TypeVarIdGen 0
let gen (TypeVarIdGen state: t): (Type_var_id.t * t) = (TypeVarId state, TypeVarIdGen (state + 1))
let gen_type (g: t): (Type.t * t) =
  let (tid, g) = gen g in
  (Type.TypeVar tid, g)
