type t = ExprIdGen of int

let make: t = ExprIdGen 0
let gen (ExprIdGen state: t): (Expr_id.t * t) = (ExprId state, ExprIdGen (state + 1))
