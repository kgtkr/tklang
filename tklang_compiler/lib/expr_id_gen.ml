type t = int

let make: t = 0
let next (gen: t): (Expr_id.t * t) = (gen, gen + 1)
