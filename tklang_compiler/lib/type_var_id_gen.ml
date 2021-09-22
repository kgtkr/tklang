type t = int

let make: t = 0
let next (gen: t): (Type_var_id.t * t) = (gen, gen + 1)
